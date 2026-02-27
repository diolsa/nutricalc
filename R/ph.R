#' Compute pH of a nutrient solution from achieved composition
#'
#' Uses charge balance with acid/base speciation and activity corrections
#' to estimate pH at 25 C. Assumes no carbonate alkalinity, no complexation, and
#' no precipitation.
#'
#' gamma_model:
#'   1 = Davies (default)
#'   2 = Debye–Hückel limiting law (DH)
#'
#' @param achieved Named numeric vector of mmol/L (e.g., solver result `x$achieved`).
#'   If present, `CO2_aq` is treated as dissolved free CO2(aq) (mmol/L), mapped
#'   from BWB "Basenkapazitaet KB 8,2" / KS 8.2 and used to fix carbonate speciation.
#' @param temp_C Temperature in Celsius (only 25 C supported in this version).
#' @param phc_bracket Numeric length-2 bracket for pHc = -log10([H+]) search.
#' @param tol Root tolerance in meq/L.
#' @param max_iter Maximum iterations for the outer bisection.
#' @param inner_max_iter Maximum iterations for the ionic strength fixed point.
#' @param debug Logical; when TRUE prints basic bracket diagnostics.
#' @param chelate_z Named numeric vector of chelate charges to include as fixed
#'   anions. Values are net charges (negative) for ligand totals in mmol/L.
#' @param gamma_model Activity model selector: 1/"davies_25C" (Davies) or 2/"debye_huckel_25C" (Debye–Hückel). Default 1.
#'
#' @return A list with pH, ionic strength, activity coefficients, species
#'   distribution, and charge balance diagnostics.
#' @export
ph_from_achieved <- function(
    achieved,
    temp_C = 25,
    phc_bracket = c(3, 9),
    tol = 1e-9,
    max_iter = 200,
    inner_max_iter = 50,
    debug = FALSE,
    chelate_z = c(EDTA = -4, DTPA = -5, EDDHA = -4, HBED = -4),
    gamma_model = 1
) {
  if (!is.numeric(inner_max_iter) || length(inner_max_iter) != 1 || is.na(inner_max_iter) ||
      inner_max_iter < 1 || inner_max_iter %% 1 != 0) {
    stop("inner_max_iter must be a single numeric value >= 1.", call. = FALSE)
  }

  achieved <- to_named_numeric(achieved)
  stopifnot(is.numeric(achieved), !is.null(names(achieved)))
  if (temp_C != 25) stop("This simple implementation currently assumes 25C")

  # ---- normalize gamma_model: allow 1/2 or helpful strings ----
  gm <- tolower(as.character(gamma_model[1]))
  if (gm %in% c("1", "davies", "davies_25c")) gm <- "davies_25c"
  if (gm %in% c("2", "dh", "debye", "debye_huckel", "debye-huckel", "debye_huckel_25c")) gm <- "debye_huckel_25c"
  if (!gm %in% c("davies_25c", "debye_huckel_25c")) {
    stop("gamma_model must be one of 1/'davies_25C' or 2/'debye_huckel_25C'.", call. = FALSE)
  }
  gamma_model <- if (identical(gm, "davies_25c")) "davies_25C" else "debye_huckel_25C"
  gamma_model_id <- if (identical(gamma_model, "davies_25C")) 1L else 2L

  # --- constants (25 C) ---
  A_25 <- 0.5085
  Kw <- 1e-14

  # acid constants (thermodynamic, for activities)
  Ka_NH4  <- 10^(-9.25)
  Ka_HSO4 <- 10^(-1.99)

  Ka1_P <- 10^(-2.16)
  Ka2_P <- 10^(-7.21)
  Ka3_P <- 10^(-12.32)
  Ka1_C <- 10^(-6.35)
  Ka2_C <- 10^(-10.33)
  Ka_B  <- 10^(-9.27)
  Ka_Si1 <- 10^(-9.90)

  # --- helpers: activity coefficient models ---
  gamma_davies_25C <- function(I, z, A = A_25) {
    if (!is.finite(I) || I <= 0) return(1.0)
    term <- (sqrt(I) / (1 + sqrt(I)) - 0.3 * I)
    val <- 10^(-A * z^2 * term)
    if (!is.finite(val) || val <= 0) return(.Machine$double.xmin)
    val
  }

  gamma_dh_25C <- function(I, z, A = A_25) {
    if (!is.finite(I) || I <= 0) return(1.0)
    val <- 10^(-A * z^2 * sqrt(I))
    if (!is.finite(val) || val <= 0) return(.Machine$double.xmin)
    val
  }

  gamma_fun <- if (identical(gamma_model, "davies_25C")) gamma_davies_25C else gamma_dh_25C

  gamma_z <- function(I, z) {
    gamma_fun(I, z)
  }

  # pull inputs (mmol/L)
  get0 <- function(nm) if (nm %in% names(achieved)) achieved[[nm]] else 0
  get_any0 <- function(keys) {
    for (key in keys) {
      if (key %in% names(achieved)) return(achieved[[key]])
    }
    0
  }

  NO3 <- get0("NO3_N")
  NT  <- get0("NH4_N")
  PT  <- get0("P")
  KT  <- get0("K")
  Ca  <- get0("Ca")
  Mg  <- get0("Mg")
  ST  <- get0("S")
  BT  <- get0("B")
  SiT <- get0("Si")

  Na <- get0("Na")
  Cl <- get0("Cl")

  Fe <- get0("Fe")
  Mn <- get0("Mn")
  Zn <- get0("Zn")
  Cu <- get0("Cu")
  Mo <- get0("Mo")

  EDTA  <- get_any0(c("EDTA"))
  DTPA  <- get_any0(c("DTPA"))
  EDDHA <- get_any0(c("EDDHA"))
  HBED  <- get_any0(c("HBED"))

  as_scalar_finite0 <- function(x) {
    if (is.null(x) || !length(x)) return(0)
    x <- suppressWarnings(as.numeric(x[1]))
    if (!is.finite(x)) return(0)
    x
  }

  CT_mM <- if ("KS4_3" %in% names(achieved)) {
    achieved[["KS4_3"]]
  } else if ("Alkalinity" %in% names(achieved)) {
    achieved[["Alkalinity"]]
  } else if ("HCO3" %in% names(achieved)) {
    achieved[["HCO3"]]
  } else {
    0
  }
  CT_mM <- max(0, as_scalar_finite0(CT_mM))

  CO2_aq_mM <- if ("CO2_aq" %in% names(achieved)) {
    achieved[["CO2_aq"]]
  } else if ("KB8_2" %in% names(achieved)) {
    achieved[["KB8_2"]]
  } else if ("KS8_2" %in% names(achieved)) {
    achieved[["KS8_2"]]
  } else {
    0
  }
  CO2_aq_mM <- max(0, as_scalar_finite0(CO2_aq_mM))

  # convert mmol/L -> mol/L where needed
  mM_to_M <- function(x_mM) x_mM * 1e-3

  # fixed ions for ionic strength
  fixed_c <- list(
    K  = c(c = mM_to_M(KT), z = 1),
    Na = c(c = mM_to_M(Na), z = 1),
    Ca = c(c = mM_to_M(Ca), z = 2),
    Mg = c(c = mM_to_M(Mg), z = 2),
    Fe = c(c = mM_to_M(Fe), z = 3),
    Mn = c(c = mM_to_M(Mn), z = 2),
    Zn = c(c = mM_to_M(Zn), z = 2),
    Cu = c(c = mM_to_M(Cu), z = 2),

    NO3 = c(c = mM_to_M(NO3), z = -1),
    Cl  = c(c = mM_to_M(Cl), z = -1),
    MoO4 = c(c = mM_to_M(Mo), z = -2)
  )

  chelate_conc <- c(EDTA = EDTA, DTPA = DTPA, EDDHA = EDDHA, HBED = HBED)
  chelate_conc <- chelate_conc[names(chelate_conc) %in% names(chelate_z)]
  chelate_conc <- chelate_conc[is.finite(chelate_conc) & chelate_conc > 0]
  if (length(chelate_conc)) {
    for (nm in names(chelate_conc)) {
      fixed_c[[nm]] <- c(c = mM_to_M(chelate_conc[[nm]]), z = chelate_z[[nm]])
    }
  }

  # charge balance residual f(phc): positive - negative (meq/L)
  residual_meq <- function(phc) {
    H <- 10^(-phc) # mol/L

    # inner fixed-point loop on ionic strength
    I <- 0.03
    gam <- list()
    species <- list()

    for (k in seq_len(inner_max_iter)) {
      # gammas by charge state (selected model)
      gam$z1 <- gamma_z(I, 1)
      gam$z2 <- gamma_z(I, 2)
      gam$z3 <- gamma_z(I, 3)

      gam$H      <- gam$z1
      gam$OH     <- gam$z1
      gam$NH4    <- gam$z1
      gam$HSO4   <- gam$z1
      gam$SO4    <- gam$z2
      gam$H2PO4  <- gam$z1
      gam$HPO4   <- gam$z2
      gam$PO4    <- gam$z3
      gam$HCO3   <- gam$z1
      gam$CO3    <- gam$z2
      gam$BOH4   <- gam$z1
      gam$H3SiO4 <- gam$z1

      # OH- from water autoprotolysis with activities
      OH <- Kw / (gam$H * gam$OH * H)

      # NH4/NH3 (NH3 neutral)
      rN <- Ka_NH4 * gam$NH4 / (gam$H * H)
      NH4 <- mM_to_M(NT) / (1 + rN)
      NH3 <- mM_to_M(NT) - NH4

      # sulfate
      rS <- Ka_HSO4 * gam$HSO4 / (gam$H * gam$SO4 * H)
      HSO4 <- mM_to_M(ST) / (1 + rS)
      SO4 <- mM_to_M(ST) - HSO4

      # phosphate with conditional Ka including gammas
      K1c <- Ka1_P / (gam$H * gam$H2PO4)
      K2c <- Ka2_P * gam$H2PO4 / (gam$H * gam$HPO4)
      K3c <- Ka3_P * gam$HPO4 / (gam$H * gam$PO4)

      D <- H^3 + K1c * H^2 + K1c * K2c * H + K1c * K2c * K3c
      H3PO4 <- mM_to_M(PT) * (H^3 / D)
      H2PO4 <- mM_to_M(PT) * (K1c * H^2 / D)
      HPO4  <- mM_to_M(PT) * (K1c * K2c * H / D)
      PO4   <- mM_to_M(PT) * (K1c * K2c * K3c / D)

      # carbonate
      K1c_C <- Ka1_C / (gam$H * gam$HCO3)
      K2c_C <- Ka2_C * gam$HCO3 / (gam$H * gam$CO3)

      if (isTRUE(CO2_aq_mM > 0)) {
        CO2 <- mM_to_M(CO2_aq_mM)
        HCO3 <- (K1c_C * CO2) / H
        CO3  <- (K2c_C * HCO3) / H
      } else if (isTRUE(CT_mM > 0)) {
        D_c <- H^2 + K1c_C * H + K1c_C * K2c_C
        a0 <- H^2 / D_c
        a1 <- (K1c_C * H) / D_c
        a2 <- (K1c_C * K2c_C) / D_c
        CT <- mM_to_M(CT_mM)
        CO2  <- a0 * CT
        HCO3 <- a1 * CT
        CO3  <- a2 * CT
      } else {
        CO2 <- 0; HCO3 <- 0; CO3 <- 0
      }

      # borate
      aH <- gam$H * H
      rB <- (Ka_B / aH) * (1 / gam$BOH4)
      BOH4 <- mM_to_M(BT) * (rB / (1 + rB))
      BOH3 <- mM_to_M(BT) - BOH4

      # silicate
      rSi <- (Ka_Si1 / aH) * (1 / gam$H3SiO4)
      H3SiO4 <- mM_to_M(SiT) * (rSi / (1 + rSi))
      H4SiO4 <- mM_to_M(SiT) - H3SiO4

      # ionic strength recompute
      I_new <- 0
      for (nm in names(fixed_c)) {
        I_new <- I_new + fixed_c[[nm]]["c"] * (fixed_c[[nm]]["z"]^2)
      }
      I_new <- I_new +
        H * 1^2 +
        OH * 1^2 +
        NH4 * 1^2 +
        HSO4 * 1^2 +
        SO4 * 2^2 +
        H2PO4 * 1^2 +
        HPO4 * 2^2 +
        PO4 * 3^2 +
        HCO3 * 1^2 +
        CO3 * 2^2 +
        BOH4 * 1^2 +
        H3SiO4 * 1^2
      I_new <- 0.5 * I_new

      species <- list(
        H = H, OH = OH,
        NH4 = NH4, NH3 = NH3,
        HSO4 = HSO4, SO4 = SO4,
        H3PO4 = H3PO4, H2PO4 = H2PO4, HPO4 = HPO4, PO4 = PO4,
        CO2 = CO2, HCO3 = HCO3, CO3 = CO3,
        BOH3 = BOH3, BOH4 = BOH4,
        H4SiO4 = H4SiO4, H3SiO4 = H3SiO4
      )

      if (isTRUE(abs(I_new - I) < 1e-10)) {
        I <- I_new
        break
      }
      I <- I_new
    }

    expected_species_full <- c(
      "H", "OH", "NH4", "NH3", "HSO4", "SO4", "H3PO4", "H2PO4", "HPO4", "PO4",
      "CO2", "HCO3", "CO3", "BOH3", "BOH4", "H4SiO4", "H3SiO4"
    )
    missing_species <- setdiff(expected_species_full, names(species))
    if (length(missing_species) > 0) {
      stop(
        sprintf(
          "Speciation failed: missing species [%s]. Present: [%s].",
          paste(missing_species, collapse = ", "),
          paste(names(species), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    non_finite <- vapply(species, function(x) !is.finite(x), logical(1))
    if (any(non_finite)) {
      return(list(
        f = NA_real_,
        I = I,
        gam = gam,
        species = species,
        H_mM = NA_real_,
        OH_mM = NA_real_,
        pos_fix_meq = NA_real_,
        neg_fix_meq = NA_real_,
        pos = NA_real_,
        neg = NA_real_
      ))
    }

    # ---- charge balance in meq/L ----
    pos_fix_meq <- KT + Na + 2 * Ca + 2 * Mg + 3 * Fe + 2 * Mn + 2 * Zn + 2 * Cu
    neg_fix_meq <- NO3 + Cl + 2 * Mo
    if (length(chelate_conc)) {
      for (nm in names(chelate_conc)) {
        neg_fix_meq <- neg_fix_meq + abs(chelate_z[[nm]]) * chelate_conc[[nm]]
      }
    }

    H_mM <- species$H * 1e3
    OH_mM <- species$OH * 1e3
    NH4_mM <- species$NH4 * 1e3
    HSO4_mM <- species$HSO4 * 1e3
    SO4_mM <- species$SO4 * 1e3
    H2PO4_mM <- species$H2PO4 * 1e3
    HPO4_mM <- species$HPO4 * 1e3
    PO4_mM <- species$PO4 * 1e3
    HCO3_mM <- species$HCO3 * 1e3
    CO3_mM <- species$CO3 * 1e3
    BOH4_mM <- species$BOH4 * 1e3
    H3SiO4_mM <- species$H3SiO4 * 1e3

    pos <- pos_fix_meq + H_mM + NH4_mM
    neg <- neg_fix_meq + OH_mM +
      (HSO4_mM + 2 * SO4_mM) +
      (H2PO4_mM + 2 * HPO4_mM + 3 * PO4_mM) +
      (HCO3_mM + 2 * CO3_mM) +
      BOH4_mM +
      H3SiO4_mM

    list(
      f = pos - neg,
      I = I,
      gam = gam,
      species = species,
      H_mM = H_mM,
      OH_mM = OH_mM,
      pos_fix_meq = pos_fix_meq,
      neg_fix_meq = neg_fix_meq,
      pos = pos,
      neg = neg
    )
  }

  # --- outer root find (bisection on pHc) ---
  find_bracket <- function(a_in, b_in) {
    fa_in <- residual_meq(a_in)$f
    fb_in <- residual_meq(b_in)$f
    if (is.finite(fa_in) && is.finite(fb_in) && fa_in * fb_in <= 0) {
      return(list(a = a_in, b = b_in, fa = fa_in, fb = fb_in))
    }

    phc_grid <- seq(-2, 16, by = 0.5)
    f_grid <- vapply(phc_grid, function(x) {
      tryCatch(residual_meq(x)$f, error = function(e) NA_real_)
    }, numeric(1))
    finite_idx <- which(is.finite(f_grid))
    if (length(finite_idx) >= 2) {
      phc_f <- phc_grid[finite_idx]
      f_f <- f_grid[finite_idx]
      signs <- sign(f_f)
      idx <- which(signs[-1] * signs[-length(signs)] <= 0)
      if (length(idx)) {
        i <- idx[1]
        return(list(
          a = phc_f[i],
          b = phc_f[i + 1],
          fa = f_f[i],
          fb = f_f[i + 1]
        ))
      }
    }

    if (isTRUE(debug)) {
      r2 <- residual_meq(2)
      r7 <- residual_meq(7)
      r12 <- residual_meq(12)
      message(sprintf(
        paste(
          "pH bracket debug phc=2: f=%.6g pos=%.6g neg=%.6g H=%.6g mM OH=%.6g mM CT_mM=%.6g HCO3_mM=%.6g CO3_mM=%.6g BOH4_mM=%.6g H3SiO4_mM=%.6g",
          "pH bracket debug phc=7: f=%.6g pos=%.6g neg=%.6g H=%.6g mM OH=%.6g mM CT_mM=%.6g HCO3_mM=%.6g CO3_mM=%.6g BOH4_mM=%.6g H3SiO4_mM=%.6g",
          "pH bracket debug phc=12: f=%.6g pos=%.6g neg=%.6g H=%.6g mM OH=%.6g mM CT_mM=%.6g HCO3_mM=%.6g CO3_mM=%.6g BOH4_mM=%.6g H3SiO4_mM=%.6g",
          sep = "\n"
        ),
        r2$f, r2$pos, r2$neg, r2$H_mM, r2$OH_mM, CT_mM, r2$species[["HCO3"]] * 1e3, r2$species[["CO3"]] * 1e3,
        r2$species[["BOH4"]] * 1e3, r2$species[["H3SiO4"]] * 1e3,
        r7$f, r7$pos, r7$neg, r7$H_mM, r7$OH_mM, CT_mM, r7$species[["HCO3"]] * 1e3, r7$species[["CO3"]] * 1e3,
        r7$species[["BOH4"]] * 1e3, r7$species[["H3SiO4"]] * 1e3,
        r12$f, r12$pos, r12$neg, r12$H_mM, r12$OH_mM, CT_mM, r12$species[["HCO3"]] * 1e3, r12$species[["CO3"]] * 1e3,
        r12$species[["BOH4"]] * 1e3, r12$species[["H3SiO4"]] * 1e3
      ))
    }

    stop(
      sprintf(
        "Root not bracketed; f(phc) has no sign change. min=%.6g max=%.6g",
        min(f_grid, na.rm = TRUE),
        max(f_grid, na.rm = TRUE)
      ),
      call. = FALSE
    )
  }

  if (isTRUE(debug)) {
    chelate_meq_total <- 0
    chelate_conc_dbg <- c(EDTA = EDTA, DTPA = DTPA, EDDHA = EDDHA, HBED = HBED)
    chelate_conc_dbg <- chelate_conc_dbg[names(chelate_conc_dbg) %in% names(chelate_z)]
    chelate_conc_dbg <- chelate_conc_dbg[is.finite(chelate_conc_dbg) & chelate_conc_dbg > 0]
    if (length(chelate_conc_dbg)) {
      for (nm in names(chelate_conc_dbg)) {
        chelate_meq_total <- chelate_meq_total + abs(chelate_z[[nm]]) * chelate_conc_dbg[[nm]]
      }
    }
    r0 <- residual_meq(0)
    r7 <- residual_meq(7)
    r14 <- residual_meq(14)
    message(sprintf(
      paste(
        "pH debug phc=0: H=%.6g mM OH=%.6g mM pos_fix=%.6g neg_fix=%.6g chelate_meq=%.6g pos=%.6g neg=%.6g f=%.6g CT_mM=%.6g HCO3_mM=%.6g CO3_mM=%.6g BOH4_mM=%.6g H3SiO4_mM=%.6g",
        "pH debug phc=7: H=%.6g mM OH=%.6g mM pos_fix=%.6g neg_fix=%.6g chelate_meq=%.6g pos=%.6g neg=%.6g f=%.6g CT_mM=%.6g HCO3_mM=%.6g CO3_mM=%.6g BOH4_mM=%.6g H3SiO4_mM=%.6g",
        "pH debug phc=14: H=%.6g mM OH=%.6g mM pos_fix=%.6g neg_fix=%.6g chelate_meq=%.6g pos=%.6g neg=%.6g f=%.6g CT_mM=%.6g HCO3_mM=%.6g CO3_mM=%.6g BOH4_mM=%.6g H3SiO4_mM=%.6g",
        sep = "\n"
      ),
      r0$H_mM, r0$OH_mM, r0$pos_fix_meq, r0$neg_fix_meq, chelate_meq_total, r0$pos, r0$neg, r0$f,
      CT_mM, r0$species[["HCO3"]] * 1e3, r0$species[["CO3"]] * 1e3,
      r0$species[["BOH4"]] * 1e3, r0$species[["H3SiO4"]] * 1e3,
      r7$H_mM, r7$OH_mM, r7$pos_fix_meq, r7$neg_fix_meq, chelate_meq_total, r7$pos, r7$neg, r7$f,
      CT_mM, r7$species[["HCO3"]] * 1e3, r7$species[["CO3"]] * 1e3,
      r7$species[["BOH4"]] * 1e3, r7$species[["H3SiO4"]] * 1e3,
      r14$H_mM, r14$OH_mM, r14$pos_fix_meq, r14$neg_fix_meq, chelate_meq_total, r14$pos, r14$neg, r14$f,
      CT_mM, r14$species[["HCO3"]] * 1e3, r14$species[["CO3"]] * 1e3,
      r14$species[["BOH4"]] * 1e3, r14$species[["H3SiO4"]] * 1e3
    ))
  }

  bracket <- find_bracket(phc_bracket[1], phc_bracket[2])
  a <- bracket$a
  b <- bracket$b
  fa <- bracket$fa
  fb <- bracket$fb

  mid <- NA_real_
  out <- NULL
  for (it in seq_len(max_iter)) {
    mid <- 0.5 * (a + b)
    out <- residual_meq(mid)
    fm <- out$f
    if (!is.finite(fm)) {
      b <- mid
      fb <- fm
      next
    }
    if (abs(fm) < tol) break
    if (fa * fm <= 0) {
      b <- mid
      fb <- fm
    } else {
      a <- mid
      fa <- fm
    }
  }

  phc <- mid
  H <- 10^(-phc)
  gamma_H <- out$gam$H
  pH <- -log10(gamma_H * H)

  # unpack species in mM
  sp <- out$species
  species_mM <- c(
    H = as.numeric(sp$H) * 1e3,
    OH = as.numeric(sp$OH) * 1e3,
    NH4 = as.numeric(sp$NH4) * 1e3,
    NH3 = as.numeric(sp$NH3) * 1e3,
    HSO4 = as.numeric(sp$HSO4) * 1e3,
    SO4 = as.numeric(sp$SO4) * 1e3,
    H3PO4 = as.numeric(sp$H3PO4) * 1e3,
    H2PO4 = as.numeric(sp$H2PO4) * 1e3,
    HPO4 = as.numeric(sp$HPO4) * 1e3,
    PO4 = as.numeric(sp$PO4) * 1e3,
    CO2 = as.numeric(sp$CO2) * 1e3,
    HCO3 = as.numeric(sp$HCO3) * 1e3,
    CO3 = as.numeric(sp$CO3) * 1e3,
    BOH3 = as.numeric(sp$BOH3) * 1e3,
    BOH4 = as.numeric(sp$BOH4) * 1e3,
    H4SiO4 = as.numeric(sp$H4SiO4) * 1e3,
    H3SiO4 = as.numeric(sp$H3SiO4) * 1e3
  )

  expected_species <- c(
    "H", "OH", "NH4", "HSO4", "SO4", "H2PO4", "HPO4", "PO4",
    "CO2", "HCO3", "CO3", "BOH3", "BOH4", "H4SiO4", "H3SiO4"
  )
  missing <- setdiff(expected_species, names(species_mM))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "species_mM missing expected names [%s]. Present: [%s].",
        paste(missing, collapse = ", "),
        paste(names(species_mM), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  fixed_cations_meq <- c(
    K = KT,
    Na = Na,
    `2Ca` = 2 * Ca,
    `2Mg` = 2 * Mg,
    `3Fe` = 3 * Fe,
    `2Mn` = 2 * Mn,
    `2Zn` = 2 * Zn,
    `2Cu` = 2 * Cu
  )
  fixed_anions_meq <- c(
    NO3 = NO3,
    Cl = Cl,
    `2Mo` = 2 * Mo
  )
  if (length(chelate_conc)) {
    for (nm in names(chelate_conc)) {
      fixed_anions_meq[[nm]] <- abs(chelate_z[[nm]]) * chelate_conc[[nm]]
    }
  }

  variable_meq <- c(
    H_plus = species_mM[["H"]],
    OH_minus = species_mM[["OH"]],
    NH4_plus = species_mM[["NH4"]],
    sulfate_total_meq = species_mM[["HSO4"]] + 2 * species_mM[["SO4"]],
    phosphate_total_meq = species_mM[["H2PO4"]] + 2 * species_mM[["HPO4"]] + 3 * species_mM[["PO4"]],
    carbonate_total_meq = species_mM[["HCO3"]] + 2 * species_mM[["CO3"]],
    borate_total_meq = species_mM[["BOH4"]],
    silicate_total_meq = species_mM[["H3SiO4"]]
  )

  pos_meq <- sum(fixed_cations_meq) + variable_meq[["H_plus"]] + variable_meq[["NH4_plus"]]
  neg_meq <- sum(fixed_anions_meq) + variable_meq[["OH_minus"]] +
    variable_meq[["sulfate_total_meq"]] + variable_meq[["phosphate_total_meq"]] +
    variable_meq[["carbonate_total_meq"]] +
    variable_meq[["borate_total_meq"]] +
    variable_meq[["silicate_total_meq"]]

  charge_breakdown <- list(
    fixed_cations_meq = fixed_cations_meq,
    fixed_anions_meq = fixed_anions_meq,
    variable_meq = variable_meq,
    species_mM = species_mM,
    totals_meq = c(
      pos_total_meq = as.numeric(pos_meq),
      neg_total_meq = as.numeric(neg_meq),
      residual_meq = as.numeric(pos_meq - neg_meq)
    )
  )

  gamma_ions <- c(
    "H+" = out$gam$z1,
    "OH-" = out$gam$z1,
    "K+" = out$gam$z1,
    "Na+" = out$gam$z1,
    "NH4+" = out$gam$z1,
    "Ca2+" = out$gam$z2,
    "Mg2+" = out$gam$z2,
    "Fe2+" = out$gam$z2,
    "Fe3+" = out$gam$z3,
    "Mn2+" = out$gam$z2,
    "Zn2+" = out$gam$z2,
    "Cu2+" = out$gam$z2,
    "Cl-" = out$gam$z1,
    "NO3-" = out$gam$z1,
    "HCO3-" = out$gam$z1,
    "CO3-2" = out$gam$z2,
    "HSO4-" = out$gam$z1,
    "SO4-2" = out$gam$z2,
    "H2PO4-" = out$gam$z1,
    "HPO4-2" = out$gam$z2,
    "PO4-3" = out$gam$z3,
    "MoO4-2" = out$gam$z2
  )

  list(
    pH = as.numeric(pH),
    pHc = as.numeric(phc),
    H = as.numeric(H),
    I = as.numeric(out$I),
    gammas = out$gam,
    gamma_ions = gamma_ions,
    gamma_model = gamma_model,
    gamma_model_id = gamma_model_id,
    gamma_model_name = gamma_model,
    species_mM = species_mM,
    charge_meq = c(
      pos = as.numeric(pos_meq),
      neg = as.numeric(neg_meq),
      residual = as.numeric(pos_meq - neg_meq)
    ),
    charge_breakdown = charge_breakdown
  )
}
