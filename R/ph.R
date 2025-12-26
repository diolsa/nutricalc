#' Compute pH of a nutrient solution from achieved composition
#'
#' Uses charge balance with acid/base speciation and Davies activity corrections
#' to estimate pH at 25 C. Assumes no carbonate alkalinity, no complexation, and
#' no precipitation.
#'
#' @param achieved Named numeric vector of mmol/L (e.g., solver result `x$achieved`).
#' @param temp_C Temperature in Celsius (only 25 C supported in this version).
#' @param phc_bracket Numeric length-2 bracket for pHc = -log10([H+]) search.
#' @param tol Root tolerance in meq/L.
#' @param max_iter Maximum iterations for the outer bisection.
#' @param inner_max_iter Maximum iterations for the ionic strength fixed point.
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
  inner_max_iter = 50
) {
  normalize_nutrient_names <- function(x) {
    x <- gsub("[–-]", "_", x)
    x <- gsub("\\s+", "", x)
    x
  }

  to_named_numeric <- function(x) {
    if (is.data.frame(x)) {
      if (!all(c("Nutrient", "Achieved") %in% names(x))) {
        stop("Data frame must have columns 'Nutrient' and 'Achieved'.", call. = FALSE)
      }
      nm <- as.character(x$Nutrient)
      vals <- x$Achieved
      names(vals) <- nm
      x <- vals
    }

    if (is.null(names(x))) {
      stop("achieved must be a named numeric vector or a data.frame with Nutrient/Achieved columns.", call. = FALSE)
    }

    vals <- x
    if (is.character(vals)) {
      vals[vals == ""] <- NA
      vals <- suppressWarnings(as.numeric(vals))
    }

    if (!is.numeric(vals)) {
      vals <- suppressWarnings(as.numeric(vals))
    }

    names(vals) <- normalize_nutrient_names(names(vals))

    na_idx <- is.na(vals)
    if (any(na_idx)) {
      bad <- names(vals)[na_idx]
      bad <- unique(bad[!is.na(bad) & nzchar(bad)])
      stop(sprintf("achieved has NA values for: %s", paste(bad, collapse = ", ")), call. = FALSE)
    }

    vals
  }

  if (!is.numeric(inner_max_iter) || length(inner_max_iter) != 1 || is.na(inner_max_iter) ||
      inner_max_iter < 1 || inner_max_iter %% 1 != 0) {
    stop("inner_max_iter must be a single numeric value >= 1.", call. = FALSE)
  }

  achieved <- to_named_numeric(achieved)
  stopifnot(is.numeric(achieved), !is.null(names(achieved)))
  if (temp_C != 25) stop("This simple implementation currently assumes 25C")

  # --- constants (25 C) ---
  A_davies <- 0.509
  Kw <- 1e-14

  # acid constants (thermodynamic, for activities)
  Ka_NH4 <- 10^(-9.25)
  Ka_HSO4 <- 10^(-1.92)

  Ka1_P <- 10^(-2.15)
  Ka2_P <- 10^(-7.20)
  Ka3_P <- 10^(-12.35)

  # --- helpers ---
  davies_gamma <- function(I, z) {
    if (I <= 0) return(1.0)
    term <- (sqrt(I) / (1 + sqrt(I)) - 0.3 * I)
    10^(-A_davies * z^2 * term)
  }

  # pull inputs (mmol/L)
  get0 <- function(nm) if (nm %in% names(achieved)) achieved[[nm]] else 0
  NO3 <- get0("NO3_N")        # mmol/L NO3- (NO3-N is 1:1)
  NT <- get0("NH4_N")         # mmol/L total ammonia (NH4+/NH3)
  PT <- get0("P")             # mmol/L total phosphate
  KT <- get0("K")             # mmol/L K+
  Ca <- get0("Ca")            # mmol/L Ca2+
  Mg <- get0("Mg")            # mmol/L Mg2+
  ST <- get0("S")             # mmol/L total sulfate (HSO4-/SO4^2-)

  Na <- get0("Na")
  Cl <- get0("Cl")

  Fe <- get0("Fe")
  Mn <- get0("Mn")
  Zn <- get0("Zn")
  Cu <- get0("Cu")
  Mo <- get0("Mo")
  # B is treated neutral here
  # Si ignored

  # convert mmol/L -> mol/L where needed
  mM_to_M <- function(x_mM) x_mM * 1e-3

  # fixed ions list for ionic strength calc (we'll update variable ions each time)
  fixed_c <- list(
    K = c(c = mM_to_M(KT), z = 1),
    Na = c(c = mM_to_M(Na), z = 1),
    Ca = c(c = mM_to_M(Ca), z = 2),
    Mg = c(c = mM_to_M(Mg), z = 2),
    Fe = c(c = mM_to_M(Fe), z = 2),
    Mn = c(c = mM_to_M(Mn), z = 2),
    Zn = c(c = mM_to_M(Zn), z = 2),
    Cu = c(c = mM_to_M(Cu), z = 2),

    NO3 = c(c = mM_to_M(NO3), z = -1),
    Cl = c(c = mM_to_M(Cl), z = -1),
    MoO4 = c(c = mM_to_M(Mo), z = -2)  # Mo as molybdate
  )

  # charge balance residual f(phc): positive - negative (meq/L)
  residual_meq <- function(phc) {
    H <- 10^(-phc) # mol/L

    # ---- inner loop: find I -> gammas -> species -> I (fixed point) ----
    I <- 0.03 # initial guess
    gam <- list()
    species <- list()

    for (k in seq_len(inner_max_iter)) {
      # gammas by charge state (Davies)
      gam$z1 <- davies_gamma(I, 1)
      gam$z2 <- davies_gamma(I, 2)
      gam$z3 <- davies_gamma(I, 3)

      gam$H <- gam$z1
      gam$OH <- gam$z1
      gam$NH4 <- gam$z1
      gam$HSO4 <- gam$z1
      gam$SO4 <- gam$z2
      gam$H2PO4 <- gam$z1
      gam$HPO4 <- gam$z2
      gam$PO4 <- gam$z3

      # OH- from water autoprotolysis with activities
      OH <- Kw / (gam$H * gam$OH * H)  # mol/L

      # NH4/NH3 with activities (NH3 neutral => gamma=1)
      rN <- Ka_NH4 * gam$NH4 / (gam$H * H)
      NH4 <- mM_to_M(NT) / (1 + rN)
      NH3 <- mM_to_M(NT) - NH4

      # sulfate with activities
      rS <- Ka_HSO4 * gam$HSO4 / (gam$H * gam$SO4 * H)
      HSO4 <- mM_to_M(ST) / (1 + rS)
      SO4 <- mM_to_M(ST) - HSO4

      # phosphate with "conditional" Ka including gammas
      K1c <- Ka1_P / (gam$H * gam$H2PO4)
      K2c <- Ka2_P * gam$H2PO4 / (gam$H * gam$HPO4)
      K3c <- Ka3_P * gam$HPO4 / (gam$H * gam$PO4)

      D <- H^3 + K1c * H^2 + K1c * K2c * H + K1c * K2c * K3c
      H3PO4 <- mM_to_M(PT) * (H^3 / D)
      H2PO4 <- mM_to_M(PT) * (K1c * H^2 / D)
      HPO4 <- mM_to_M(PT) * (K1c * K2c * H / D)
      PO4 <- mM_to_M(PT) * (K1c * K2c * K3c / D)

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
        PO4 * 3^2
      I_new <- 0.5 * I_new

      # converge inner loop
      species <- list(
        H = H, OH = OH,
        NH4 = NH4, NH3 = NH3,
        HSO4 = HSO4, SO4 = SO4,
        H3PO4 = H3PO4, H2PO4 = H2PO4, HPO4 = HPO4, PO4 = PO4
      )
      if (abs(I_new - I) < 1e-10) {
        I <- I_new
        break
      }
      I <- I_new
    }

    expected_species_full <- c(
      "H", "OH", "NH4", "NH3", "HSO4", "SO4", "H3PO4", "H2PO4", "HPO4", "PO4"
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
      bad <- names(species)[non_finite]
      stop(
        sprintf("Speciation failed: non-finite values for [%s].", paste(bad, collapse = ", ")),
        call. = FALSE
      )
    }

    # ---- charge balance in meq/L (use concentrations) ----
    pos_fix_meq <- KT + Na + 2 * Ca + 2 * Mg + 2 * Fe + 2 * Mn + 2 * Zn + 2 * Cu
    neg_fix_meq <- NO3 + Cl + 2 * Mo

    # variable parts (convert mol/L -> mmol/L)
    H_mM <- species$H * 1e3
    OH_mM <- species$OH * 1e3
    NH4_mM <- species$NH4 * 1e3
    HSO4_mM <- species$HSO4 * 1e3
    SO4_mM <- species$SO4 * 1e3
    H2PO4_mM <- species$H2PO4 * 1e3
    HPO4_mM <- species$HPO4 * 1e3
    PO4_mM <- species$PO4 * 1e3

    pos <- pos_fix_meq + H_mM + NH4_mM
    neg <- neg_fix_meq + OH_mM +
      (HSO4_mM + 2 * SO4_mM) +
      (H2PO4_mM + 2 * HPO4_mM + 3 * PO4_mM)

    list(f = pos - neg, I = I, gam = gam, species = species)
  }

  # --- outer root find (bisection on pHc) ---
  a <- phc_bracket[1]
  b <- phc_bracket[2]
  fa <- residual_meq(a)$f
  fb <- residual_meq(b)$f
  if (fa * fb > 0) stop("Root not bracketed; widen phc_bracket")

  mid <- NA
  out <- NULL
  for (it in seq_len(max_iter)) {
    mid <- 0.5 * (a + b)
    out <- residual_meq(mid)
    fm <- out$f
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
    PO4 = as.numeric(sp$PO4) * 1e3
  )

  expected_species <- c("H", "OH", "NH4", "HSO4", "SO4", "H2PO4", "HPO4", "PO4")
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
    `2Fe` = 2 * Fe,
    `2Mn` = 2 * Mn,
    `2Zn` = 2 * Zn,
    `2Cu` = 2 * Cu
  )
  fixed_anions_meq <- c(
    NO3 = NO3,
    Cl = Cl,
    `2Mo` = 2 * Mo
  )

  variable_meq <- c(
    H_plus = species_mM[["H"]],
    OH_minus = species_mM[["OH"]],
    NH4_plus = species_mM[["NH4"]],
    sulfate_total_meq = species_mM[["HSO4"]] + 2 * species_mM[["SO4"]],
    phosphate_total_meq = species_mM[["H2PO4"]] + 2 * species_mM[["HPO4"]] + 3 * species_mM[["PO4"]]
  )

  pos_meq <- sum(fixed_cations_meq) + variable_meq[["H_plus"]] + variable_meq[["NH4_plus"]]
  neg_meq <- sum(fixed_anions_meq) + variable_meq[["OH_minus"]] +
    variable_meq[["sulfate_total_meq"]] + variable_meq[["phosphate_total_meq"]]

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

  list(
    pH = as.numeric(pH),
    pHc = as.numeric(phc),
    H = as.numeric(H),
    I = as.numeric(out$I),
    gammas = out$gam,
    species_mM = species_mM,
    charge_meq = c(
      pos = as.numeric(pos_meq),
      neg = as.numeric(neg_meq),
      residual = as.numeric(pos_meq - neg_meq)
    ),
    charge_breakdown = charge_breakdown
  )
}
