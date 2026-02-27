#' Lookup table of ion diffusion coefficients at 25 C
#'
#' Returns a data frame with ions, charge, and diffusion coefficients (D) at 25 C.
#' Values are expressed as 10^-5 cm^2/s.
#'
#' @return A data.frame with columns: ion, z, D_1e5_cm2_s.
#' @export
ion_diffusion_25C <- function() {
  data.frame(
    ion = c(
      "H+", "OH-",
      "K+", "Na+", "NH4+",
      "Ca2+", "Mg2+",
      "Fe2+", "Fe3+", "Mn2+", "Zn2+", "Cu2+",
      "Cl-", "NO3-",
      "HCO3-", "CO3-2",
      "HSO4-", "SO4-2",
      "H2PO4-", "HPO4-2", "PO4-3",
      "MoO4-2"
    ),
    z = c(
      +1, -1,
      +1, +1, +1,
      +2, +2,
      +2, +3, +2, +2, +2,
      -1, -1,
      -1, -2,
      -1, -2,
      -1, -2, -3,
      -2
    ),
    # Diffusion coefficients at 25 C in 10^-5 cm^2/s
    D_1e5_cm2_s = c(
      9.310, 5.273,
      1.957, 1.334, 1.957,
      0.792, 0.706,
      0.719, 0.604, 0.712, 0.703, 0.714,
      2.032, 1.902,
      1.185, 0.923,
      1.335, 1.065,
      0.959, 0.759, 0.824,
      1.984
    ),
    stringsAsFactors = FALSE
  )
}

# Convert achieved totals + pH-speciated species into the ions we want for EC (mmol/L).
make_ec_ions <- function(achieved, ph_res) {
  achieved <- to_named_numeric(achieved)
  get0 <- function(x, nm) if (!is.null(x[[nm]])) x[[nm]] else 0

  # Fixed strong electrolytes from totals (mmol/L)
  K   <- get0(achieved, "K")
  Na  <- get0(achieved, "Na")
  Ca  <- get0(achieved, "Ca")
  Mg  <- get0(achieved, "Mg")
  Fe  <- get0(achieved, "Fe")
  Mn  <- get0(achieved, "Mn")
  Zn  <- get0(achieved, "Zn")
  Cu  <- get0(achieved, "Cu")
  Cl  <- get0(achieved, "Cl")
  NO3 <- get0(achieved, "NO3_N")  # NO3-N is 1:1 with NO3-

  # Mo: assume molybdate for EC purposes (mmol/L)
  Mo <- get0(achieved, "Mo")

  # From your pH/speciation output (mmol/L)
  sp <- ph_res$species_mM
  if (is.null(sp)) stop("ph_res$species_mM is missing.", call. = FALSE)

  H    <- get0(sp, "H")
  OH   <- get0(sp, "OH")
  NH4  <- get0(sp, "NH4")

  HSO4 <- get0(sp, "HSO4")
  SO4  <- get0(sp, "SO4")

  HCO3 <- get0(sp, "HCO3")
  CO3  <- get0(sp, "CO3")

  H2PO4 <- get0(sp, "H2PO4")
  HPO4  <- get0(sp, "HPO4")
  PO4   <- get0(sp, "PO4")

  # Build concentration vector (mmol/L) for ions in our lookup
  c(
    "H+"     = H,
    "OH-"    = OH,
    "K+"     = K,
    "Na+"    = Na,
    "NH4+"   = NH4,
    "Ca2+"   = Ca,
    "Mg2+"   = Mg,
    "Fe2+"   = Fe,   # total Fe mapped to Fe2+ unless fe_state forces Fe3+
    "Mn2+"   = Mn,
    "Zn2+"   = Zn,
    "Cu2+"   = Cu,
    "Cl-"    = Cl,
    "NO3-"   = NO3,
    "HCO3-"  = HCO3,
    "CO3-2"  = CO3,
    "HSO4-"  = HSO4,
    "SO4-2"  = SO4,
    "H2PO4-" = H2PO4,
    "HPO4-2" = HPO4,
    "PO4-3"  = PO4,
    "MoO4-2" = Mo
  )
}

#' Compute EC from achieved composition and pH speciation
#'
#' Estimates conductivity using an ion-transport summation based on diffusion
#' coefficients and activity coefficients.
#'
#' Units:
#' - c is in mmol/L (mM); numerically identical to mol/m^3, used directly as such.
#' - D is in 10^-5 cm^2/s and converted to m^2/s by multiplying 1e-9.
#' - Output: EC in mS/cm and uS/cm.
#'
#' gamma_model:
#'   1 = Davies (default)
#'   2 = Debye-Hückel
#'
#' Override precedence:
#'   model baseline -> ph_res$gamma_ions -> gamma argument (highest priority)
#'
#' @param achieved Named numeric vector (mmol/L) or data.frame with columns
#'   Nutrient and Achieved.
#' @param ph_res Result list from `ph_from_achieved()` containing `species_mM`.
#' @param fe_state One of "Fe2+" or "Fe3+". When "Fe3+", all Fe2+ is treated
#'   as Fe3+ for EC purposes.
#' @param t_C Temperature in degrees C.
#' @param gamma Optional named numeric vector of activity coefficients (per ion).
#' @param gamma_model Activity model selector: 1/"davies_25C" (Davies) or 2/"debye_huckel_25C" (Debye-Huckel). Default 1.
#' @param A_DH_25 Debye-Huckel A constant at 25 C (also used as A in Davies).
#'
#' @return A list with EC_mS_cm, EC_uS_cm, and per-ion contributions.
#' @export
ec_from_ph <- function(
    achieved,
    ph_res,
    fe_state = c("Fe2+", "Fe3+"),
    t_C = 25,
    gamma = NULL,
    gamma_model = 1,
    A_DH_25 = 0.5085
) {
  fe_state <- match.arg(fe_state)

  # ---- normalize gamma_model: allow 1/2 or helpful strings ----
  gm <- tolower(as.character(gamma_model[1]))
  if (gm %in% c("1", "davies", "davies_25c")) gm <- "davies_25c"
  if (gm %in% c("2", "dh", "debye", "debye_huckel", "debye-huckel", "debye_huckel_25c")) gm <- "debye_huckel_25c"
  if (!gm %in% c("davies_25c", "debye_huckel_25c")) {
    stop("gamma_model must be one of 1/'davies_25C' or 2/'debye_huckel_25C'.", call. = FALSE)
  }
  gamma_model <- if (identical(gm, "davies_25c")) "davies_25C" else "debye_huckel_25C"
  gamma_model_id <- if (identical(gamma_model, "davies_25C")) 1L else 2L

  # ---- helpers ----
  gamma_dh_25C <- function(z, I_mol_L, A = 0.5085, tol = 1e-12) {
    z <- as.numeric(z)
    I <- as.numeric(I_mol_L)
    if (!is.finite(I) || I <= tol) return(rep(1, length(z)))
    10^(-A * z^2 * sqrt(I))
  }

  gamma_davies_25C <- function(z, I_mol_L, A = 0.5085, tol = 1e-12) {
    z <- as.numeric(z)
    I <- as.numeric(I_mol_L)
    if (!is.finite(I) || I <= tol) return(rep(1, length(z)))
    sI <- sqrt(I)
    term <- (sI / (1 + sI)) - 0.3 * I
    10^(-A * z^2 * term)
  }

  apply_named_gamma_override <- function(current, ions, gamma_named) {
    if (is.null(gamma_named) || is.null(names(gamma_named))) return(current)
    idx <- match(ions, names(gamma_named))
    g   <- gamma_named[idx]
    ok  <- !is.na(idx) & is.finite(g) & g > 0
    current[ok] <- g[ok]
    current
  }

  # ---- data ----
  tab <- ion_diffusion_25C()

  # Ion concentrations (mmol/L) from your existing pipeline
  c_mM <- make_ec_ions(achieved, ph_res)

  # Ensure Fe3+ exists for switching
  if (!"Fe3+" %in% names(c_mM)) c_mM[["Fe3+"]] <- 0

  # Fe redox choice
  if (fe_state == "Fe3+") {
    c_mM[["Fe3+"]] <- c_mM[["Fe2+"]]
    c_mM[["Fe2+"]] <- 0
  }

  # Merge concentrations with diffusion lookup
  df <- merge(
    data.frame(ion = names(c_mM), c_mM = as.numeric(c_mM), stringsAsFactors = FALSE),
    tab,
    by = "ion",
    all = FALSE
  )
  if (nrow(df) == 0) {
    stop("No overlapping ions between concentrations and diffusion lookup table.", call. = FALSE)
  }
  df$c_mM[!is.finite(df$c_mM)] <- 0

  # Ionic strength I in mol/L (c_mM is mmol/L)
  I_mol_L <- 0.5 * sum((df$z^2) * (df$c_mM / 1000), na.rm = TRUE)

  # alpha exponent (piecewise)
  abs_z <- abs(df$z)
  df$alpha <- ifelse(
    I_mol_L <= 0.36 * abs_z,
    0.6 / sqrt(abs_z),
    sqrt(I_mol_L) / abs_z
  )

  # ---- baseline gamma from selected model ----
  gamma_vals <- if (identical(gamma_model, "davies_25C")) {
    gamma_davies_25C(df$z, I_mol_L, A = A_DH_25)
  } else {
    gamma_dh_25C(df$z, I_mol_L, A = A_DH_25)
  }

  # Overrides: first ph_res (lower priority), then explicit gamma (highest)
  if (!is.null(ph_res$gamma_ions)) {
    gamma_vals <- apply_named_gamma_override(gamma_vals, df$ion, ph_res$gamma_ions)
  }
  if (!is.null(gamma)) {
    gamma_vals <- apply_named_gamma_override(gamma_vals, df$ion, gamma)
  }

  # Clamp
  gamma_vals[!is.finite(gamma_vals) | gamma_vals <= 0] <- .Machine$double.xmin
  df$gamma <- as.numeric(gamma_vals)

  # ---- conductivity sum ----
  F_const <- 9.6485e4
  R_const <- 8.31446
  T_K <- t_C + 273.15
  pref <- F_const^2 / (R_const * T_K)

  # D: 10^-5 cm^2/s -> m^2/s
  D_m2_s <- df$D_1e5_cm2_s * 1e-9

  # c: mmol/L == mol/m^3 numerically
  c_mol_m3 <- df$c_mM

  # Contribution in S/m
  kappa_S_m <- pref * D_m2_s * (df$z^2) * (df$gamma^df$alpha) * c_mol_m3

  # Convert to mS/cm (1 S/m = 10 mS/cm)
  df$kappa_mS_cm <- kappa_S_m * 10

  EC_mS_cm <- sum(df$kappa_mS_cm, na.rm = TRUE)
  EC_uS_cm <- EC_mS_cm * 1000

  list(
    EC_mS_cm = EC_mS_cm,
    EC_uS_cm = EC_uS_cm,
    contributions = df[order(-df$kappa_mS_cm), c(
      "ion", "c_mM", "z", "D_1e5_cm2_s", "gamma", "alpha", "kappa_mS_cm"
    )],
    inputs = list(
      t_C = t_C,
      T_K = T_K,
      I_mol_L = I_mol_L,
      gamma_model = gamma_model,
      gamma_model_id = gamma_model_id,
      gamma_model_name = gamma_model,
      A_DH_25 = A_DH_25
    )
  )
}
