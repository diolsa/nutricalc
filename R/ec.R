<<<<<<< HEAD
#' Lookup table of diffusion coefficients at 25 °C for EC calculation
#'
#' Returns a data frame with ions, charge, and diffusion coefficients at 25 °C.
#' Diffusion values are stored as numbers in 10^-5 cm^2/s (which is numerically
#' equal to 10^-9 m^2/s).
=======
#' Lookup table of ion diffusion coefficients at 25 C
#'
#' Returns a data frame with ions, charge, and diffusion coefficients (D) at 25 C.
#' Values are expressed as 10^-5 cm^2/s.
>>>>>>> 3c18957ec15e594e4615c877b4277bd2f688970a
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
<<<<<<< HEAD
    # Diffusion at 25 °C in 10^-5 cm^2/s
=======
>>>>>>> 3c18957ec15e594e4615c877b4277bd2f688970a
    D_1e5_cm2_s = c(
      9.310, 5.270,
      1.960, 1.330, 1.980,
      0.793, 0.705,
      0.719, 0.604, 0.688, 0.715, 0.733,
      2.030, 1.900,
      1.180, 0.955,
      1.330, 1.070,
      0.846, 0.690, 0.612,
      1.984
    ),
    stringsAsFactors = FALSE
  )
}

#' Compute electrical conductivity (EC) from achieved composition and pH speciation
#'
<<<<<<< HEAD
#' Estimates solution electrical conductivity from an ion mixture using:
#'   EC = (F^2 / (R*T)) * sum_i( D_i * z_i^2 * (gamma_i^alpha_i) * c_i )
#'
#' Inputs are expected in mmol/L (mM). Numerically, mmol/L equals mol/m^3,
#' which is consistent with SI when D is in m^2/s.
#'
#' Activity coefficients can be supplied (gamma), taken from `ph_res$gamma_ions`,
#' or approximated via the simple Debye–Hückel expression at 25 °C:
#'   log10(gamma_i) = -A * z_i^2 * sqrt(I), with A = 0.5085 (25 °C, water)
#'
#' The exponent alpha_i is computed from ionic strength I (mol/L):
#'   alpha_i = 0.6/sqrt(|z_i|)           if I <= 0.36*|z_i|
#'            = sqrt(I)/|z_i|            otherwise
#'
#' @param achieved Named numeric vector (mmol/L) or data.frame with columns
#'   Nutrient and Achieved.
#' @param ph_res Result list from your pH/speciation routine; must contain `species_mM`
#'   (named numeric, mmol/L). Optionally may contain `gamma_ions` (named numeric).
#' @param fe_state Either "Fe2+" or "Fe3+". If "Fe3+", all Fe is assigned to Fe3+.
#' @param t_C Temperature in °C used in the prefactor (default 25).
#' @param gamma Optional named numeric vector of per-ion activity coefficients.
#' @param gamma_model One of "auto", "debye_huckel_25C", "unity", "provided".
#'   - "auto": use `gamma` if provided, else `ph_res$gamma_ions` if present, else Debye–Hückel.
#'   - "debye_huckel_25C": always use Debye–Hückel at 25 °C (A = A_DH_25).
#'   - "unity": gamma_i = 1 for all ions.
#'   - "provided": require `gamma` and use it.
#' @param A_DH_25 Debye–Hückel A parameter at 25 °C (default 0.5085).
#'
#' @return A list with:
#'   - EC_mS_cm: conductivity in mS/cm
#'   - EC_uS_cm: conductivity in µS/cm
#'   - inputs: list of diagnostics (T, ionic strength, gamma model)
#'   - contributions: per-ion contributions (mS/cm), sorted descending
=======
#' Estimates conductivity using an ion-transport summation based on diffusion
#' coefficients and activity coefficients.
#'
#' @param achieved Named numeric vector (mmol/L) or data.frame with columns
#'   Nutrient and Achieved.
#' @param ph_res Result list from `ph_from_achieved()` containing `species_mM`.
#' @param fe_state One of "Fe2+" or "Fe3+". When "Fe3+", all Fe2+ is treated
#'   as Fe3+ for EC purposes.
#' @param t_C Temperature in degrees C.
#' @param gamma Optional named numeric vector of activity coefficients.
#' @param gamma_model One of "auto", "debye_huckel_25C", "unity", or "provided".
#' @param A_DH_25 Debye-Huckel A constant at 25 C.
>>>>>>> 3c18957ec15e594e4615c877b4277bd2f688970a
#'
#' @export
<<<<<<< HEAD
ec_from_ph <- function(achieved, ph_res,
                       fe_state = c("Fe2+", "Fe3+"),
                       t_C = 25,
                       gamma = NULL,
                       gamma_model = c("auto", "debye_huckel_25C", "unity", "provided"),
                       A_DH_25 = 0.5085) {

  fe_state <- match.arg(fe_state)
  gamma_model <- match.arg(gamma_model)

  # Constants (SI)
  F <- 9.6485e4   # C/mol
  R <- 8.31446    # J/(K mol)

  # Ionic strength I in mol/L (c is in mmol/L)
  ionic_strength_mol_L <- function(c_mM, z) {
    c_mol_L <- c_mM / 1000.0
    0.5 * sum((z^2) * c_mol_L, na.rm = TRUE)
  }

  # alpha exponent (piecewise rule)
  alpha_rule <- function(I_mol_L, z) {
    az <- abs(z)
    ifelse(I_mol_L <= 0.36 * az, 0.6 / sqrt(az), sqrt(I_mol_L) / az)
  }

  # Debye–Hückel gamma at 25 °C
  gamma_debye_huckel_25C <- function(I_mol_L, z, A = 0.5085) {
    10^(-A * (z^2) * sqrt(I_mol_L))
  }

  # Unit conversions
  S_m_to_mS_cm <- function(x) x * 10.0     # 1 S/m = 10 mS/cm
  mS_cm_to_uS_cm <- function(x) x * 1000

  # Ion concentrations (mmol/L) from your existing pipeline
  c_mM <- make_ec_ions(achieved, ph_res)

  # Ensure Fe3+ exists for switching
  if (is.null(c_mM[["Fe3+"]])) c_mM[["Fe3+"]] <- 0

  # Fe redox choice
=======
ec_from_ph <- function(
  achieved,
  ph_res,
  fe_state = c("Fe2+", "Fe3+"),
  t_C = 25,
  gamma = NULL,
  gamma_model = c("auto", "debye_huckel_25C", "unity", "provided"),
  A_DH_25 = 0.5085
) {
  fe_state <- match.arg(fe_state)
  gamma_model <- match.arg(gamma_model)

  tab <- ion_diffusion_25C()
  c_mM <- make_ec_ions(achieved, ph_res)
  if (!"Fe3+" %in% names(c_mM)) {
    c_mM[["Fe3+"]] <- 0
  }
>>>>>>> 3c18957ec15e594e4615c877b4277bd2f688970a
  if (fe_state == "Fe3+") {
    c_mM[["Fe3+"]] <- c_mM[["Fe2+"]]
    c_mM[["Fe2+"]] <- 0
  }

<<<<<<< HEAD
  tab <- ion_diffusion_25C()

  # Align concentrations with lookup
=======
>>>>>>> 3c18957ec15e594e4615c877b4277bd2f688970a
  df <- merge(
    data.frame(ion = names(c_mM), c_mM = as.numeric(c_mM), stringsAsFactors = FALSE),
    tab,
    by = "ion",
    all.x = FALSE, all.y = FALSE
  )
<<<<<<< HEAD
  if (nrow(df) == 0) stop("No overlapping ions between concentrations and lookup table.", call. = FALSE)
  df$c_mM[!is.finite(df$c_mM)] <- 0
=======
  if (nrow(df) == 0) {
    stop("No overlapping ions between concentrations and diffusion lookup table.", call. = FALSE)
  }
  df$c_mM[!is.finite(df$c_mM)] <- 0

  I_mol_L <- 0.5 * sum((df$z^2) * (df$c_mM / 1000), na.rm = TRUE)
  abs_z <- abs(df$z)
  df$alpha <- ifelse(
    I_mol_L <= 0.36 * abs_z,
    0.6 / sqrt(abs_z),
    sqrt(I_mol_L) / abs_z
  )

  gamma_dh <- function(z, I_mol_L, A) {
    10^(-A * z^2 * sqrt(I_mol_L))
  }

  if (gamma_model == "auto") {
    if (!is.null(gamma)) {
      gamma_src <- gamma
      gamma_vals <- gamma_src[df$ion]
      gamma_vals[is.na(gamma_vals)] <- 1
    } else if (!is.null(ph_res$gamma_ions)) {
      gamma_src <- ph_res$gamma_ions
      gamma_vals <- gamma_src[df$ion]
      gamma_vals[is.na(gamma_vals)] <- 1
    } else {
      gamma_vals <- gamma_dh(df$z, I_mol_L, A_DH_25)
    }
  } else if (gamma_model == "provided") {
    if (is.null(gamma) || is.null(names(gamma))) {
      stop("gamma_model = \"provided\" requires a named numeric gamma vector.", call. = FALSE)
    }
    gamma_vals <- gamma[df$ion]
    missing_gamma <- df$ion[is.na(gamma_vals)]
    if (length(missing_gamma) > 0) {
      stop(
        sprintf(
          "gamma is missing values for ions: %s",
          paste(unique(missing_gamma), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  } else if (gamma_model == "unity") {
    gamma_vals <- rep(1, nrow(df))
  } else {
    gamma_vals <- gamma_dh(df$z, I_mol_L, A_DH_25)
  }

  if (any(!is.finite(gamma_vals) | gamma_vals <= 0)) {
    stop("gamma must be finite and > 0 for all ions used in EC.", call. = FALSE)
  }

  df$gamma <- as.numeric(gamma_vals)

  F_const <- 9.6485e4
  R_const <- 8.31446
  T_K <- t_C + 273.15
  pref <- F_const^2 / (R_const * T_K)
  D_m2_s <- df$D_1e5_cm2_s * 1e-9
  c_mol_m3 <- df$c_mM
  kappa_S_m <- pref * D_m2_s * (df$z^2) * (df$gamma^df$alpha) * c_mol_m3
  df$kappa_mS_cm <- kappa_S_m * 10
>>>>>>> 3c18957ec15e594e4615c877b4277bd2f688970a

  # Ionic strength and alpha
  I_mol_L <- ionic_strength_mol_L(df$c_mM, df$z)
  df$alpha <- alpha_rule(I_mol_L, df$z)

  # Choose gamma source
  gamma_used <- NULL
  if (gamma_model == "provided") {
    if (is.null(gamma)) stop("gamma_model='provided' requires a named gamma vector.", call. = FALSE)
    gamma_used <- to_named_numeric(gamma)
  } else if (gamma_model == "unity") {
    gamma_used <- setNames(rep(1.0, nrow(df)), df$ion)
  } else if (gamma_model == "debye_huckel_25C") {
    gamma_used <- setNames(gamma_debye_huckel_25C(I_mol_L, df$z, A = A_DH_25), df$ion)
  } else { # auto
    if (!is.null(gamma)) {
      gamma_used <- to_named_numeric(gamma)
    } else if (!is.null(ph_res$gamma_ions)) {
      gamma_used <- to_named_numeric(ph_res$gamma_ions)
    } else {
      gamma_used <- setNames(gamma_debye_huckel_25C(I_mol_L, df$z, A = A_DH_25), df$ion)
    }
  }

  # Align gamma to df; missing -> 1
  g <- rep(1.0, nrow(df))
  hit <- intersect(df$ion, names(gamma_used))
  g[match(hit, df$ion)] <- gamma_used[hit]
  if (any(!is.finite(g)) || any(g <= 0)) stop("gamma must be finite and > 0 for all used ions.", call. = FALSE)
  df$gamma <- g

  # Units for the summation:
  # - c in mmol/L is numerically equal to mol/m^3
  c_mol_m3 <- df$c_mM
  # - D in 10^-5 cm^2/s -> m^2/s by multiplying 1e-9
  df$D_m2_s <- df$D_1e5_cm2_s * 1e-9

  # Prefactor
  T_K <- t_C + 273.15
  pref <- (F^2) / (R * T_K)

  # Conductivity contributions (S/m)
  df$kappa_S_m <- pref * (df$D_m2_s * (df$z^2) * (df$gamma^df$alpha) * c_mol_m3)

  # Total EC
  EC_S_m   <- sum(df$kappa_S_m, na.rm = TRUE)
  EC_mS_cm <- S_m_to_mS_cm(EC_S_m)
  EC_uS_cm <- mS_cm_to_uS_cm(EC_mS_cm)

  # Per-ion contributions in mS/cm (for reporting)
  df$kappa_mS_cm <- S_m_to_mS_cm(df$kappa_S_m)

  list(
    EC_mS_cm = EC_mS_cm,
    EC_uS_cm = EC_uS_cm,
<<<<<<< HEAD
    inputs = list(t_C = t_C, T_K = T_K, I_mol_L = I_mol_L,
                  gamma_model = gamma_model, A_DH_25 = A_DH_25),
    contributions = df[order(-df$kappa_mS_cm),
                       c("ion", "c_mM", "z", "D_1e5_cm2_s", "gamma", "alpha", "kappa_mS_cm")]
=======
    contributions = df[order(-df$kappa_mS_cm), c(
      "ion", "c_mM", "z", "D_1e5_cm2_s", "gamma", "alpha", "kappa_mS_cm"
    )],
    inputs = list(
      t_C = t_C,
      T_K = T_K,
      I_mol_L = I_mol_L,
      gamma_model = gamma_model,
      A_DH_25 = A_DH_25
    )
>>>>>>> 3c18957ec15e594e4615c877b4277bd2f688970a
  )
}
