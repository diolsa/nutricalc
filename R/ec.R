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

# Convert achieved totals + pH-speciated species into the ions we want for EC.
make_ec_ions <- function(achieved, ph_res) {
  achieved <- to_named_numeric(achieved)
  get0 <- function(x, nm) if (!is.null(x[[nm]])) x[[nm]] else 0

  # Fixed strong electrolytes from totals (mmol/L)
  K <- get0(achieved, "K")
  Na <- get0(achieved, "Na")
  Ca <- get0(achieved, "Ca")
  Mg <- get0(achieved, "Mg")
  Fe <- get0(achieved, "Fe")
  Mn <- get0(achieved, "Mn")
  Zn <- get0(achieved, "Zn")
  Cu <- get0(achieved, "Cu")
  Cl <- get0(achieved, "Cl")
  NO3 <- get0(achieved, "NO3_N")  # NO3-N is 1:1 with NO3-

  # Mo: assume molybdate for EC purposes (mmol/L)
  Mo <- get0(achieved, "Mo")

  # From your pH/speciation output (mmol/L)
  sp <- ph_res$species_mM
  if (is.null(sp)) stop("ph_res$species_mM is missing.", call. = FALSE)

  H <- get0(sp, "H")
  OH <- get0(sp, "OH")
  NH4 <- get0(sp, "NH4")

  HSO4 <- get0(sp, "HSO4")
  SO4 <- get0(sp, "SO4")

  HCO3 <- get0(sp, "HCO3")
  CO3 <- get0(sp, "CO3")

  H2PO4 <- get0(sp, "H2PO4")
  HPO4 <- get0(sp, "HPO4")
  PO4 <- get0(sp, "PO4")

  # Build concentration vector (mmol/L) for ions in our lookup
  c(
    "H+" = H,
    "OH-" = OH,
    "K+" = K,
    "Na+" = Na,
    "NH4+" = NH4,
    "Ca2+" = Ca,
    "Mg2+" = Mg,
    "Fe2+" = Fe,
    "Mn2+" = Mn,
    "Zn2+" = Zn,
    "Cu2+" = Cu,
    "Cl-" = Cl,
    "NO3-" = NO3,
    "HCO3-" = HCO3,
    "CO3-2" = CO3,
    "HSO4-" = HSO4,
    "SO4-2" = SO4,
    "H2PO4-" = H2PO4,
    "HPO4-2" = HPO4,
    "PO4-3" = PO4,
    "MoO4-2" = Mo
  )
}

#' Compute EC from achieved composition and pH speciation
#'
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
#'
#' @return A list with EC_mS_cm, EC_uS_cm, and per-ion contributions.
#' @export
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
  if (fe_state == "Fe3+") {
    c_mM[["Fe3+"]] <- c_mM[["Fe2+"]]
    c_mM[["Fe2+"]] <- 0
  }

  df <- merge(
    data.frame(ion = names(c_mM), c_mM = as.numeric(c_mM), stringsAsFactors = FALSE),
    tab,
    by = "ion",
    all.x = FALSE,
    all.y = FALSE
  )
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
      A_DH_25 = A_DH_25
    )
  )
}
