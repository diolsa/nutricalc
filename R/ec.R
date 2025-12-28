#' Lookup table of limiting equivalent conductances at 25 C
#'
#' Returns a data frame with ions, charge, and limiting equivalent conductances
#' (Lambda0_eq) at 25 C. Values are expressed as S*cm^2/eq.
#'
#' @return A data.frame with columns: ion, z, Lambda0_eq.
#' @export
ion_lambda0_25C <- function() {
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
    # Lambda0_eq (S*cm^2/eq). For multivalent ions we use values already per eq.
    Lambda0_eq = c(
      349.81, 198.5,
      73.48, 50.11, 73.5,
      59.47, 53.06,
      53.1, 68.0, 53.5, 52.8, 53.6,
      76.35, 71.45,
      44.5, 69.3,
      50.1, 80.0,
      36.0, 57.0, 92.8,
      53.0
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
#' Uses limiting equivalent conductances (Lambda0_eq) at 25 C to estimate
#' conductivity from species concentrations.
#'
#' @param achieved Named numeric vector (mmol/L) or data.frame with columns
#'   Nutrient and Achieved.
#' @param ph_res Result list from `ph_from_achieved()` containing `species_mM`.
#' @param method One of "standard" or "divide_z".
#'
#' @return A list with EC_mS_cm, EC_uS_cm, and per-ion contributions.
#' @export
ec_from_ph <- function(achieved, ph_res, fe_state = c("Fe2+", "Fe3+")) {
  fe_state <- match.arg(fe_state)

  tab <- ion_lambda0_25C()
  c_mM <- make_ec_ions(achieved, ph_res)
  if (fe_state == "Fe3+") {
    c_mM[["Fe3+"]] <- c_mM[["Fe2+"]]
    c_mM[["Fe2+"]] <- 0
  }

  ions <- intersect(names(c_mM), tab$ion)
  if (length(ions) == 0) stop("No overlapping ions between concentrations and lookup table.", call. = FALSE)

  df <- merge(
    data.frame(ion = names(c_mM), c_mM = as.numeric(c_mM), stringsAsFactors = FALSE),
    tab,
    by = "ion",
    all.x = FALSE,
    all.y = FALSE
  )
  df$c_mM[!is.finite(df$c_mM)] <- 0
  df$kappa_mS_cm <- df$c_mM * df$Lambda0_eq / 1000

  EC_mS_cm <- sum(df$kappa_mS_cm, na.rm = TRUE)
  EC_uS_cm <- EC_mS_cm * 1000

  list(
    EC_mS_cm = EC_mS_cm,
    EC_uS_cm = EC_uS_cm,
    contributions = df[order(-df$kappa_mS_cm), c("ion", "c_mM", "z", "Lambda0_eq", "kappa_mS_cm")]
  )
}
