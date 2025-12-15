#' Atomic molar masses used for formula parsing
#'
#' A lookup table of elemental (atomic) molar masses in grams per mole (g/mol).
#' Used by molar-mass and unit-conversion helpers (e.g., when computing the molar
#' mass of a chemical formula from its elemental composition).
#'
#' @format A data frame with 23 rows and 2 columns:
#' \describe{
#'   \item{Element}{Element symbol (character), e.g. \code{"C"}, \code{"O"}, \code{"Fe"}.}
#'   \item{MolarMass_g_per_mol}{Atomic molar mass in g/mol (numeric).}
#' }
#'
#' @usage data(element_molar_mass_df)
"element_molar_mass_df"


element_molar_mass_df <- data.frame(
  Element = c("C", "O",   "H",    "N",    "P",     "K",     "Mg",    "S",     "Ca",
              "B",   "Cu",   "Cl",   "Fe",    "Mn",    "Ni",    "Zn",    "Mo",
              "Na",  "Al",   "Co",   "Se",    "Si"),
  MolarMass_g_per_mol = c(
    12.011, 15.999, 1.008, 14.007, 30.974, 39.098, 24.305, 32.065, 40.078,
    10.811, 63.546, 35.453, 55.845, 54.938, 58.693, 65.38,  95.95,
    22.990, 26.982, 58.933, 78.971, 28.085
  )
)
