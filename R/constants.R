#' Canonical nutrient keys for targets and UI
#'
#' Defines the stable nutrient order used across recipes, UI inputs, and target
#' vectors. This is intentionally independent from the full `nutrient_matrix`,
#' which may include additional columns for chelate ligands or other species.
#'
#' @format A character vector of nutrient keys.
#' @export
CANONICAL_NUTRIENTS <- c(
  "NO3_N", "NH4_N", "P", "K", "Ca", "Mg", "S", "Na", "Cl",
  "Fe", "Mn", "Zn", "B", "Cu", "Mo", "Si"
)
