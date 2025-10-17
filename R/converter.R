#' Convert amount of one compound to another using molar mass and element counts
#'
#' This function computes the conversion factor between two chemical formulas or elements
#' based on their molar masses and the count of shared atoms. It adjusts for stoichiometry
#' where applicable (e.g., converting from "SO4" to "S").
#'
#' @param from A character string representing the source formula or element (e.g., "Ca", "NaCl", "SO4").
#' @param to A character string representing the target formula or element.
#' @param value A numeric value to convert (default is 1). Represents the quantity of `from`.
#'
#' @return A numeric value representing the equivalent amount of `to`, adjusted by molar mass and atom ratio.
#'
#' @examples
#' converte("SO4", "S")        # Convert sulfate to elemental sulfur
#' converte("Na", "NaCl")      # Convert sodium to sodium chloride
#' converte("P2O5", "P")       # Convert phosphorus pentoxide to elemental phosphorus
#' converte("Ca", "CaCl2·2H2O", 10)  # Convert 10 mmol of Ca to CaCl2·2H2O
#'
#' @export


converte <- function(from, to, value = 1) {
  # Normalize to string
  from_str <- if (is.character(from)) from else deparse(substitute(from))
  to_str   <- if (is.character(to)) to else deparse(substitute(to))

  # Compute molar masses
  molar_from <- compute_molar_mass(from_str)
  molar_to   <- compute_molar_mass(to_str)

  if (is.na(molar_from) || is.na(molar_to)) {
    stop(sprintf("Invalid formula: from = '%s', to = '%s'", from_str, to_str))
  }

  # Parse element counts
  el_counts_from <- parse_formula(from_str)
  el_counts_to   <- parse_formula(to_str)

  # Determine common elements
  common_elements <- intersect(names(el_counts_from), names(el_counts_to))
  if (length(common_elements) == 0) {
    warning("No shared elements between 'from' and 'to'. Assuming 1:1 conversion.")
    return(value * (molar_to / molar_from))
  }

  # For each shared element, apply count-based adjustment
  el <- common_elements[1]  # use first match
  count_from <- el_counts_from[[el]]
  count_to   <- el_counts_to[[el]]

  adjusted <- value * (molar_to / molar_from) * (count_from / count_to)
  return(adjusted)
}


