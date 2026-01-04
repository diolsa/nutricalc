#' Convert amount of one compound to another using molar mass and element counts
#'
#' This function computes the conversion factor between two chemical formulas or elements
#' based on their molar masses and the count of shared atoms. It adjusts for stoichiometry
#' where applicable (e.g., converting from "SO4" to "S").
#'
#' @param from A character string representing the source formula or element (e.g., "Ca", "NaCl", "SO4").
#' @param to A character string representing the target formula or element.
#' @param value A numeric value to convert (default is 1). Represents the quantity of `from`.
#' @param element Optional element symbol to anchor the stoichiometric conversion when
#'   multiple elements are shared between `from` and `to`. If omitted and more than one
#'   element is shared, an error is raised to avoid ambiguous conversions.
#'
#' @return A numeric value representing the equivalent amount of `to`, adjusted by molar mass and atom ratio.
#'
#' @examples
#' converte("SO4", "S")        # Convert sulfate to elemental sulfur
#' converte("Na", "NaCl")      # Convert sodium to sodium chloride
#' converte("P2O5", "P")       # Convert phosphorus pentoxide to elemental phosphorus
#' converte("Ca", "CaCl2\u00b72H2O", 10)  # Convert 10 mmol of Ca to CaCl2\u00b72H2O
#' converte("(NH4)2HPO4", "NH4H2PO4", element = "P")  # Disambiguate using P as the anchor
#'
#' @export


converte <- function(from, to, value = 1, element = NULL) {
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

  # Determine anchor element
  if (is.null(element)) {
    common_elements <- intersect(names(el_counts_from), names(el_counts_to))
    if (length(common_elements) == 0) {
      stop("No shared elements between 'from' and 'to'; specify an element explicitly.", call. = FALSE)
    }
    if (length(common_elements) > 1) {
      stop(
        "Multiple shared elements (", paste(common_elements, collapse = ", "),
        "); please provide `element` to disambiguate.",
        call. = FALSE
      )
    }
    el <- common_elements[1]
  } else {
    el <- as.character(element)[1]
    if (!el %in% names(el_counts_from)) {
      stop("Element '", el, "' not found in 'from' formula.", call. = FALSE)
    }
    if (!el %in% names(el_counts_to)) {
      stop("Element '", el, "' not found in 'to' formula.", call. = FALSE)
    }
  }

  count_from <- el_counts_from[[el]]
  count_to   <- el_counts_to[[el]]

  adjusted <- value * (molar_to / molar_from) * (count_from / count_to)
  return(adjusted)
}
