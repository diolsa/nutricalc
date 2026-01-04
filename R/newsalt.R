#' Add a fertilizer to the nutrient matrix
#'
#' @param formula Chemical formula (e.g. "NaCl", "Ca(NO3)2", "CuSO4\u00b75H2O")
#' @param nutrient_matrix Existing nutrient matrix [fertilizer x nutrients]
#' @return Updated nutrient matrix with new fertilizer row
#' @export
add_fertilizer <- function(formula, nutrient_matrix) {
  # Sanitize and parse formula
  formula <- sanitize_formula_input(formula)
  element_counts <- parse_formula(formula)

  # Handle NO3 and NH4 mapping to NO3_N and NH4_N
  if ("NO3" %in% names(element_counts)) {
    element_counts[["NO3_N"]] <- element_counts[["NO3"]]
    element_counts[["NO3"]] <- NULL
  }
  if ("NH4" %in% names(element_counts)) {
    element_counts[["NH4_N"]] <- element_counts[["NH4"]]
    element_counts[["NH4"]] <- NULL
  }

  # Supported nutrients
  nutrients <- colnames(nutrient_matrix)

  # Initialize row
  new_row <- stats::setNames(numeric(length(nutrients)), nutrients)

  # Populate matching values
  for (el in names(element_counts)) {
    if (el %in% nutrients) {
      new_row[el] <- element_counts[[el]]
    }
  }

  # Append to matrix
  nutrient_matrix <- rbind(nutrient_matrix, new_row)
  rownames(nutrient_matrix)[nrow(nutrient_matrix)] <- formula

  return(nutrient_matrix)
}
