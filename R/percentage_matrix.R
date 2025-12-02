# Map nutrient names in the matrix to their elemental symbols
nutrient_element_map <- list(
  "NO3_N" = "N",
  "NH4_N" = "N",
  "P"     = "P",
  "K"     = "K",
  "Ca"    = "Ca",
  "Mg"    = "Mg",
  "S"     = "S",
  "Na"    = "Na",
  "Cl"    = "Cl",
  "Fe"    = "Fe",
  "Mn"    = "Mn",
  "Zn"    = "Zn",
  "B"     = "B",
  "Cu"    = "Cu",
  "Mo"    = "Mo",
  "Si"    = "Si"
)

#' Compute a matrix of mass percentages of nutrients in each fertilizer
#'
#' @param nutrient_matrix A [fertilizer x nutrient] matrix in mmol/mol
#' @return A matrix of nutrient mass percentages per fertilizer
#' @export
compute_percentage_matrix <- function(nutrient_matrix) {
  if (!exists(".molar_mass")) stop("'.molar_mass' must be loaded in the environment.")

  percentage_matrix <- matrix(0, nrow = nrow(nutrient_matrix), ncol = ncol(nutrient_matrix))
  colnames(percentage_matrix) <- colnames(nutrient_matrix)
  rownames(percentage_matrix) <- rownames(nutrient_matrix)

  for (i in seq_len(nrow(nutrient_matrix))) {
    formula <- rownames(nutrient_matrix)[i]
    salt_mass <- compute_molar_mass(formula)
    if (is.na(salt_mass)) next

    for (j in seq_len(ncol(nutrient_matrix))) {
      nutrient <- colnames(nutrient_matrix)[j]
      element <- nutrient_element_map[[nutrient]]
      if (is.null(element)) next
      element_mass <- .molar_mass[[element]]
      if (is.na(element_mass)) next

      mmol <- nutrient_matrix[i, j]
      total_element_mass <- mmol * element_mass
      percentage_matrix[i, j] <-total_element_mass / salt_mass
    }
  }

  return(percentage_matrix)
}

