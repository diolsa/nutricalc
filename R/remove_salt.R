#' Remove a fertilizer from the nutrient matrix
#' @param name Name of the fertilizer (row name) to remove
#' @param nutrient_matrix The current matrix
#' @return Updated matrix
#' @export
remove_fertilizer <- function(name, nutrient_matrix) {
  if (!name %in% rownames(nutrient_matrix)) {
    warning("Fertilizer not found: ", name)
    return(nutrient_matrix)
  }
  nutrient_matrix <- nutrient_matrix[rownames(nutrient_matrix) != name, , drop = FALSE]
  return(nutrient_matrix)
}
