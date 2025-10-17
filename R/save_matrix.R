#' Save the nutrient matrix
#' @param nutrient_matrix The matrix to save
#' @param path File path to save (e.g. "data/nutrient_matrix.rda")
#' @export
save_nutrient_matrix <- function(nutrient_matrix, path = "data/nutrient_matrix.rda") {
  save(nutrient_matrix, file = path)
}
