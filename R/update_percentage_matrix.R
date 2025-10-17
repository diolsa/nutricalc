update_percentage_matrix <- function() {
  percentage_matrix <- compute_percentage_matrix(nutrient_matrix)
  usethis::use_data(percentage_matrix, overwrite = TRUE)
  message("✅ percentage_matrix updated and saved.")
}
