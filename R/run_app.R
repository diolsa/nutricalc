#' Launch the NutriCalc Shiny App
#' @export
run_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) stop("Please install 'shiny'.")
  app_dir <- system.file("app", package = "nutricalc")
  if (app_dir == "") stop("App directory not found inside the package.")
  shiny::runApp(app_dir, loadSupport = FALSE)
}


