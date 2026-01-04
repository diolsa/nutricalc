#' Launch the NutriCalc Shiny App
#' @param ... Arguments passed to [shiny::runApp()].
#' @export
run_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install 'shiny'.", call. = FALSE)
  }

  app_dir <- system.file("app", package = "nutricalc")
  if (app_dir == "") {
    stop("App directory not found inside the package.", call. = FALSE)
  }

  shiny::runApp(app_dir, ...)
}
