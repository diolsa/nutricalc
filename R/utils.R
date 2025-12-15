#'Zero-out tiny floating point values
#'
#' Utility helper to clamp extremely small values to zero, avoiding noisy output.
#'
#' @param x Numeric vector or matrix.
#' @param tol Values with absolute magnitude below this tolerance are set to zero.
#'
#' @return The input with near-zero entries replaced by zeros.
#' @export
#' @examples
#' drop_tiny(c(1, 1e-10, -1e-12))
#' drop_tiny(matrix(c(0.5, 1e-12), nrow = 1))
drop_tiny <- function(x, tol = 1e-9) {
  x[abs(x) < tol] <- 0
  x
}
