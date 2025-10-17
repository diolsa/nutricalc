#' Optimize Nutrient Solution Using NNLS
#'
#' Solves for optimal fertilizer amounts to meet target nutrient concentrations.
#'
#' @param nutrient_matrix A matrix of nutrient contents [nutrient x compound].
#' @param target A named numeric vector of target concentrations (mol/L).
#' @param importance A named numeric vector (-2 to +2) specifying nutrient importance.
#'
#' @return An object of class 'nutrient_optimization_result'.
#' @export
optimize_nutrients <- function(nutrient_matrix, target, importance) {
  rescale_weights <- function(x, relaxed_weight = 0.1, strict_weight = 100) {
    relaxed_weight * (strict_weight / relaxed_weight) ^ x
  }

  importance <- pmin(pmax(importance, -2), 2)
  weights <- rescale_weights(importance)

   safe_target <- target
  safe_target[safe_target == 0] <- -1

  #To handle nutrients with a target of zero, we assign a normalization factor of -1.
  #This preserves numerical stability during matrix scaling and leverages the NNLS algorithm's inherent
  #non-negativity constraint to minimize these nutrients' contribution. Empirically, this method
  #discourages delivery of unneeded nutrients without requiring inequality constraints or zero masking.

  A_sub <- t(nutrient_matrix[, names(target)]) / safe_target



#
  W <- diag(sqrt(weights))
  A_weighted <- W %*% A_sub
  target_weighted <- W %*% rep(1, length(target))

  fit <- nnls::nnls(A_weighted, target_weighted)
  amounts <- fit$x
  names(amounts) <- colnames(A_sub)

  achieved <- t(nutrient_matrix[, names(target)]) %*% amounts
  names(achieved) <- names(target)

  abs_error <- achieved - target
  percent_error <- abs_error / target * 100
  percent_error[is.nan(percent_error) | is.infinite(percent_error)] <- NA
  squared_error <- sum(abs_error^2)

  structure(list(
    amounts = amounts,
    achieved = achieved,
    target = target,
    abs_error = abs_error,
    percent_error = percent_error,
    squared_error = squared_error
  ), class = "nutrient_optimization_result")
}


#' @export
print.nutrient_optimization_result <- function(x, ...) {
  cat("🧪 Fertilizer amounts:\n")

  # Only include compounds with amount > 0
  formulas <- names(x$amounts)
  amounts <- x$amounts[x$amounts > 0]
  formulas <- names(amounts)

  molar_masses <- sapply(formulas, compute_molar_mass)
  mg_l <- amounts * molar_masses

  df <- data.frame(
    Formula     = formulas,
    "mmol l⁻¹"      = round(amounts, 2),
    "g mol⁻¹"      = round(molar_masses, 2),
    "  mg l⁻¹"      = round(mg_l, 2),
    row.names   = NULL,
    check.names = FALSE
  )


  print(df)

  cat("\n🎯 Nutrient delivery vs. target:\n")

  nutrients_df <- data.frame(
    Nutrient      = names(x$target),
    Target        = round(x$target, 2),
    Achieved      = round(as.vector(x$achieved), 2),
    Abs_Error     = round(as.vector(x$abs_error), 2),
    Percent_Error = round(as.vector(x$percent_error), 2),
    check.names = FALSE
  )

  print(nutrients_df, row.names = FALSE)


  cat("\n🧮 Total squared error:", round(x$squared_error, 5), "\n")
}
