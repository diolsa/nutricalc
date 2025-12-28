#' Optimize Nutrient Solution Using NNLS
#'
#' Solves for optimal fertilizer amounts to meet target nutrient concentrations.
#' Expects a compound-by-nutrient matrix: rows = salts/compounds, columns = nutrients.
#'
#' @param nutrient_matrix Matrix [compound x nutrient] with entries in mol of nutrient per mmol of compound
#'   (i.e., how many mmol of nutrient you get from 1 mmol of the compound).
#' @param target Named numeric vector of target concentrations (mmol/L).
#' @param importance Named numeric vector (-2 to +2) specifying nutrient importance (weights).
#'
#' @return An object of class 'nutrient_optimization_result' with elements:
#'   \itemize{
#'     \item amounts — named numeric (mmol/L) of each compound (same order as rownames of input)
#'     \item achieved — named numeric (mmol/L) delivered per nutrient
#'     \item target — named numeric (mmol/L) target per nutrient
#'     \item abs_error — achieved - target (mmol/L)
#'     \item percent_error — 100 * abs_error / target (NA when target == 0)
#'     \item squared_error — sum(abs_error^2)
#'     \item rel_squared_error — sum((abs_error/target)^2, na.rm = TRUE)
#'   }
#' @export
optimize_nutrients <- function(nutrient_matrix, target, importance) {
  # weight scaling: map [-2,2] to [relaxed_weight, strict_weight] on a smooth curve
  rescale_weights <- function(x, relaxed_weight = 0.1, strict_weight = 100) {
    relaxed_weight * (strict_weight / relaxed_weight) ^ x
  }

  # clamp importance range and compute weights
  importance <- pmin(pmax(importance, -2), 2)
  weights <- rescale_weights(importance)

  # Normalize rows using target, but avoid division-by-zero.
  # When target == 0, use 1 so RHS becomes 0 (target/row_scale), not a sign flip.
  row_scale <- target
  row_scale[row_scale == 0] <- 1

  # Build design matrix A_sub (nutrients x compounds), normalized by row_scale
  A_sub <- t(nutrient_matrix[, names(target), drop = FALSE]) / row_scale

  # Apply importance weights
  W <- diag(sqrt(weights[names(target)]), nrow = length(target))
  A_weighted <- W %*% A_sub
  target_weighted <- W %*% (target / row_scale)


  # NNLS fit
  fit <- nnls::nnls(A_weighted, target_weighted)
  amounts <- fit$x
  names(amounts) <- colnames(A_sub)  # compound names

  # Achieved nutrient delivery at the optimum (mmol/L)
  achieved <- t(nutrient_matrix[, names(target), drop = FALSE]) %*% amounts
  achieved <- as.vector(achieved)
  names(achieved) <- names(target)

  amounts_full <- numeric(nrow(nutrient_matrix))
  names(amounts_full) <- rownames(nutrient_matrix)
  amounts_full[names(amounts)] <- amounts
  achieved_full <- as.vector(t(nutrient_matrix) %*% amounts_full)
  names(achieved_full) <- colnames(nutrient_matrix)

  # Errors
  abs_error <- achieved - target
  percent_error <- abs_error / target * 100
  percent_error[is.nan(percent_error) | is.infinite(percent_error)] <- NA

  tol <- 1e-9
  zero_tgt <- target == 0
  percent_error[zero_tgt & abs(achieved) < tol] <- 0

  rel_error <- abs_error / target
  rel_error[is.nan(rel_error) | is.infinite(rel_error)] <- NA

  # Clamp tiny numerical noise
  amounts       <- drop_tiny(amounts)
  achieved      <- drop_tiny(achieved)
  abs_error     <- drop_tiny(abs_error)
  percent_error <- drop_tiny(percent_error)
  rel_error     <- drop_tiny(rel_error)

  structure(
    list(
      amounts = amounts,                 # mmol L^-1 per compound
      achieved = achieved,               # mmol L^-1 per nutrient
      achieved_full = achieved_full,     # mmol L^-1 per nutrient (all columns)
      target = target,                   # mmol L^-1 per nutrient
      abs_error = abs_error,             # mmol L^-1
      percent_error = percent_error,     # %
      squared_error = sum(abs_error^2),
      rel_squared_error = sum(rel_error^2, na.rm = TRUE)
    ),
    class = "nutrient_optimization_result"
  )
}

#' Pretty-print the optimization result
#'
#' Shows per-liter amounts; if `vol > 1`, also shows a dynamic column "g for <vol> L".
#' Uses `compute_molar_mass(formula)` (g/mol) when available; if not found, prints without mass columns.
#'
#' @param x A 'nutrient_optimization_result' object.
#' @param vol Total solution volume in liters (must be > 0). If > 1, a "g for <vol> L" column is added.
#' @param ... Unused.
#'
#'
#' @export
print.nutrient_optimization_result <- function(x, vol = 1, ...) {
  if (!is.numeric(vol) || length(vol) != 1 || is.na(vol) || vol <= 0) {
    stop("`vol` must be a single positive number.", call. = FALSE)
  }

  cat("🧪 Fertilizer amounts:\n")

  # Keep only positive amounts
  amounts <- x$amounts[x$amounts > 0]
  formulas <- names(amounts)

  has_mm <- exists("compute_molar_mass", mode = "function")

  if (length(amounts) == 0L) {
    print(data.frame(Message = "All amounts are zero.", check.names = FALSE))
  } else if (has_mm) {
    molar_masses <- sapply(formulas, compute_molar_mass)  # g mol⁻¹
    mg_l <- amounts * molar_masses                        # mg L⁻¹
    df <- data.frame(
      Formula    = formulas,
      "mmol l⁻¹" = round(amounts, 3),
      "g mol⁻¹"  = round(molar_masses, 2),
      "mg l⁻¹"   = round(mg_l, 2),
      row.names  = NULL,
      check.names = FALSE
    )

    # Only add total grams if vol > 1
    if (vol > 1) {
      g_total <- mg_l * vol / 1000

      # Format volume label scientifically: "g 40 l⁻¹"
      vol_label <- if (abs(vol - round(vol)) < 1e-9) as.integer(vol) else vol
      g_colname <- sprintf("g %s l⁻¹", vol_label)

      df[[g_colname]] <- round(g_total, 3)
    }

    print(df)
  } else {
    df <- data.frame(
      Formula    = formulas,
      "mmol l⁻¹" = round(amounts, 3),
      row.names  = NULL,
      check.names = FALSE
    )
    print(df)
    cat("\nℹ️ Tip: define compute_molar_mass(formula) to show mg L⁻¹ and gram totals.\n")
  }

  cat("\n🎯 Nutrient delivery vs. target (per liter):\n")

  nutrients_df <- data.frame(
    Nutrient      = names(x$target),
    Target        = strip_negative_zero(round(drop_tiny(as.vector(x$target)), 3)),
    Achieved      = strip_negative_zero(round(drop_tiny(as.vector(x$achieved)), 3)),
    Abs_Error     = strip_negative_zero(round(drop_tiny(as.vector(x$abs_error)), 3)),
    Percent_Error = strip_negative_zero(round((x$percent_error), 2)),
    check.names   = FALSE
  )
  print(nutrients_df, row.names = FALSE)

  cat("\n🧮 Total squared error:", round(x$squared_error, 6), "\n")
  cat("📊 Total squared error (relative):", round(x$rel_squared_error, 6), "\n")
  if (vol > 1) cat("🧴 Volume used for totals:", vol, "L\n")
}

#---------Second Solver------------

#' Two-stage nutrient optimization separating neutral salts from acids/bases
#'
#' Runs an initial NNLS optimization on neutral salts, then uses acids/bases to
#' fill remaining deficiencies (when they can affect the off-target nutrients).
#'
#' @param nutrient_matrix Matrix [compound x nutrient] with entries in mol of nutrient per mmol of compound.
#' @param target Named numeric vector of target concentrations (mmol/L).
#' @param importance Named numeric vector (-2 to +2) specifying nutrient importance (weights).
#' @param tol_abs Absolute tolerance (mmol/L) below which residuals are ignored for acid/base correction.
#' @param tol_rel Relative tolerance (fraction of target) below which residuals are ignored for acid/base correction.
#'
#' @return A 'nutrient_optimization_result' object.
#' @export
two_stage_optimize_nutrients <- function(
    nutrient_matrix,
    target,
    importance,
    tol_abs = 0.01,
    tol_rel = 0.01
) {
  rn <- rownames(nutrient_matrix)

  # Helper to build the final result list consistently across exit paths
  assemble_result <- function(amounts_salts, amounts_acid) {
    full_amounts <- numeric(nrow(nutrient_matrix))
    names(full_amounts) <- rn
    full_amounts[neutral_idx] <- amounts_salts
    full_amounts[acid_idx] <- amounts_acid

    A_full <- t(nutrient_matrix[, names(target), drop = FALSE])
    achieved <- as.vector(A_full %*% full_amounts)
    names(achieved) <- names(target)

    achieved_full <- as.vector(t(nutrient_matrix) %*% full_amounts)
    names(achieved_full) <- colnames(nutrient_matrix)

    abs_error <- achieved - target
    percent_error <- abs_error / target * 100
    percent_error[is.nan(percent_error) | !is.finite(percent_error)] <- NA

    rel_error <- abs_error / target
    rel_error[is.nan(rel_error) | !is.finite(rel_error)] <- NA

    tol <- 1e-9
    zero_tgt <- target == 0
    percent_error[zero_tgt & abs(achieved) < tol] <- 0

    full_amounts <- drop_tiny(full_amounts)
    achieved <- drop_tiny(achieved)
    abs_error <- drop_tiny(abs_error)
    percent_error <- drop_tiny(percent_error)

    res <- list(
      amounts = full_amounts,
      achieved = achieved,
      achieved_full = achieved_full,
      target = target,
      abs_error = abs_error,
      percent_error = percent_error,
      squared_error = sum(abs_error^2, na.rm = TRUE),
      rel_squared_error = sum(rel_error^2, na.rm = TRUE)
    )
    class(res) <- "nutrient_optimization_result"
    res
  }

  # ----- 1) Split neutrals vs acids/bases using salt_info flag -----
  is_acid_base <- salt_info$is_acid_base[match(rn, salt_info$salt)]
  is_acid_base[is.na(is_acid_base)] <- FALSE

  acid_idx <- which(is_acid_base)
  neutral_idx <- which(!is_acid_base)

  nm_neutral <- nutrient_matrix[neutral_idx, , drop = FALSE]
  nm_acid <- nutrient_matrix[acid_idx, , drop = FALSE]

  # If no neutral salts at all -> fall back to one-step solve with everything
  if (nrow(nm_neutral) == 0L) {
    return(
      optimize_nutrients(
        nutrient_matrix = nutrient_matrix,
        target = target,
        importance = importance
      )
    )
  }

  # ----- 2) Stage 1: salts-only optimization -----
  res_salts <- optimize_nutrients(
    nutrient_matrix = nm_neutral,
    target = target,
    importance = importance
  )

  achieved_salts <- res_salts$achieved
  residual <- target - achieved_salts # positive = missing

  # Zero-out tiny residuals when the percent error from stage 1 is below the
  # relative tolerance (expressed in %). This keeps stage 2 from nudging acids
  # or bases when we're already within the configured relative tolerance.
  percent_error_salts <- abs(res_salts$percent_error)
  percent_error_salts[is.na(percent_error_salts)] <- 0
  residual[percent_error_salts < (tol_rel * 100)] <- 0

  # If every nutrient is already within the relative tolerance, stop after
  # stage 1. This prevents minute acid/base additions when percent errors are
  # effectively zero across the board.
  if (all(percent_error_salts < (tol_rel * 100))) {
    return(assemble_result(res_salts$amounts, amounts_acid = numeric(length(acid_idx))))
  }

  # ----- 3) Which nutrients can acids/bases actually change? -----
  if (nrow(nm_acid) > 0L) {
    acid_sub <- nm_acid[, names(target), drop = FALSE]
    can_change <- colSums(acid_sub != 0) > 0
    fixable <- names(target)[can_change]
  } else {
    fixable <- character(0)
  }

  need_big <- residual > pmax(tol_abs, tol_rel * target, na.rm = TRUE)
  need_fix_fixable <- need_big & (names(target) %in% fixable)

  # ----- 4) If no fixable nutrient is off by much → skip acids/bases -----
  if (!any(need_fix_fixable)) {
    return(assemble_result(res_salts$amounts, amounts_acid = numeric(length(acid_idx))))
  }

  # ----- 5) Stage 2: acids/bases fill residuals (only for fixable nutrients) -----
  residual_target <- residual
  residual_target[residual_target < 0] <- 0
  residual_target[!names(residual_target) %in% fixable] <- 0

  res_acid <- optimize_nutrients(
    nutrient_matrix = nm_acid,
    target = residual_target,
    importance = importance
  )

  assemble_result(res_salts$amounts, res_acid$amounts)
}
