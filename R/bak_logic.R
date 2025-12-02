#' Assign salts to A, B, and Micro tanks based on composition and solver result
#'
#' This helper takes the result from \code{optimize_nutrients()} and the
#' corresponding \code{nutrient_matrix} (compounds x nutrients) and returns a
#' BAK-style grouping:
#'
#' - First, salts are classified purely by composition:
#'   * Micro  = any micro nutrient present (e.g. Fe, Mn, Zn, B, Cu, Mo)
#'   * A      = contains Ca (and no micro)
#'   * B      = contains P or S (and no micro / Ca)
#'   * AB     = none of the above (can go to A or B)
#'
#' - Then the AB salts are assigned to A or B so that A and B have a similar
#'   overall "load" (sum of delivered mmol of selected nutrients).
#'
#' @param res A \code{nutrient_optimization_result} as returned by
#'   \code{optimize_nutrients()}.
#' @param nutrient_matrix Matrix [compound x nutrient] used in the optimization.
#'   Row names must match the salt names (formulas).
#' @param micro_nutrients Character vector of column names that count as
#'   micronutrients. Defaults to \code{c("Fe", "Mn", "Zn", "B", "Cu", "Mo")}.
#' @param calcium_nutrient Column name for calcium (default: \code{"Ca"}).
#' @param phosphorus_nutrient Column name for phosphorus (default: \code{"P"}).
#' @param sulfur_nutrient Column name for sulfur (default: \code{"S"}).
#' @param balance_nutrients Character vector of nutrient columns used to define
#'   the "load" when splitting AB salts between A and B. Defaults to
#'   \code{c("NO3_N","NH4_N","P","K","Ca","Mg","S","Na")}.
#' @param drop_zero Logical, if \code{TRUE} (default), remove salts with
#'   effectively zero mmol/L from the returned \code{table} and \code{by_tank}.
#'   The \code{assignments} vector always includes all salts.
#' @param tol Numeric tolerance for deciding what counts as "zero" mmol/L.
#'
#' @return A list with:
#'   \item{assignments}{Named character vector, salt -> tank ("A","B","Micro").}
#'   \item{table}{Data frame with columns \code{salt}, \code{tank}, \code{mmol_l}
#'                (filtered to non-zero if \code{drop_zero = TRUE}).}
#'   \item{by_tank}{Named list of data frames (A, B, Micro, possibly others),
#'                  each subset of \code{table}.}
#'
assign_salts_bak <- function(res,
                             nutrient_matrix,
                             micro_nutrients     = c("Fe", "Mn", "Zn", "B", "Cu", "Mo", "Si"),
                             calcium_nutrient    = "Ca",
                             phosphorus_nutrient = "P",
                             sulfur_nutrient     = "S",
                             balance_nutrients   = c("NO3_N", "NH4_N", "P", "K", "Ca", "Mg", "S", "Na"),
                             drop_zero           = TRUE,
                             tol                 = 0) {

  # --- basic checks ---------------------------------------------------------
  if (!inherits(res, "nutrient_optimization_result")) {
    stop("`res` must be a 'nutrient_optimization_result' from optimize_nutrients().", call. = FALSE)
  }

  if (is.null(rownames(nutrient_matrix))) {
    stop("`nutrient_matrix` must have row names (salt formulas).", call. = FALSE)
  }

  amounts <- res$amounts
  if (is.null(amounts) || !is.numeric(amounts) || !length(amounts)) {
    stop("`res$amounts` is empty or not numeric.", call. = FALSE)
  }

  if (is.null(names(amounts)) || any(!nzchar(names(amounts)))) {
    stop("`res$amounts` must be a named vector with salt names.", call. = FALSE)
  }

  # Subset nutrient_matrix to salts appearing in the result
  salts <- names(amounts)
  if (!all(salts %in% rownames(nutrient_matrix))) {
    missing_rows <- setdiff(salts, rownames(nutrient_matrix))
    stop("The following salts from `res$amounts` are missing in `nutrient_matrix`: ",
         paste(missing_rows, collapse = ", "), call. = FALSE)
  }
  nm_sub <- nutrient_matrix[salts, , drop = FALSE]

  # --- 1) initial classification based on composition -----------------------

  # Helper to safely extract a column, defaulting to 0 if missing
  get_col <- function(col) {
    if (!col %in% colnames(nm_sub)) {
      return(rep(0, nrow(nm_sub)))
    }
    nm_sub[, col]
  }

  # Micro: any micro nutrient present
  micro_cols <- intersect(micro_nutrients, colnames(nm_sub))
  if (length(micro_cols)) {
    micro_present <- rowSums(nm_sub[, micro_cols, drop = FALSE] > 0) > 0
  } else {
    micro_present <- rep(FALSE, nrow(nm_sub))
  }

  ca_present <- get_col(calcium_nutrient)    > 0
  p_present  <- get_col(phosphorus_nutrient) > 0
  s_present  <- get_col(sulfur_nutrient)     > 0

  tank <- rep(NA_character_, length(salts))
  names(tank) <- salts

  # Rule priority:
  # 1) any micro -> Micro
  tank[micro_present] <- "Micro"

  # 2) Ca (but not already Micro) -> A
  tank[!micro_present & ca_present] <- "A"

  # 3) P or S (but not Micro or A) -> B
  tank[!micro_present & !ca_present & (p_present | s_present)] <- "B"

  # 4) everything else -> AB
  tank[is.na(tank)] <- "AB"

  # --- 2) compute "load" for each salt to balance AB between A and B --------

  bal_cols <- intersect(balance_nutrients, colnames(nm_sub))

  if (length(bal_cols)) {
    # For each salt, load = sum( mmol_salt * stoich_salt,n ) over selected nutrients
    salt_load <- vapply(
      X = salts,
      FUN = function(s) {
        sum(amounts[[s]] * nm_sub[s, bal_cols])
      },
      FUN.VALUE = numeric(1)
    )
  } else {
    # Fallback: use total mmol of salt as load
    salt_load <- as.numeric(amounts)
    names(salt_load) <- salts
  }

  # Fixed groups (before AB balancing)
  is_micro <- tank == "Micro"
  is_a_fix <- tank == "A"
  is_b_fix <- tank == "B"
  is_ab    <- tank == "AB"

  load_a <- sum(salt_load[is_a_fix], na.rm = TRUE)
  load_b <- sum(salt_load[is_b_fix], na.rm = TRUE)

  # --- 3) assign AB salts greedily to balance A/B load ----------------------

  ab_salts <- salts[is_ab]
  if (length(ab_salts)) {
    # Assign heaviest AB salts first
    ab_salts <- ab_salts[order(salt_load[ab_salts], decreasing = TRUE)]

    for (s in ab_salts) {
      # Choose tank with currently lower load
      if (load_a <= load_b) {
        tank[s] <- "A"
        load_a  <- load_a + salt_load[[s]]
      } else {
        tank[s] <- "B"
        load_b  <- load_b + salt_load[[s]]
      }
    }
  }

  # Ensure Micro stays Micro
  tank[is_micro] <- "Micro"

  # --- 4) build result objects ----------------------------------------------

  # Full table first (all salts)
  table <- data.frame(
    salt   = salts,
    tank   = unname(tank),
    mmol_l = as.numeric(amounts),
    stringsAsFactors = FALSE
  )

  # Optionally drop zero mmol_l rows from table/by_tank
  if (isTRUE(drop_zero)) {
    keep <- abs(table$mmol_l) > tol
    table <- table[keep, , drop = FALSE]
  }

  by_tank <- split(table, table$tank)

  list(
    assignments = tank,   # named vector: salt -> "A"/"B"/"Micro"
    table       = table,  # filtered if drop_zero = TRUE
    by_tank     = by_tank # list(A = ..., B = ..., Micro = ...)
  )
}
