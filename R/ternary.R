#' Steiner-style ternary plot (Target vs Achieved) with constant-component drops
#'
#' @param result   List with numeric named vectors `target` and `achieved`.
#' @param keys     Character(3) giving axis order, e.g. c("K","Ca","Mg").
#' @param title    Plot title.
#' @param show_legend Logical, show legend (default TRUE).
#' @param step_percent Integer in [1, 50]; major grid step as percent. Default 10.
#' @param lab_cex,axis_cex,padding Plot layout controls.
#' @param col_target,col_achieved Colors.
#' @param pch_target,pch_achieved Point symbols.
#' @param cex_points Point size.
#' @param show_link Logical, draw connector between Target and Achieved.
#' @param lty_link,lwd_link Style for connector if shown.
#' @param legend_pos Legend position.
#' @param show_axis_drops Logical, draw constant-component drops from Achieved to sides.
#' @param drop_col,drop_lwd,drop_lty Style for drop lines.
#' @param show_ratio Logical, include "(A, B; C)" values in legend (default TRUE).
#' @param ratio_format "percent" (default) or "proportion".
#' @param ratio_digits Digits for numeric formatting.
#' @return Invisibly returns NULL (draws the plot).
#' @export
plot_ternary <- function(
    result,
    keys,
    title            = "Ternary (ratios)",
    show_legend      = TRUE,
    step_percent     = 10,
    lab_cex          = 0.95,
    axis_cex         = 0.85,
    padding          = 0.08,
    col_target       = "black",
    col_achieved     = "firebrick",
    pch_target       = 16,
    pch_achieved     = 17,
    cex_points       = 1.25,
    show_link        = FALSE,
    lty_link         = 2,
    lwd_link         = 1,
    legend_pos       = "topright",
    show_axis_drops  = TRUE,
    drop_col         = "grey35",
    drop_lwd         = 1,
    drop_lty         = 1,
    show_ratio       = TRUE,
    ratio_format     = "percent",
    ratio_digits     = 1
) {
  # ---- Argument checks -----------------------------------------------------
  if (!is.list(result) || is.null(result$target) || is.null(result$achieved)) {
    stop("`result` must be a list with elements `target` and `achieved`.")
  }
  if (!is.character(keys) || length(keys) != 3L) {
    stop("`keys` must be a character vector of length 3.")
  }

  key_pos_target   <- match(keys, names(result$target))
  key_pos_achieved <- match(keys, names(result$achieved))
  if (anyNA(key_pos_target)) {
    stop("Missing key(s) in `result$target`: ",
         paste(keys[is.na(key_pos_target)], collapse = ", "), ".")
  }
  if (anyNA(key_pos_achieved)) {
    stop("Missing key(s) in `result$achieved`: ",
         paste(keys[is.na(key_pos_achieved)], collapse = ", "), ".")
  }

  t <- as.numeric(result$target[key_pos_target]);     names(t) <- keys
  a <- as.numeric(result$achieved[key_pos_achieved]); names(a) <- keys

  if (any(!is.finite(t)) || any(!is.finite(a)) || any(t < 0) || any(a < 0)) {
    graphics::plot.new(); graphics::title(main = paste(title, "(invalid values)"))
    return(invisible(NULL))
  }
  st <- sum(t); sa <- sum(a)
  if (st <= 0 || sa <= 0) {
    graphics::plot.new(); graphics::title(main = paste(title, "(no data)"))
    return(invisible(NULL))
  }

  # Normalize to proportions
  t <- t / st
  a <- a / sa

  # ---- Grid/ticks configuration -------------------------------------------
  step_percent <- max(1L, min(50L, as.integer(step_percent)))
  grid_lines   <- max(1L, round(100L / step_percent))

  # Axis labels
  alab <- keys[1]; blab <- keys[2]; clab <- keys[3]

  # ---- Draw plot -----------------------------------------------------------
  Ternary::TernaryPlot(
    alab            = alab, blab = blab, clab = clab,
    main            = title,
    lab.cex         = lab_cex,
    axis.cex        = axis_cex,
    padding         = padding,
    grid.lines       = grid_lines,
    grid.minor.lines = 0,
    grid.col         = "grey70",
    grid.lty         = "dotted",
    axis.labels      = TRUE,
    axis.tick        = TRUE,
    ticks.length     = 0.025
  )

  # Points
  Ternary::TernaryPoints(rbind(t), pch = pch_target,   cex = cex_points, col = col_target)
  Ternary::TernaryPoints(rbind(a), pch = pch_achieved, cex = cex_points, col = col_achieved)

  # Optional connector
  if (isTRUE(show_link)) {
    Ternary::TernaryLines(rbind(t, a), lty = lty_link, lwd = lwd_link, col = col_achieved)
  }

  # ---- Constant-component drops from Achieved to chosen sides --------------
  if (isTRUE(show_axis_drops)) {
    A_val <- unname(a[keys[1L]])
    B_val <- unname(a[keys[2L]])
    C_val <- unname(a[keys[3L]])

    clip01 <- function(x) pmin(1, pmax(0, x))
    renorm <- function(v) { s <- sum(v); if (s <= .Machine$double.eps) v else v / s }

    p_xy  <- Ternary::TernaryToXY(c(A_val, B_val, C_val))
    qA_xy <- Ternary::TernaryToXY(renorm(clip01(c(A_val,      0, 1 - A_val))))
    qB_xy <- Ternary::TernaryToXY(renorm(clip01(c(1 - B_val,  B_val,        0))))
    qC_xy <- Ternary::TernaryToXY(renorm(clip01(c(0,      1 - C_val,   C_val))))

    graphics::segments(p_xy[1], p_xy[2], qA_xy[1], qA_xy[2],
                       col = drop_col, lwd = drop_lwd, lty = drop_lty)
    graphics::segments(p_xy[1], p_xy[2], qB_xy[1], qB_xy[2],
                       col = drop_col, lwd = drop_lwd, lty = drop_lty)
    graphics::segments(p_xy[1], p_xy[2], qC_xy[1], qC_xy[2],
                       col = drop_col, lwd = drop_lwd, lty = drop_lty)
  }

  # ---- Legend with ratio values -------------------------------------------
  if (isTRUE(show_legend)) {
    format_comp <- function(x) {
      if (ratio_format == "percent") {
        paste0(round(100 * x, ratio_digits), "%")
      } else {
        formatC(x, format = "f", digits = ratio_digits)
      }
    }

    ratio_str <- function(vec) {
      A <- unname(vec[keys[1L]]); B <- unname(vec[keys[2L]]); C <- unname(vec[keys[3L]])
      sprintf("(%s, %s; %s)", format_comp(A), format_comp(B), format_comp(C))
    }

    leg_labels <- if (isTRUE(show_ratio)) {
      c(
        paste0("Target   ", ratio_str(t)),
        paste0("Achieved ", ratio_str(a))
      )
    } else {
      c("Target", "Achieved")
    }

    graphics::legend(
      legend_pos,
      legend = leg_labels,
      pch    = c(pch_target, pch_achieved),
      col    = c(col_target, col_achieved),
      bty    = "n",
      cex    = 0.9
    )
  }

  invisible(NULL)
}
