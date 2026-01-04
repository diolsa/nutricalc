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


#' Remove negative zeros introduced by rounding
#'
#' Ensures rounded values like -0.00 are displayed as 0.
#'
#' @param x Numeric vector or matrix.
#'
#' @export
strip_negative_zero <- function(x) {
  x[x == 0] <- 0
  x
}

#' @export
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Normalize nutrient name keys to match canonical forms
#'
#' @param x Character vector of nutrient names.
#' @export
normalize_nutrient_names <- function(x) {
  x <- gsub("[–-]", "_", x)
  x <- gsub("\\s+", "", x)
  x
}

#' Coerce nutrient data into a named numeric vector
#'
#' @param x Named numeric vector or data.frame with Nutrient/Achieved columns.
#' @export
to_named_numeric <- function(x) {
  if (is.data.frame(x)) {
    if (!all(c("Nutrient", "Achieved") %in% names(x))) {
      stop("Data frame must have columns 'Nutrient' and 'Achieved'.", call. = FALSE)
    }
    nm <- as.character(x$Nutrient)
    vals <- x$Achieved
    names(vals) <- nm
    x <- vals
  }

  if (is.null(names(x))) {
    stop("achieved must be a named numeric vector or a data.frame with Nutrient/Achieved columns.", call. = FALSE)
  }

  vals <- x
  if (is.character(vals)) {
    vals[vals == ""] <- NA
    vals <- suppressWarnings(as.numeric(vals))
  }
  if (!is.numeric(vals)) {
    vals <- suppressWarnings(as.numeric(vals))
  }

  names(vals) <- normalize_nutrient_names(names(vals))

  na_idx <- is.na(vals)
  if (any(na_idx)) {
    bad <- names(vals)[na_idx]
    bad <- unique(bad[!is.na(bad) & nzchar(bad)])
    stop(sprintf("achieved has NA values for: %s", paste(bad, collapse = ", ")), call. = FALSE)
  }

  vals
}

#' Normalize unit labels to canonical form
#'
#' @param u Unit string.
#' @param canonical_unit The canonical unit to default to.
#' @export
normalize_unit <- function(u, canonical_unit = "mmol/L") {
  if (is.null(u) || is.na(u) || !nzchar(u)) return(canonical_unit)
  u <- trimws(u)
  u <- gsub("l-1", "L", u, ignore.case = TRUE)
  u <- gsub("/l", "/L", u, ignore.case = TRUE)
  u <- gsub("^umol", "µmol", u, ignore.case = TRUE)
  u
}

#' Convert from user unit to canonical unit
#'
#' @param vals Numeric vector of values.
#' @param unit_in Unit string.
#' @param canonical_unit Canonical unit.
#' @export
to_canonical_from_unit <- function(vals, unit_in, canonical_unit = "mmol/L") {
  unit_in <- normalize_unit(unit_in, canonical_unit = canonical_unit)

  if (unit_in == canonical_unit) {
    return(vals)
  } else if (unit_in == "mg/L") {
    if (!exists("convert_units", mode = "function")) {
      stop("convert_units() not found (check R/convert_unit.R).", call. = FALSE)
    }
    return(convert_units(vals, to = canonical_unit))
  } else if (unit_in == "µmol/L") {
    return(vals / 1000)
  } else {
    stop("Unsupported input unit: ", unit_in, call. = FALSE)
  }
}

#' Convert from canonical unit to output unit
#'
#' @param vals Numeric vector of values.
#' @param unit_out Unit string.
#' @param canonical_unit Canonical unit.
#' @export
from_canonical_to_unit <- function(vals, unit_out, canonical_unit = "mmol/L") {
  unit_out <- normalize_unit(unit_out, canonical_unit = canonical_unit)

  if (unit_out == canonical_unit) {
    return(vals)
  } else if (unit_out == "mg/L") {
    if (!exists("convert_units", mode = "function")) {
      stop("convert_units() not found (check R/convert_unit.R).", call. = FALSE)
    }
    return(convert_units(vals, to = "mg/L"))
  } else if (unit_out == "µmol/L") {
    return(vals * 1000)
  } else {
    stop("Unsupported output unit: ", unit_out, call. = FALSE)
  }
}
