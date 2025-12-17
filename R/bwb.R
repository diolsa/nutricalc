#' Fetch BWB "In aller Tiefe" Mittelwerte by postal code
#'
#' Retrieves Berliner Wasserbetriebe (BWB) analysis data for a given German
#' postal code and returns the Mittelwert values (first available entry per
#' parameter) for selected parameters as mmol/L.
#'
#' @param plz Character or numeric vector of length one with a five digit
#'   German postal code.
#'
#' @return A named numeric vector of length 16 in mmol/L with names
#'   `c("NO3_N","NH4_N","P","K","Ca","Mg","S","Na","Cl",
#'   "Fe","Mn","Zn","B","Cu","Mo","Si")`, limited to the decimal precision
#'   provided by the source values.
#' @export
#'


fetch_bwb_mittelwert <- function(plz) {
  if (length(plz) != 1L) {
    stop("`plz` must be a single value.")
  }
  plz_num <- suppressWarnings(as.integer(plz))
  if (is.na(plz_num)) {
    stop("`plz` must be coercible to an integer postal code.")
  }
  plz_chr <- sprintf("%05d", plz_num)
  if (!grepl("^\\d{5}$", plz_chr)) {
    stop("`plz` must be a five digit German postal code.")
  }

  url <- sprintf("https://www.bwb.de/de/analysedaten-nach-postleitzahlen.php?PLZ=%s", plz_chr)
  page <- tryCatch(read_html(url), error = function(e) {
    stop("Failed to retrieve data from BWB: ", conditionMessage(e), call. = FALSE)
  })

  tables <- xml_find_all(page, ".//table")
  parsed_tables <- lapply(tables, parse_html_table)

  table_matches <- vapply(parsed_tables, function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(FALSE)
    cols <- tolower(trimws(names(tbl)))
    "parameter" %in% cols && "mittelwert" %in% cols
  }, logical(1))

  relevant <- parsed_tables[table_matches]
  target_names <- c("NO3_N","NH4_N","P","K","Ca","Mg","S","Na","Cl",
                    "Fe","Mn","Zn","B","Cu","Mo","Si")
  empty_result <- stats::setNames(rep(NA_real_, length(target_names)), target_names)

  if (!length(relevant)) {
    warning("No table with columns 'Parameter' and 'Mittelwert' was found for PLZ ", plz_chr)
    return(empty_result)
  }

  extracted <- lapply(relevant, function(tbl) {
    cols <- tolower(trimws(names(tbl)))
    param_col <- names(tbl)[match("parameter", cols)]
    value_col <- names(tbl)[match("mittelwert", cols)]
    unit_col  <- names(tbl)[match("einheit", cols)]
    data.frame(
      Parameter = tbl[[param_col]],
      Mittelwert = tbl[[value_col]],
      Einheit = if (!is.na(unit_col)) tbl[[unit_col]] else NA_character_,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  combined <- do.call(rbind, extracted)
  combined$Parameter <- trimws(combined$Parameter)
  combined$Mittelwert_num <- vapply(combined$Mittelwert, parse_mittelwert, numeric(1))
  combined$Mittelwert_digits <- vapply(combined$Mittelwert, count_decimal_places, integer(1))
  combined$Einheit_clean <- vapply(combined$Einheit, normalize_unit, character(1))
  combined$Target <- vapply(combined$Parameter, map_parameter_to_nutrient, character(1))
  combined <- combined[nzchar(combined$Target), , drop = FALSE]

  mmol_values <- empty_result
  for (nm in target_names) {
    rows <- combined[combined$Target == nm, , drop = FALSE]
    if (!nrow(rows)) next

    mmol <- mapply(
      function(val, unit) convert_mittelwert(val, unit, nm),
      rows$Mittelwert_num,
      rows$Einheit_clean
    )

    first_valid <- which(!is.na(mmol))[1]
    if (!is.na(first_valid)) {
      mmol_values[nm] <- mmol[first_valid]
      max_digits <- suppressWarnings(max(rows$Mittelwert_digits, na.rm = TRUE))
      if (is.infinite(max_digits)) max_digits <- NA_integer_
      if (!is.na(max_digits)) {
        mmol_values[nm] <- round(mmol_values[nm], max_digits)
      }
    } else {
      mmol_values[nm] <- NA_real_
    }
  }

  strip_trailing_zeros_numeric(mmol_values)
}

parse_html_table <- function(tbl) {
  rows <- xml_find_all(tbl, ".//tr")
  if (!length(rows)) return(NULL)

  rows_info <- lapply(rows, function(r) {
    list(
      has_header = length(xml_find_all(r, "./th")) > 0,
      cells = trimws(xml_text(xml_find_all(r, "./th|./td")))
    )
  })

  ncol <- max(vapply(rows_info, function(x) length(x$cells), integer(1)))
  if (ncol == 0) return(NULL)

  padded <- lapply(rows_info, function(x) c(x$cells, rep("", ncol - length(x$cells))))
  mat <- do.call(rbind, padded)

  header_row <- which(vapply(rows_info, function(x) x$has_header, logical(1)))
  header_idx <- if (length(header_row)) header_row[1] else 1

  header <- mat[header_idx, , drop = FALSE]
  body <- mat[-seq_len(header_idx), , drop = FALSE]

  colnames(body) <- make.unique(ifelse(nchar(header[1, ]) > 0, header[1, ], paste0("V", seq_len(ncol))))
  if (!nrow(body)) return(NULL)
  data.frame(body, stringsAsFactors = FALSE, check.names = FALSE)
}

parse_mittelwert <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr <- sub("^<\\s*", "", x_chr)
  x_chr <- gsub(",", ".", x_chr, fixed = TRUE)
  val <- suppressWarnings(as.numeric(x_chr))
  if (is.na(val)) NA_real_ else val
}

count_decimal_places <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr <- sub("^<\\s*", "", x_chr)
  x_chr <- gsub(",", ".", x_chr, fixed = TRUE)
  if (!grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", x_chr)) return(NA_integer_)
  if (!grepl("\\.", x_chr)) return(0L)
  fractional <- sub("^[^\\.]*\\.([^eE]*).*", "\\1", x_chr)
  fractional <- sub("[eE].*$", "", fractional)
  nchar(fractional)
}
map_parameter_to_nutrient <- function(param) {
  clean <- trimws(to_lower_ascii(param))
  if (grepl("nitrat", clean)) return("NO3_N")
  if (grepl("ammonium|^nh4", clean)) return("NH4_N")
  if (grepl("^phosphor", clean)) return("P")
  if (grepl("^kalium", clean)) return("K")
  if (grepl("^calcium", clean)) return("Ca")
  if (grepl("^magnesium", clean)) return("Mg")
  if (grepl("^sulfat", clean)) return("S")
  if (grepl("^natrium", clean)) return("Na")
  if (grepl("^chlorid", clean)) return("Cl")
  if (grepl("^eisen", clean)) return("Fe")
  if (grepl("^mangan", clean)) return("Mn")
  if (grepl("^zink", clean)) return("Zn")
  if (grepl("^bor", clean)) return("B")
  if (grepl("^kupfer", clean)) return("Cu")
  if (grepl("^molybd", clean)) return("Mo")
  if (grepl("^silicium", clean)) return("Si")
  ""
}

normalize_unit <- function(unit) {
  u <- tolower(trimws(gsub("\u00a0", "", as.character(unit), fixed = TRUE)))
  u <- gsub(",", ".", u, fixed = TRUE)
  if (!nzchar(u)) return(NA_character_)

  u <- sub("^ug/l$", "\u00b5g/l", u)  # normalize plain 'ug/l' to 'µg/l'
  switch(u,
         "mmol/l" = "mmol/L",
         "mg/l" = "mg/L",
         "\u00b5g/l" = "µg/L",
         NA_character_)
}

convert_mittelwert <- function(value, unit, nutrient) {
  if (is.na(value) || is.na(unit)) return(NA_real_)

  if (identical(unit, "mmol/L")) return(value)

  if (identical(unit, "mg/L")) {
    return(convert_units(stats::setNames(value, nutrient), to = "mmol/L")[[1]])
  }

  if (identical(unit, "µg/L")) {
    mg_value <- value / 1000
    return(convert_units(stats::setNames(mg_value, nutrient), to = "mmol/L")[[1]])
  }

  NA_real_
}

to_lower_ascii <- function(x) {
  out <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
  ifelse(is.na(out), tolower(as.character(x)), out)
}

strip_trailing_zeros_numeric <- function(x) {
  formatted <- formatC(x, format = "fg", digits = 22, drop0trailing = TRUE)
  out <- suppressWarnings(as.numeric(formatted))
  names(out) <- names(x)
  class(out) <- c("bwb_mittelwert", class(out))
  out
}

print.bwb_mittelwert <- function(x, ...) {
  vals <- formatC(x, format = "fg", digits = 22, drop0trailing = TRUE)
  names(vals) <- names(x)
  print(noquote(vals), ...)
  invisible(x)
}
