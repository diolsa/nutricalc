#' Compute molar mass of a chemical formula
#'
#' Supports formulas like "NaCl", "Ca(NO3)2", "CuSO4·5H2O", "MgN2O6·6H2O"
#' @param formula Chemical formula string
#' @return Molar mass in g/mol
#' @export
compute_molar_mass <- function(formula) {
  element_counts <- parse_formula(formula)
  unknown <- setdiff(names(element_counts), names(.molar_mass))
  if (length(unknown)) {
    warning("Missing elements in molar mass table: ", paste(unknown, collapse = ", "))
    return(NA)
  }
  sum(element_counts * .molar_mass[names(element_counts)])
}

# Internal: element molar mass vector
.molar_mass <- stats::setNames(element_molar_mass_df$MolarMass_g_per_mol, element_molar_mass_df$Element)

# Internal: sanitize inputs like "CuSO4 5H2O" -> "CuSO4\u00b75H2O"
sanitize_formula_input <- function(formula) {
  formula <- gsub("\\s+", " ", formula)
  formula <- gsub("([A-Za-z0-9\\)]+)\\s+([0-9]+H2O)", "\\1\u00b7\\2", formula)
  formula <- gsub("\\s*\u00b7\\s*", "\u00b7", formula)
  return(formula)
}

# Internal: parse basic formulas like H2O or MgCl2
parse_simple_formula <- function(formula) {
  matches <- stringr::str_match_all(formula, "([A-Z][a-z]?)([0-9]*)")[[1]]
  elements <- matches[, 2]
  counts <- as.numeric(matches[, 3])
  counts[is.na(counts)] <- 1
  summed <- tapply(counts, elements, sum)
  stats::setNames(as.numeric(summed), names(summed))
}

# Internal: expand brackets like (NO3)2 -> N2O6
parse_recursive <- function(formula) {
  while (grepl("\\([A-Za-z0-9]+\\)[0-9]*", formula)) {
    formula <- stringr::str_replace_all(
      formula,
      "\\(([^\\(\\)]+)\\)([0-9]*)",
      function(m) {
        match <- stringr::str_match(m, "\\(([^\\(\\)]+)\\)([0-9]*)")
        inner <- match[2]
        mult <- as.numeric(match[3])
        if (is.na(mult)) mult <- 1
        parsed <- parse_simple_formula(inner)
        paste0(
          mapply(function(el, count) paste0(el, count * mult),
                 names(parsed), parsed),
          collapse = ""
        )
      }
    )
  }
  parse_simple_formula(formula)
}

# Internal: handles full formula and hydration (e.g. CuSO4\u00b75H2O)
parse_formula <- function(formula) {
  formula <- sanitize_formula_input(formula)
  formula <- gsub("[\u00b7\u2219.]", "+", formula)
  parts <- unlist(strsplit(formula, "\\+"))

  total_counts <- list()

  for (part in parts) {
    match <- regexec("^([0-9]+)?(.*)$", part)
    res <- regmatches(part, match)[[1]]
    mult <- ifelse(length(res) >= 2 && nzchar(res[2]), as.numeric(res[2]), 1)
    if (is.na(mult)) mult <- 1
    core <- res[3]

    parsed_core <- parse_recursive(core)

    # iterate by position to respect duplicates
    for (i in seq_along(parsed_core)) {
      el <- names(parsed_core)[i]
      val <- unname(parsed_core[i])
      total_counts[[el]] <- sum(c(total_counts[[el]], mult * val), na.rm = TRUE)
    }
  }

  unlist(total_counts)
}
