# Nutrient → element mapping (keep this once in your package)
nutrient_element_map <- list(
  "NO3_N" = "N", "NH4_N" = "N", "P" = "P", "K" = "K", "Ca" = "Ca",
  "Mg" = "Mg", "S" = "S", "Na" = "Na", "Cl" = "Cl", "Fe" = "Fe",
  "Mn" = "Mn", "Zn" = "Zn", "B" = "B", "Cu" = "Cu", "Mo" = "Mo", "Si" = "Si"
)

# ---- Core converter ----
# Works for a single element (use `element=`) OR a whole recipe (named vector).
# to = "mg/L" or "mmol/L"
convert_units <- function(x, element = NULL, to = c("mg/L","mmol/L")) {
  to <- match.arg(to)

  # convert one number given an element symbol
  convert_one <- function(value, element, to) {
    M <- element_molar_mass_df$MolarMass_g_per_mol[
      match(element, element_molar_mass_df$Element)
    ]
    if (is.na(M)) stop("Element not found: ", element)
    if (to == "mg/L")  return(value * M)   # mmol/L → mg/L
    if (to == "mmol/L") return(value / M)  # mg/L   → mmol/L
  }

  # Recipe vector (named)
  if (!is.null(names(x))) {
    out <- x
    for (nutrient in names(x)) {
      element_sym <- nutrient_element_map[[nutrient]]
      if (is.null(element_sym)) next
      out[nutrient] <- convert_one(x[nutrient], element_sym, to)
    }
    return(out)
  }

  # Single number
  if (!is.null(element)) return(convert_one(x, element, to))

  stop("If x is a scalar, please provide the `element` argument.")
}

# ---- Pretty summary table ----
# `unit` tells me what unit your input recipe is in.
# Returns a data.frame with both mmol/L and mg/L.
recipe_summary <- function(recipe, unit = c("mmol/L","mg/L")) {
  unit <- match.arg(unit)
  if (is.null(names(recipe)))
    stop("`recipe` must be a named vector (e.g., names like NO3_N, P, K, Ca, ...).")

  nutrients <- names(recipe)
  elements  <- vapply(nutrients, function(n) nutrient_element_map[[n]] %||% NA_character_, character(1))

  # lookup molar masses aligned with elements
  M <- element_molar_mass_df$MolarMass_g_per_mol[
    match(elements, element_molar_mass_df$Element)
  ]

  mmolL <- mgL <- rep(NA_real_, length(recipe))

  if (unit == "mmol/L") {
    mmolL <- as.numeric(recipe)
    mgL   <- mmolL * M
  } else {
    mgL   <- as.numeric(recipe)
    mmolL <- mgL / M
  }

  df <- data.frame(
    Nutrient = nutrients,
    Element  = elements,
    `mmol/L` = round(mmolL, 4),
    `mg/L`   = round(mgL,   3),
    check.names = FALSE
  )
  rownames(df) <- NULL
  df
}

# tiny helper for null-coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a
