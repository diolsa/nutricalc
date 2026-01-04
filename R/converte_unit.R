#' Convert nutrient concentrations between mmol/L and mg/L
#'
#' Converts numeric nutrient values (single element or named vector)
#' between mmol/L and mg/L using molar masses in `element_molar_mass_df`.
#'
#' @param x Numeric scalar or named numeric vector of nutrient values.
#'   If named, names should match those in `nutrient_element_map`.
#' @param element Optional element symbol (e.g. "N", "P") when `x` is a scalar.
#' @param to Target unit: "mg/L" or "mmol/L".
#'
#' @return Numeric vector (same length/names as `x`) in the target unit.
#' @examples
#' convert_units(c(NO3_N = 15, P = 1.5), to = "mg/L")
#' convert_units(c(NO3_N = 210, P = 46), to = "mmol/L")
#' @export

convert_units <- function(x, element = NULL, to = c("mg/L", "mmol/L")) {
  to <- match.arg(to)

  convert_one <- function(value, element, to) {
    M <- element_molar_mass_df$MolarMass_g_per_mol[
      match(element, element_molar_mass_df$Element)
    ]
    if (is.na(M)) stop("Element not found: ", element)
    if (to == "mg/L")  return(value * M)   # mmol/L \u2192 mg/L
    if (to == "mmol/L") return(value / M)  # mg/L   \u2192 mmol/L
  }

  # Named recipe vector
  if (!is.null(names(x))) {
    out <- x
    for (nutrient in names(x)) {
      element_sym <- nutrient_element_map[[nutrient]]
      if (is.null(element_sym)) next
      out[nutrient] <- convert_one(x[nutrient], element_sym, to)
    }
    return(out)
  }

  # Single value
  if (!is.null(element)) return(convert_one(x, element, to))

  stop("If x is a scalar, please provide the `element` argument.")
}

# Nutrient \u2192 element mapping (internal)
#' @keywords internal
nutrient_element_map <- list(
  "NO3_N" = "N", "NH4_N" = "N", "P" = "P", "K" = "K", "Ca" = "Ca",
  "Mg" = "Mg", "S" = "S", "Na" = "Na", "Cl" = "Cl", "Fe" = "Fe",
  "Mn" = "Mn", "Zn" = "Zn", "B" = "B", "Cu" = "Cu", "Mo" = "Mo", "Si" = "Si"
)

#' Create a summary table of nutrient concentrations in both units
#'
#' Given a named vector of nutrient concentrations and its unit,
#' returns a data frame showing each nutrient's value in mmol/L and mg/L.
#'
#' @param recipe Named numeric vector of nutrient concentrations (e.g., NO3_N, P, K, ...).
#' @param unit Current unit of the values: "mmol/L" or "mg/L".
#'
#' @return A data frame with columns `Nutrient`, `Element`, `mmol/L`, `mg/L`.
#' @examples
#' recipe_summary(c(NO3_N = 15, P = 1.5), unit = "mmol/L")
#' recipe_summary(c(NO3_N = 210, P = 46), unit = "mg/L")
#' @export
recipe_summary <- function(recipe, unit = c("mmol/L","mg/L")) {
  unit <- match.arg(unit)
  if (is.null(names(recipe)))
    stop("`recipe` must be a named vector (e.g., NO3_N, P, K, Ca, ...).")

  nutrients <- names(recipe)
  elements  <- vapply(
    nutrients,
    function(n) nutrient_element_map[[n]] %||% NA_character_,
    character(1)
  )

  # lookup molar masses aligned with elements
  M <- element_molar_mass_df$MolarMass_g_per_mol[
    match(elements, element_molar_mass_df$Element)
  ]

  if (unit == "mmol/L") {
    mmolL <- as.numeric(recipe)
    mgL   <- mmolL * M
  } else {
    mgL   <- as.numeric(recipe)
    mmolL <- mgL / M
  }

  data.frame(
    Nutrient = nutrients,
    Element  = elements,
    `mmol/L` = round(mmolL, 4),
    `mg/L`   = round(mgL,   3),
    check.names = FALSE,
    row.names = NULL
  )
}
