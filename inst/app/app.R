# app.R — NutriCalc

library(shiny)
library(bslib)
library(Ternary)

`%||%` <- function(a, b) if (is.null(a)) b else a

# =========================================================
# 1) UI HELPERS & CONSTANTS
# =========================================================

format_formula_html <- function(x) {
  if (is.na(x) || !nzchar(x)) return(x)
  x <- gsub("·", "&middot;", x, fixed = TRUE)
  x <- gsub("\\^([0-9]+)?([+-])", "<sup>\\1\\2</sup>", x, perl = TRUE)
  x <- sub("([+-])$", "<sup>\\1</sup>", x, perl = TRUE)
  repeat {
    new <- gsub("((?:[A-Z][a-z]?|\\)))(\\d+)", "\\1<sub>\\2</sub>", x, perl = TRUE)
    if (identical(new, x)) break
    x <- new
  }
  x
}

safe_chr1 <- function(x) {
  x <- as.character(x)
  if (length(x) != 1 || is.na(x)) return("")
  x
}


# - Only allows: 0–9, e/E, + - * / ^, parentheses, dot, whitespace
safe_numeric_expr <- function(txt, default = 0) {
  # normalize to a single character string
  txt <- trimws(as.character(txt)[1L])
  if (!nzchar(txt)) {
    return(default)
  }

  # Allow ONLY numbers and basic arithmetic characters.
  # Anything else → treat as invalid and return default.
  if (!grepl("^[-0-9eE+*/().^ \t\r\n]*$", txt)) {

    return(default)
  }

  # Parse safely
  expr <- tryCatch(
    parse(text = txt)[[1L]],
    error = function(e) NULL
  )
  if (is.null(expr)) {
    return(default)
  }

  # Tiny, whitelisted environment
  safe_env <- list2env(
    list(
      `+` = `+`,
      `-` = `-`,
      `*` = `*`,
      `/` = `/`,
      `^` = `^`,
      pi  = pi
    ),
    parent = emptyenv()
  )

  # Evaluate inside that safe environment
  val <- tryCatch(
    eval(expr, envir = safe_env),
    error = function(e) NA_real_
  )

  if (!is.numeric(val) || length(val) != 1L || !is.finite(val)) {
    return(default)
  }

  as.numeric(val)
}


pretty_nutrient_label_str <- function(nm) {
  switch(
    nm,
    "NO3_N" = "NO<sub>3</sub>&ndash;N",
    "NH4_N" = "NH<sub>4</sub>&ndash;N",
    nm
  )
}

nutrients <- c("NO3_N","NH4_N","P","K","Ca","Mg","S","Na","Cl",
               "Fe","Mn","Zn","B","Cu","Mo","Si")

default_expr <- c(
  "15","1.25","2.5","9","3","1.00","1.3","0","0",
  "0.015","0.01","0.005","0.015","0.00075","0.0005","0"
)
names(default_expr) <- nutrients

default_importance <- c(
  NO3_N = 0, NH4_N = 0, P = 0, K = 0, Ca = 0, Mg = 0, S = 0, Na = 0, Cl = 0,
  Fe = 0, Mn = 0, Zn = 0, B = 0, Cu = 0, Mo = 0, Si = 0
)

canonical_unit <- "mmol/L"

default_targets_mmol <- {
  v <- as.numeric(default_expr)
  names(v) <- nutrients
  v
}

normalize_unit <- function(u) {
  if (is.null(u) || is.na(u) || !nzchar(u)) return(canonical_unit)
  u <- trimws(u)
  u <- gsub("l-1", "L", u, ignore.case = TRUE)
  u <- gsub("/l", "/L", u, ignore.case = TRUE)
  u <- gsub("^umol", "µmol", u, ignore.case = TRUE)
  u
}

to_canonical_from_unit <- function(vals, unit_in) {
  unit_in <- normalize_unit(unit_in)

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

from_canonical_to_unit <- function(vals, unit_out) {
  unit_out <- normalize_unit(unit_out)

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

parse_targets_from_inputs <- function(input, nutrients, unit_in) {
  vals <- sapply(nutrients, function(nm) {
    txt <- input[[paste0("expr_", nm)]]

    # 1) Try safe numeric expression (sandboxed)
    val <- safe_numeric_expr(txt, default = NA_real_)

    # 2) Fallback: plain numeric with 0 default (same behaviour as before)
    if (is.na(val)) {
      v2 <- suppressWarnings(as.numeric(txt))
      if (is.na(v2)) v2 <- 0
      val <- v2
    }

    as.numeric(val)
  })
  names(vals) <- nutrients

  to_canonical_from_unit(vals, unit_in)
}



targets_for_display <- function(targets_mmol, unit_out) {
  from_canonical_to_unit(targets_mmol, unit_out)
}

source("modules.R")

# =========================================================
# 2) UI
# =========================================================

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  titlePanel("NutriCalc 🌿 Nutrient Optimization"),
  fluidRow(
    input_panel_ui("inputs"),
    results_panel_ui("results")
  )
)

# =========================================================
# 3) SERVER
# =========================================================

server <- function(input, output, session) {
  inputs <- input_panel_server("inputs")
  results_panel_server("results", inputs)
}

shinyApp(ui, server)
