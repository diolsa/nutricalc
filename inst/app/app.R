# app.R \u2014 NutriCalc

library(shiny)
library(bslib)
library(Ternary)
library(xml2)

# =========================================================
# 1) UI HELPERS & CONSTANTS
# =========================================================

format_formula_html <- function(x) {
  if (is.na(x) || !nzchar(x)) return(x)
  x <- gsub("\u00b7", "&middot;", x, fixed = TRUE)
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


# - Only allows: 0\u20139, e/E, + - * / ^, parentheses, dot, whitespace
safe_numeric_expr <- function(txt, default = 0) {
  # normalize to a single character string
  txt <- trimws(as.character(txt)[1L])
  if (!nzchar(txt)) {
    return(default)
  }

  # Allow ONLY numbers and basic arithmetic characters.
  # Anything else \u2192 treat as invalid and return default.
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

nutrients <- if (requireNamespace("nutricalc", quietly = TRUE) &&
  exists("CANONICAL_NUTRIENTS", where = asNamespace("nutricalc"), inherits = FALSE)) {
  get("CANONICAL_NUTRIENTS", envir = asNamespace("nutricalc"))
} else {
  c("NO3_N","NH4_N","P","K","Ca","Mg","S","Na","Cl",
    "Fe","Mn","Zn","B","Cu","Mo","Si")
}

tissue_nutrients <- {
  out <- character(0)
  for (nm in nutrients) {
    if (nm == "NO3_N") {
      out <- c(out, "N")
    } else if (nm != "NH4_N") {
      out <- c(out, nm)
    }
  }
  out
}

split_total_n_by_electroneutrality <- function(total_n_mgL, targets_mgL) {
  total_n_mgL <- suppressWarnings(as.numeric(total_n_mgL)[1])
  if (!is.finite(total_n_mgL) || total_n_mgL <= 0) {
    return(c(NO3_N = 0, NH4_N = 0))
  }

  fallback <- c(NO3_N = total_n_mgL, NH4_N = 0)

  nm_targets <- names(targets_mgL)
  targets_mgL <- suppressWarnings(as.numeric(targets_mgL))
  names(targets_mgL) <- nm_targets
  targets_mgL[is.na(targets_mgL)] <- 0

  base_mgL <- setNames(rep(0, length(nutrients)), nutrients)
  shared <- intersect(names(targets_mgL), nutrients)
  base_mgL[shared] <- targets_mgL[shared]
  base_mgL[c("NO3_N", "NH4_N")] <- 0

  cation_eq <- c(K = 1, Ca = 2, Mg = 2, Na = 1, Fe = 3, Mn = 2, Zn = 2, Cu = 2)
  anion_eq <- c(P = 1, S = 2, Cl = 1, Mo = 2, B = 1, Si = 1)

  out <- tryCatch({
    other_mmol <- to_canonical_from_unit(base_mgL, "mg/L")

    n_mgL <- setNames(rep(0, length(nutrients)), nutrients)
    n_mgL[["NO3_N"]] <- total_n_mgL
    total_n_mmol <- to_canonical_from_unit(n_mgL, "mg/L")[["NO3_N"]]

    pos_meq <- sum(other_mmol[names(cation_eq)] * cation_eq, na.rm = TRUE)
    neg_meq <- sum(other_mmol[names(anion_eq)] * anion_eq, na.rm = TRUE)

    nh4_mmol <- min(max(neg_meq - pos_meq, 0), total_n_mmol)
    no3_mmol <- max(total_n_mmol - nh4_mmol, 0)

    split_mgL <- from_canonical_to_unit(c(NO3_N = no3_mmol, NH4_N = nh4_mmol), "mg/L")
    split_mgL <- c(NO3_N = split_mgL[["NO3_N"]] %||% 0, NH4_N = split_mgL[["NH4_N"]] %||% 0)
    split_mgL[!is.finite(split_mgL)] <- 0
    split_mgL <- pmax(split_mgL, 0)

    split_sum <- sum(split_mgL)
    if (!is.finite(split_sum) || split_sum <= 0) {
      fallback
    } else {
      split_mgL * (total_n_mgL / split_sum)
    }
  }, error = function(e) {
    fallback
  })

  out <- c(NO3_N = out[["NO3_N"]] %||% 0, NH4_N = out[["NH4_N"]] %||% 0)
  out[!is.finite(out)] <- 0
  out <- pmax(out, 0)
  out[["NH4_N"]] <- min(out[["NH4_N"]], total_n_mgL)
  out[["NO3_N"]] <- max(total_n_mgL - out[["NH4_N"]], 0)
  out
}

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

default_water_expr <- setNames(rep("0", length(nutrients)), nutrients)
default_water_hco3_expr <- "0"
default_water_ks82_expr <- "0"
default_water_mmol <- {
  v <- as.numeric(default_water_expr)
  names(v) <- nutrients
  v
}
default_water1_expr <- default_water_expr
default_water2_expr <- default_water_expr
water_extra_keys <- c("HCO3", "KS82")
water_extra_labels <- c(HCO3 = "KS 4.3", KS82 = "KB 8.2")
water_extra_formulas <- c(HCO3 = "HCO3", KS82 = "CO2")
water_keys <- c(nutrients, water_extra_keys)

parse_targets_from_inputs <- function(input, nutrients, unit_in, prefix = "expr_") {
  vals <- sapply(nutrients, function(nm) {
    txt <- input[[paste0(prefix, nm)]]

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

convert_water_value <- function(value, unit_in, unit_out, formula) { unit_in <- normalize_unit(unit_in); unit_out <- normalize_unit(unit_out)
  if (unit_in == unit_out) return(value); molar_mass <- compute_molar_mass(formula)
  if (unit_in == "mg/L") value <- value / molar_mass else if (unit_in == "\u00b5mol/L") value <- value / 1000
  if (unit_out == "mg/L") value <- value * molar_mass else if (unit_out == "\u00b5mol/L") value <- value * 1000; value }

water_input_id <- function(prefix, key) {
  if (key == "KS82") {
    if (prefix == "water_expr_") {
      return("water_ks82")
    }
    return(paste0(prefix, "ks82"))
  }
  paste0(prefix, key)
}

water_input_default <- function(key) {
  if (key %in% nutrients) return(default_water_expr[[key]])
  if (key == "HCO3") return(default_water_hco3_expr)
  default_water_ks82_expr
}

water_key_label <- function(key) {
  if (key %in% nutrients) return(pretty_nutrient_label_str(key))
  water_extra_labels[[key]]
}

# =========================================================
# 2) UI
# =========================================================

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(tags$style(HTML("
  body { font-size: 13px; }
  .form-group { margin-bottom: 3px; }
  .shiny-input-container { margin-bottom: 3px; }

  .container-fluid { max-width: 1300px; } input.form-control { height: 26px; padding: 2px 4px; font-size: 13px; }
  .irs-single, .irs-min, .irs-max { font-size: 10px; }

 .importance-slider .irs-min,.importance-slider .irs-max {display: none !important;}

.importance-slider .irs-single { display: block !important; top: auto
!important; transform: translateY(12px); margin-top: 0 !important;
 background: transparent !important; color: #000000 !important; font-size: 10px;
  font-weight: bold;  padding: 0; z-index: 5 !important; pointer-events: none;}


 .importance-slider .irs-single:before { display: none !important;}
 .importance-slider .irs { margin-top: -16px; height: 28px !important;}
 .importance-slider { margin-bottom: 10px; }
 .importance-slider .irs-grid {  height: 0 !important;  margin-top: -4px !important;}
 .importance-slider .irs-grid-text { display: none !important;}

 .nutrient-row {border-bottom: 1px solid #dee2e6;padding: 2px 0 4px 0;margin-bottom: 2px; }
 .nutrient-row:last-child {border-bottom: none;}

 .water-mix-slider .irs-min,
 .water-mix-slider .irs-max,
 .water-mix-slider .irs-single,
 .water-mix-slider .irs-grid-text,
 .water-mix-slider .irs-grid { display: none !important; }

 .water-mix-table td { padding: 4px 6px; height: 37.8px; vertical-align: top; }
 .water-mix-table tr:last-child td { border-bottom: none !important; }
 .water-input-grid { margin-top: 10px; }



  #selected_salts { max-height: 590px; overflow-y: auto; padding: 6px 1px 1px; }
  #input_unit label { color: #6c757d !important; font-size: 0.9em !important; font-weight: normal !important; margin-right: 6px !important; }
  #input_unit .radio-inline { margin-right: 6px !important; }
  #select_all_visible, #deselect_all_salts, #clear_search { width: 30px !important; height: 30px !important; padding: 0 !important; display: inline-flex; align-items: center; justify-content: center; background-color: #e9ecef !important; border: 1px solid rgba(0,0,0,0.25) !important; border-radius: 4px !important; box-shadow: inset 0 1px 1px rgba(0,0,0,0.075); margin: 3px; }
  #select_all_visible i, #deselect_all_salts i, #clear_search i { color: #212529 !important; font-size: 16px; line-height: 1; margin: 3px; }
  #select_all_visible:hover, #deselect_all_salts:hover, #clear_search:hover { background-color: #dee2e6 !important; }
  .search-inline .shiny-input-container { margin-bottom: 0 !important; flex: 1; }
  .search-inline .form-control { height: 30px !important; padding: 4px 8px !important; line-height: 1.2 !important; }
  #clear_search { height: 30px !important; padding: 0 10px !important; display: inline-flex; align-items: center; justify-content: center; }

#input_tabs{
  --bs-nav-link-padding-y: 10px;
  --bs-nav-link-padding-x: 18px;
  --bs-nav-link-font-size: 14px;
}

#input_tabs > ul.nav.nav-tabs{
  display:flex !important;
  flex-wrap:nowrap !important;
  overflow-x:auto !important;
}

#result_tabs{
  --bs-nav-link-padding-y: 10px;
  --bs-nav-link-padding-x: 18px;
  --bs-nav-link-font-size: 14px;
}

#result_tabs > ul.nav.nav-tabs{
  display:flex !important;
  flex-wrap:nowrap !important;
  overflow-x:auto !important;
}



")),



  ),

  titlePanel("NutriCalc \U0001f33f Nutrient Solution Formulation in Horticultural Sciences"),

  fluidRow(
    # LEFT: inputs
    column(
      width = 6,
      div(class = "card p-3",
          h5("\u2697\ufe0f Nutrient Targets & Fertilizers"),
          tabsetPanel(id = "input_tabs", type = "tabs",
                      tabPanel("\U0001f3af Nutrient Targets",
                               fluidRow(
          column(2, strong("Nutrient")),
          column(4,
                fluidRow(
                  column(6, strong(textOutput("target_col_header"))),
                  column(6, strong("Water"))
                ),
                div(
                                          radioButtons("input_unit", label = NULL,
                                                       choices = c("mmol/L", "\u00b5mol/L", "mg/L"),
                                                       selected = "mmol/L", inline = TRUE),
                                        )
                                 ),
          column(6,
                div(strong("Priority"),
                    tags$br(),
                    tags$small(class = "text-muted", "Weight for target matching"))
          )
                               ),
                               tags$hr(style = "margin:4px 0;"),
                               uiOutput("nutrient_rows"),




                      ),
                      tabPanel("\U0001f4a6 Water",
                               fluidRow(
                                 column(6,
                                        textInput("bwb_plz", "Berlin postal code:", placeholder = "e.g. 14195", width = "100%"),
                                        tags$small(class = "text-muted", "Fetch average values from Berliner Wasserbetriebe")),
                                 column(6,
                                        br(),
                                        div(
                                          style = "display:flex; gap:6px; align-items:center;",
                                          actionButton("apply_bwb_water1", "Mix Water", class = "btn btn-outline-primary btn-sm"),
                                          actionButton("apply_bwb", "Load Water", class = "btn btn-primary btn-sm")
                                        ),
                                        tags$br(), tags$br())
                               ),
                               tags$hr(style = "margin:6px 0;"),
                               fluidRow(
                                 column(
                                   8,
                                   div(
                                     class = "water-mix-slider",
                                     sliderInput(
                                       "water_mix_pct",
                                       label = NULL,
                                       min = 0,
                                       max = 100,
                                       value = 0,
                                       step = 1,
                                       width = "100%",
                                       ticks = FALSE,
                                       sep = ""
                                     )
                                   )
                                 ),
                                 column(
                                   4,
                                   div(style = "margin-top: 14px; margin-bottom: 14px;",
                                       actionButton("apply_mix_to_water", "Load Mixed Water", class = "btn btn-primary btn-sm"))
                                 )
                               ),
                               fluidRow(
                                 column(
                                   4,
                                   h6(textOutput("water1_mix_label")),
                                   div(class = "water-input-grid", uiOutput("water1_inputs"))
                                 ),
                                 column(
                                   4,
                                   h6(textOutput("water2_mix_label")),
                                   div(class = "water-input-grid", uiOutput("water2_inputs"))
                                 ),
                                 column(
                                   4,
                                   h6(textOutput("mixed_water_label")),
                                   div(class = "water-mix-table", tableOutput("water_mix_table"))
                                 )
                               )
                      ),
                      tabPanel("\U0001f9c2 Fertilizers",
                               fluidRow(column(10,
                                               div(class = "search-inline", style = "display:flex; align-items:center; gap:6px;",
                                                   textInput("salt_search", label = NULL, placeholder = "Search salts...", width = "100%"),
                                                   actionButton("clear_search", label = NULL, icon = icon("times-circle"), class = "btn btn-light btn-sm", title = "Clear search")
                                               )
                               )),
                               tags$hr(style = "margin:6px 0;"),
                               div(class = "btn-group", role = "group",
                                   actionButton("select_all_visible", label = NULL, icon = icon("check-square"), class = "btn btn-light btn-sm", title = "Select all visible"),
                                   actionButton("deselect_all_salts", label = NULL, icon = icon("square"), class = "btn btn-light btn-sm", title = "Deselect all")
                               ),
                               tags$div(style = "margin:13px 0;"),
                               uiOutput("salt_picker"),
                               uiOutput("sel_status")
                      ),
                      tabPanel("\U0001f4da Recipes",
                               fluidRow(
                                 column(
                                   width = 4,
                                   radioButtons(
                                     "recipe_category", div(strong("Category")),
                                     choices  = c(
                                       "\U0001f96c Olericulture"           = "Olericulture",
                                       "\U0001f353 Fruticulture"           = "Fruticulture",
                                       "\U0001f338 Floriculture"           = "Floriculture",
                                       "\u2699\ufe0f Standard Formulations"  = "Standard Formulations"
                                     ),
                                     selected = "Olericulture"
                                   )
                                 ),
                                 column(
                                   width = 8,
                                   uiOutput("recipe_select_ui"),
                                   br(),
                                   actionButton("apply_recipe", "Apply recipe", class = "btn btn-primary btn-sm")
                                 )
                               ),
                               tags$hr(style = "margin:6px 0;"),
                               uiOutput("recipe_notes"),
                               tags$small(
                                 class = "text-muted",
                                 "Pick a category, then a recipe. Applying a recipe will set the unit, fill targets, and (if defined) select defined salts."
                               )
                      ),
                      tabPanel("\U0001f9ec Tissue",
                               fluidRow(
                                 column(
                                   width = 4,
                                   h5("Tissue (%)"),
                                   tags$small(class = "text-muted",
                                              "Nutrient concentration in dry mass"),
                                   br(),
                                   # --- header row: Nutrient | Tissue % -------------------------------
                                   fluidRow(
                                     column(6, strong("Nutrient")),
                                     column(6, strong("Tissue %"))
                                   ),
                                   tags$hr(style = "margin:4px 0;"),
                                   tags$div(
                                     lapply(tissue_nutrients, function(nm) {
                                       fluidRow(
                                         column(
                                           width = 6,
                                           tags$label(HTML(pretty_nutrient_label_str(nm)))
                                         ),
                                         column(
                                           width = 6,
                                           numericInput(
                                             inputId = paste0("tissue_", nm),
                                             label   = NULL,           # label handled in left column
                                             value   = NA,
                                             min     = 0,
                                             step    = 0.01,
                                             width   = "100%"
                                           )
                                         )
                                       )
                                     })
                                   )
                                 ),
                                 column(
                                   width = 3,
                                   h5("Water Use Efficiency (WUE)"),
                                   tags$small(class = "text-muted",
                                              "WUE = plant dry mass / water transpired."),
                                   numericInput("tissue_dm", "Plant dry mass (g)",
                                                value = NA, min = 0, step = 0.1, width = "100%"),
                                   numericInput("tissue_water", "Water transpired (L)",
                                                value = NA, min = 0, step = 0.1, width = "100%"),
                                   br(),
                                   strong(textOutput("tissue_wue_text"))
                                 ),
                                 column(
                                   width = 5,
                                   h5("Nutrient solution"),
                                   tags$small(class = "text-muted",
                                              "Calculated mg L\u207b\u00b9 from tissue % \u00d7 WUE."),
                                   tableOutput("tissue_table"),
                                   br(),
                                   actionButton("tissue_apply", "Use as targets",
                                                class = "btn btn-primary btn-sm")
                                 )
                               )
                      )

          ),
          # ---- Global buttons (always visible under tabs) ----
          tags$hr(style = "margin:6px 0;"),
          fluidRow(
            id = "button_row",
            column(
              width = 3,
              actionButton(
                "set_all_zero", "Set all to 0",
                class = "btn btn-light btn-sm",
                style = "width:100%; margin:0;"
              )
            ),
            column(
              width = 3,
              actionButton(
                "reset", "Reset to defaults",
                class = "btn btn-light btn-sm",
                style = "width:100%; margin:0;"
              )
            ),
            column(
              width = 3,
              div(
                style = "margin-top: 2px;",
                checkboxInput("use_acid_base", "Use acids/bases", value = FALSE, width = "100%")
              )
            ),
            column(
              width = 3,
              actionButton(
                "run", "\U0001f9ea Result",
                class = "btn btn-primary btn-sm",
                style = "width:100%; margin:0;"
              )
            )
          ),

          br(),
          uiOutput("status")

      )
    ),



    # RIGHT: Result & Plots tabs
    column(
      width = 6,
      div(class = "card p-3",
          h5("\U0001f9ea Result & Plots"),
          tabsetPanel(id = "result_tabs", type = "tabs",
                      tabPanel("\U0001f5d2Result",
                               uiOutput("delivery_ui")
                      ),
                      tabPanel("\U0001f9ea pH & EC",
                               tags$div(
                                 class = "text-center mb-2",
                                 tags$strong("Adjust Concentration"),
                                 sliderInput("ph_ec_multiplier", label = NULL, min = -100, max = 100, value = 0,
                                             step = 1, ticks = FALSE, width = "100%", sep = "", post = " %")
                               ),
                               uiOutput("ph_ui")
                      ),
                      tabPanel("\U0001f6e2\ufe0f Solution",
                               fluidRow(
                                 column(
                                   width = 4,
                                   numericInput(
                                     "vol", "Working volume (L)",
                                     value = 1, min = 0, step = 1, width = "100%"
                                   )
                                 ),
                                 column(
                                   width = 4,
                                   numericInput(
                                     "stock_factor", "Stock factor (\u00d7)",
                                     value = 100, min = 1, step = 1, width = "100%"
                                   )
                                 ),
                                 column(
                                   width = 4,
                                   numericInput(
                                     "stock_vol", "Stock volume (L)",
                                     value = 1, min = 0, step = 0.1, width = "100%"
                                   )
                                 )
                               ),
                               uiOutput("fertilizer_ui")
                      )

                      ,
                      tabPanel("\u2796 Anions",
                               tags$p(class = "text-muted", "Select exactly three anions (defaults: NO3\u2013N, P, S)."),
                               selectizeInput("anion_keys", label = NULL,
                                              choices = c("NO3_N", "P", "S", "Cl"),
                                              selected = c("NO3_N", "P", "S"),
                                              multiple = TRUE,
                                              options = list(maxItems = 3, plugins = list("remove_button"))
                               ),
                               div(style = "display:flex; justify-content:center; align-items:center;",
                                   plotOutput("ternary_anions", height = "666px"))
                      ),
                      tabPanel("\u2795 Cations",
                               tags$p(class = "text-muted", "Select exactly three cations (defaults: K, Ca, Mg)."),
                               selectizeInput("cation_keys", label = NULL,
                                              choices = c("K", "Ca", "Mg", "NH4_N", "Na"),
                                              selected = c("K", "Ca", "Mg"),
                                              multiple = TRUE,
                                              options = list(maxItems = 3, plugins = list("remove_button"))
                               ),
                               div(style = "display:flex; justify-content:center; align-items:center;",
                                   plotOutput("ternary_cations", height = "666px"))
                      )
          )
      )
    )
  ))


# =========================================================
# 3) SERVER
# =========================================================

server <- function(input, output, session) {

  run_trigger <- reactiveVal(0)

  targets_mmol <- reactiveVal(default_targets_mmol)
  water_mmol <- reactiveVal(default_water_mmol)
  water_hco3 <- reactiveVal(as.numeric(default_water_hco3_expr))
  water_co2_aq <- reactiveVal(as.numeric(default_water_ks82_expr))
  current_input_unit <- reactiveVal(canonical_unit)
  selected_salts <- reactiveVal(rownames(nutrient_matrix))

  recipe_categories <- vapply(
    recipes,
    function(r) r$category %||% "Standard Formulations",
    character(1)
  )

  output$target_col_header <- renderText({
    "Target"
  })

  output$nutrient_rows <- renderUI({
    rows <- lapply(nutrients, function(nm) {
      div(
        class = "nutrient-row",fluidRow(
          column(2, tags$label(HTML(pretty_nutrient_label_str(nm)))),
          column(2, textInput(inputId = paste0("expr_", nm), label = NULL, value = default_expr[[nm]])),
          column(2, textInput(inputId = paste0("water_expr_", nm), label = NULL, value = default_water_expr[[nm]])),
          column(6, div(class = "importance-slider",
                        sliderInput(inputId = paste0("imp_", nm), label = NULL ,ticks   = TRUE
                                    , min = -2, max = 2, step = 1, value = default_importance[[nm]])))
        ))
    })

    hco3_row <- div(
      class = "nutrient-row",
      fluidRow(
        column(2, tags$label("KS 4.3")),
        column(2, tags$div()),
        column(2, textInput(inputId = "water_expr_HCO3", label = NULL, value = default_water_hco3_expr)),
        column(6, tags$div())
      )
    )

    co2_row <- div(
      class = "nutrient-row",
      fluidRow(
        column(2, tags$label("KB 8.2")),
        column(2, tags$div()),
        column(2, textInput(inputId = "water_ks82", label = NULL, value = default_water_ks82_expr)),
        column(6, tags$div())
      )
    )

    tagList(c(rows, list(hco3_row, co2_row)))
  })

  output$water1_inputs <- renderUI({
    rows <- lapply(water_keys, function(key) {
      div(
        class = "nutrient-row",
        fluidRow(
          column(6, tags$label(HTML(water_key_label(key)))),
          column(6, textInput(inputId = water_input_id("water1_expr_", key), label = NULL, value = water_input_default(key), width = "100%"))
        )
      )
    })

    tagList(rows)
  })

  output$water2_inputs <- renderUI({
    rows <- lapply(water_keys, function(key) {
      div(
        class = "nutrient-row",
        fluidRow(
          column(6, tags$label(HTML(water_key_label(key)))),
          column(6, textInput(inputId = water_input_id("water2_expr_", key), label = NULL, value = water_input_default(key), width = "100%"))
        )
      )
    })

    tagList(rows)
  })

  inputs_ready <- reactive({
    all(vapply(nutrients, function(nm) {
      !is.null(input[[paste0("expr_", nm)]]) &&
        !is.null(input[[paste0("water_expr_", nm)]])
    }, logical(1))) && !is.null(input$water_expr_HCO3) && !is.null(input$water_ks82)
  })

  water_mix_inputs_ready <- reactive({
    all(vapply(water_keys, function(key) {
      !is.null(input[[water_input_id("water1_expr_", key)]]) &&
        !is.null(input[[water_input_id("water2_expr_", key)]])
    }, logical(1)))
  })

  parse_water_set <- function(prefix, unit_in) {
    vals <- parse_targets_from_inputs(input, nutrients, unit_in, prefix = prefix)
    extra_vals <- vapply(
      water_extra_keys,
      function(key) {
        raw <- input[[water_input_id(prefix, key)]] %||% ""
        raw <- gsub(",", ".", raw, fixed = TRUE)
        val <- safe_numeric_expr(raw, default = 0)
        convert_water_value(val, unit_in, canonical_unit, water_extra_formulas[[key]])
      },
      numeric(1)
    )
    c(vals, extra_vals)
  }

  water_values_for_display <- function(vals, unit_out) {
    out <- numeric(length(water_keys))
    names(out) <- water_keys
    out[nutrients] <- targets_for_display(vals[nutrients], unit_out)
    for (key in water_extra_keys) {
      out[[key]] <- convert_water_value(vals[[key]], canonical_unit, unit_out, water_extra_formulas[[key]])
    }
    out
  }

  update_water_inputs <- function(prefix, vals, unit_out) {
    display <- water_values_for_display(vals, unit_out)
    for (key in water_keys) {
      updateTextInput(
        session,
        water_input_id(prefix, key),
        value = format(display[[key]], trim = TRUE, scientific = FALSE)
      )
    }
  }

  mixed_water <- reactive({
    req(water_mix_inputs_ready())
    unit_in <- input$input_unit %||% canonical_unit
    w1 <- parse_water_set("water1_expr_", unit_in)
    w2 <- parse_water_set("water2_expr_", unit_in)

    ratio <- 1 - (suppressWarnings(as.numeric(input$water_mix_pct %||% 50)) / 100)
    if (!is.numeric(ratio) || is.na(ratio)) ratio <- 0.5
    ratio <- min(max(ratio, 0), 1)

    list(
      mixed = (w1 * ratio) + (w2 * (1 - ratio)),
      ratio = ratio,
      unit = unit_in,
      w1 = w1,
      w2 = w2
    )
  })

  output$water_mix_table <- renderTable({
    mix <- mixed_water()
    unit_out <- input$input_unit %||% canonical_unit
    w1_display <- water_values_for_display(mix$w1, unit_out)
    w2_display <- water_values_for_display(mix$w2, unit_out)
    mix_display <- water_values_for_display(mix$mixed, unit_out)

    data.frame(
      Nutrient = vapply(water_keys, water_key_label, character(1)),
      Mix = round(mix_display, 4),
      check.names = FALSE
    )
  }, sanitize.text.function = function(x) x, colnames = FALSE)

  output$water1_mix_label <- renderText({
    pct <- 100 - (suppressWarnings(as.numeric(input$water_mix_pct %||% 50)))
    if (!is.numeric(pct) || is.na(pct)) pct <- 50
    pct <- round(min(max(pct, 0), 100))
    sprintf("Water 1 [%d%%]", pct)
  })

  output$water2_mix_label <- renderText({
    pct <- suppressWarnings(as.numeric(input$water_mix_pct %||% 50))
    if (!is.numeric(pct) || is.na(pct)) pct <- 50
    pct <- round(min(max(pct, 0), 100))
    sprintf("Water 2 [%d%%]", pct)
  })

  output$mixed_water_label <- renderText({
    unit_out <- input$input_unit %||% canonical_unit
    sprintf("Mixed water [%s]", unit_out)
  })

  observeEvent(input$reset, {
    for (nm in nutrients) {
      updateTextInput(session, paste0("expr_", nm), value = default_expr[[nm]])
      updateSliderInput(session, paste0("imp_", nm), value = default_importance[[nm]])
    }
    targets_mmol(default_targets_mmol)
    current_input_unit(input$input_unit %||% canonical_unit)
  })

  observeEvent(input$set_all_zero, {
    updateTabsetPanel(session, "input_tabs", selected = "\U0001f3af Nutrient Targets")

    session$onFlushed(function() {
      for (nm in nutrients) {
        updateTextInput(session, paste0("expr_", nm), value = "0")
        updateTextInput(session, paste0("water_expr_", nm), value = "0")
        updateTextInput(session, paste0("water1_expr_", nm), value = "0")
        updateTextInput(session, paste0("water2_expr_", nm), value = "0")
      }
      updateTextInput(session, "water_expr_HCO3", value = "0")
      updateTextInput(session, "water_ks82", value = "0")
      updateTextInput(session, "water1_expr_HCO3", value = "0")
      updateTextInput(session, "water1_expr_ks82", value = "0")
      updateTextInput(session, "water2_expr_HCO3", value = "0")
      updateTextInput(session, "water2_expr_ks82", value = "0")
    }, once = TRUE)
  })

  apply_bwb_values <- function() {
    plz <- input$bwb_plz %||% ""
    plz <- trimws(plz)
    if (!nzchar(plz)) {
      showNotification("Please enter a postal code.", type = "error")
      return(NULL)
    }

    vals_mmol <- tryCatch(
      fetch_bwb_mittelwert(plz),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )

    if (is.null(vals_mmol)) return(NULL)

    vals_mmol[is.na(vals_mmol)] <- 0
    water_hco3_mmol <- vals_mmol[["KS4_3"]]
    if (is.na(water_hco3_mmol)) {
      water_hco3_mmol <- vals_mmol[["Alkalinity"]]
    }
    if (is.na(water_hco3_mmol)) water_hco3_mmol <- 0
    water_co2_aq_mmol <- vals_mmol[["KB8_2"]]
    if (is.na(water_co2_aq_mmol)) {
      water_co2_aq_mmol <- vals_mmol[["KS8_2"]]
    }
    if (is.na(water_co2_aq_mmol)) water_co2_aq_mmol <- 0
    list(
      values = c(vals_mmol[nutrients], HCO3 = water_hco3_mmol, KS82 = water_co2_aq_mmol)
    )
  }

  observeEvent(input$apply_bwb, {
    bwb <- apply_bwb_values()
    if (is.null(bwb)) return()

    unit_out <- current_input_unit() %||% canonical_unit
    water_mmol(bwb$values[nutrients])
    water_hco3(bwb$values[["HCO3"]])
    water_co2_aq(bwb$values[["KS82"]])
    update_water_inputs("water_expr_", bwb$values, unit_out)

    showNotification("BWB water values applied.", type = "message")
    updateTabsetPanel(session, "input_tabs", selected = "\U0001f3af Nutrient Targets")
  })

  observeEvent(input$apply_bwb_water1, {
    bwb <- apply_bwb_values()
    if (is.null(bwb)) return()

    unit_out <- current_input_unit() %||% canonical_unit
    update_water_inputs("water1_expr_", bwb$values, unit_out)

    showNotification("BWB water values applied to Water 1.", type = "message")
  })

  observeEvent(input$run, {
    req(inputs_ready())
    updateSliderInput(session, "ph_ec_multiplier", value = 0)
    unit_in <- input$input_unit %||% canonical_unit
    vals_mmol <- parse_targets_from_inputs(input, nutrients, unit_in, prefix = "expr_")
    vals_water_mmol <- parse_targets_from_inputs(input, nutrients, unit_in, prefix = "water_expr_")
    targets_mmol(vals_mmol)
    water_mmol(vals_water_mmol)
    water_vals <- parse_water_set("water_expr_", unit_in)
    water_hco3(water_vals[["HCO3"]])
    water_co2_aq(water_vals[["KS82"]])
    run_trigger(isolate(run_trigger()) + 1L)
  })

  observeEvent(input$ph_ec_multiplier, {
    req(inputs_ready())
    run_trigger(isolate(run_trigger()) + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$input_unit, {
    req(inputs_ready())

    new_unit <- input$input_unit %||% canonical_unit
    old_unit <- current_input_unit() %||% canonical_unit

    vals_mmol <- parse_targets_from_inputs(input, nutrients, old_unit, prefix = "expr_")
    vals_water_mmol <- parse_targets_from_inputs(input, nutrients, old_unit, prefix = "water_expr_")
    targets_mmol(vals_mmol)
    water_mmol(vals_water_mmol)
    water_vals <- parse_water_set("water_expr_", old_unit)
    water_hco3(water_vals[["HCO3"]])
    water_co2_aq(water_vals[["KS82"]])
    display_vals <- targets_for_display(vals_mmol, new_unit)

    for (nm in nutrients) {
      updateTextInput(
        session, paste0("expr_", nm),
        value = format(display_vals[[nm]], trim = TRUE, scientific = FALSE)
      )
    }
    update_water_inputs("water_expr_", water_vals, new_unit)

    if (water_mix_inputs_ready()) {
      water1_vals <- parse_water_set("water1_expr_", old_unit)
      water2_vals <- parse_water_set("water2_expr_", old_unit)
      update_water_inputs("water1_expr_", water1_vals, new_unit)
      update_water_inputs("water2_expr_", water2_vals, new_unit)
    }

    current_input_unit(new_unit)
  }, ignoreInit = TRUE)

  observeEvent(input$apply_mix_to_water, {
    mix <- mixed_water()
    unit_out <- input$input_unit %||% canonical_unit
    update_water_inputs("water_expr_", mix$mixed, unit_out)
    showNotification("Mixed water values applied.", type = "message")
    updateTabsetPanel(session, "input_tabs", selected = "\U0001f3af Nutrient Targets")
  })

  target_with_multiplier <- reactive({
    vals <- targets_mmol()
    multiplier_pct <- input$ph_ec_multiplier %||% 0
    multiplier <- suppressWarnings(as.numeric(multiplier_pct)) / 100
    if (!is.numeric(multiplier) || is.na(multiplier)) multiplier <- 0
    multiplier <- min(max(multiplier, -1), 1)
    validate(
      need(!is.null(vals), "Targets not initialized yet.")
    )

    vals * (1 + multiplier)
  })

  adjusted_targets <- reactive({
    validate(need(!is.null(water_mmol()), "Water not initialized yet."))
    target_with_multiplier() - water_mmol()
  })

  eval_targets <- reactive({
    pmax(adjusted_targets(), 0)
  })

  importance_vec <- reactive({
    vals <- sapply(nutrients, function(nm) input[[paste0("imp_", nm)]])
    names(vals) <- nutrients
    pmin(pmax(vals, -2), 2)
  })

  # ---- Tissue Analysis: WUE and mg/L calculation --------------------------
  tissue_wue <- reactive({
    dm   <- input$tissue_dm   %||% NA_real_
    wat  <- input$tissue_water %||% NA_real_
    if (is.na(dm) || is.na(wat) || wat <= 0) return(NA_real_)
    dm / wat  # g dry mass per L water
  })

  output$tissue_wue_text <- renderText({
    w <- tissue_wue()
    if (is.na(w)) return("WUE: \u2014")
    sprintf("WUE: %.2f g dry mass per L water", w)
  })


  tissue_mgL <- reactive({
    w <- tissue_wue()
    if (is.na(w)) {
      out <- setNames(rep(0, length(nutrients)), nutrients)
      return(out)
    }

    # % in tissue (g per 100 g dry mass)
    tissue_perc <- setNames(rep(0, length(tissue_nutrients)), tissue_nutrients)
    for (nm in tissue_nutrients) {
      val <- input[[paste0("tissue_", nm)]] %||% NA_real_
      tissue_perc[[nm]] <- ifelse(is.na(val), 0, as.numeric(val))
    }

    # convert % -> mg/g : 1% = 10 mg/g
    tissue_mg_per_g <- tissue_perc * 10

    # mg/L = (mg/g) * (g/L)
    tissue_mgL <- tissue_mg_per_g * w

    mgL <- setNames(rep(0, length(nutrients)), nutrients)
    other_keys <- intersect(setdiff(tissue_nutrients, "N"), nutrients)
    mgL[other_keys] <- tissue_mgL[other_keys]

    n_split <- split_total_n_by_electroneutrality(
      total_n_mgL = tissue_mgL[["N"]] %||% 0,
      targets_mgL = mgL[nutrients[!nutrients %in% c("NO3_N", "NH4_N")]]
    )
    mgL[["NO3_N"]] <- n_split[["NO3_N"]]
    mgL[["NH4_N"]] <- n_split[["NH4_N"]]

    mgL
  })

  output$tissue_table <- renderTable({
    w <- tissue_wue()


    perc <- setNames(rep(NA_real_, length(tissue_nutrients)), tissue_nutrients)
    for (nm in tissue_nutrients) {
      val <- input[[paste0("tissue_", nm)]] %||% NA_real_
      perc[[nm]] <- ifelse(is.na(val), NA_real_, as.numeric(val))
    }

    mgL <- tissue_mgL()

    table_nutrients <- tissue_nutrients
    mg_lookup <- c(mgL, N = sum(mgL[c("NO3_N", "NH4_N")]))

    df <- data.frame(
      Nutrient   = vapply(table_nutrients, pretty_nutrient_label_str, character(1)),
      `Tissue %` = round(perc[table_nutrients], 3),
      check.names = FALSE
    )
    df[["mg l\u207b\u00b9"]] <- round(mg_lookup[table_nutrients], 3)
    df
  }, sanitize.text.function = function(x) x)





  # ---- Salts --------------------------
  all_salts <- reactive(rownames(nutrient_matrix))

  filtered_salts <- reactive({
    salts <- all_salts()
    q <- input$salt_search

    # 1) no search \u2192 return all salts (we still sort below)
    if (is.null(q) || !nzchar(trimws(q))) {
      hits <- rep(TRUE, length(salts))
    } else {
      q <- trimws(q)

      # --- load salt_info for names ---
      si <- NULL
      if (exists("salt_info", inherits = TRUE)) {
        si <- get("salt_info", inherits = TRUE)
      }
      if (is.null(si)) {
        si <- attr(nutrient_matrix, "salt_info")
      }

      # --- search by formula (original logic) ---
      hit_formula_raw   <- grepl(q, salts, ignore.case = TRUE)
      plain             <- gsub("[^A-Za-z0-9()+.-]", " ", salts)
      hit_formula_plain <- grepl(q, plain, ignore.case = TRUE)

      # --- search by human-readable name / category ---
      hit_desc <- rep(FALSE, length(salts))
      if (!is.null(si)) {
        desc_vec <- si$category[match(salts, si$salt)]
        desc_vec[is.na(desc_vec)] <- ""
        hit_desc <- grepl(q, desc_vec, ignore.case = TRUE)
      }

      hits <- hit_formula_raw | hit_formula_plain | hit_desc
    }

    salts_sub <- salts[hits]

    # ---- sort by Name (category) alphabetically; fallback: formula ----
    si <- NULL
    if (exists("salt_info", inherits = TRUE)) {
      si <- get("salt_info", inherits = TRUE)
    }
    if (is.null(si)) {
      si <- attr(nutrient_matrix, "salt_info")
    }

    if (!is.null(si)) {
      desc <- si$category[match(salts_sub, si$salt)]
      # fallback: use formula when name is missing
      desc[is.na(desc) | !nzchar(desc)] <- salts_sub[is.na(desc) | !nzchar(desc)]
      ord <- order(tolower(desc))
      salts_sub <- salts_sub[ord]
    } else {
      # no salt_info \u2192 just sort by formula
      salts_sub <- sort(salts_sub)
    }

    salts_sub
  })


  output$salt_picker <- renderUI({
    choices <- filtered_salts()
    sel     <- selected_salts()

    # no matches
    if (!length(choices)) {
      return(tags$p(class = "text-muted", "No salts match the search."))
    }

    # load salt_info (for names)
    si <- NULL
    if (exists("salt_info", inherits = TRUE)) {
      si <- get("salt_info", inherits = TRUE)
    }
    if (is.null(si)) {
      si <- attr(nutrient_matrix, "salt_info")
    }

    # descriptions
    desc_vec <- vapply(choices, function(s) {
      if (!is.null(si) && s %in% si$salt) {
        d <- si$category[match(s, si$salt)]
        if (!is.na(d) && nzchar(d)) d else ""
      } else ""
    }, character(1))

    # build table rows
    tbl_rows <- lapply(seq_along(choices), function(i) {
      s        <- choices[[i]]
      checked  <- s %in% sel
      formula  <- format_formula_html(s)
      desc     <- desc_vec[[i]]

      tags$tr(
        tags$td(
          tags$input(
            type    = "checkbox",
            class   = "salt_check form-check-input",  # <- add Bootstrap class
            value   = s,
            checked = if (checked) "checked" else NULL
          ))
        ,
        tags$td(HTML(formula)),
        tags$td(desc)
      )
    })


    js <- HTML("
    <script>
      (function(){
        var scrollKey = 'saltPickerScrollTop';

        // Persist scroll position across re-renders
        var el = document.getElementById('selected_salts');
        if (el) {
          el.scrollTop = window[scrollKey] || 0;
          el.addEventListener('scroll', function(){
            window[scrollKey] = this.scrollTop;
          });
        }

        // Track checkbox changes
        $(document).off('change.saltCheck', '.salt_check');
        $(document).on('change.saltCheck', '.salt_check', function() {
          var vals = [];
          $('.salt_check:checked').each(function(){ vals.push($(this).val()); });
          Shiny.setInputValue('salt_check_values', vals, {priority: 'event'});
        });
      })();


    </script>
  ")

    # wrap table in #selected_salts so your existing CSS (max-height, scroll) still works
    tags$div(
      id = "selected_salts",
      tags$table(
        class = "table table-sm",
        style = "width:100%; font-size:13px;",
        tags$thead(
          tags$tr(
            tags$th(""),
            tags$th("Formula"),
            tags$th("Name")
          )
        ),
        tags$tbody(tbl_rows)
      ),
      js
    )
  })




  observeEvent(input$salt_check_values, {
    vis <- filtered_salts()
    old <- selected_salts()
    visible_sel <- input$salt_check_values %||% character(0)
    new_sel <- union(  setdiff(old, vis), visible_sel)
    selected_salts(new_sel)
  })


  observeEvent(input$deselect_all_salts, {
    vis <- filtered_salts()
    old <- selected_salts()
    new_sel <- setdiff(old, vis)
    selected_salts(new_sel)

  })

  observeEvent(input$select_all_visible, {
    vis <- filtered_salts()
    old <- selected_salts()
    new_sel <- union(old, vis)
    selected_salts(new_sel)

  })

  observeEvent(input$clear_search, { updateTextInput(session, "salt_search", value = "") })

  output$sel_status <- renderUI({
    total   <- length(all_salts())
    visible <- length(filtered_salts())
    n_sel   <- length(selected_salts())
    tags$p(sprintf("Using %d selected of %d visible (total %d).", n_sel, visible, total))
  })

  recipes_for_category <- reactive({
    cat <- input$recipe_category %||% "Olericulture"
    ids <- names(recipes)[recipe_categories == cat]
    ids
  })

  output$recipe_select_ui <- renderUI({
    ids <- recipes_for_category()
    if (!length(ids)) {
      return(tags$p(class = "text-muted", "No recipes for this category yet."))
    }

    labels <- vapply(recipes[ids], `[[`, "", "name")

    current <- input$recipe_pick
    if (!is.null(current) && current %in% ids) {
      selected <- current
    } else {
      selected <- ids[[1]]
    }

    div(
      style = "max-height: 666px; overflow-y: auto; padding-right: 4px;",
      radioButtons(
        "recipe_pick",
        label   = strong("Recipe"),
        choices = setNames(ids, labels),
        selected = selected,
        width ="100%"
      )
    )
  })

  output$recipe_notes <- renderUI({
    id <- input$recipe_pick
    if (is.null(id) || !nzchar(id) || is.null(recipes[[id]])) return(NULL)
    rec <- recipes[[id]]
    if (!is.null(rec$notes) && nzchar(rec$notes)) {
      tags$p(class = "text-muted", rec$notes)
    } else NULL
  })

  observeEvent(input$recipe_pick, {
    req(input$recipe_pick)
    rec <- recipes[[input$recipe_pick]]

    if (!is.null(rec$unit) && rec$unit %in% c("mmol/L", "mg/L", "\u00b5mol/L", "umol/L")) {
      if (!identical(normalize_unit(input$input_unit), normalize_unit(rec$unit))) {
        updateRadioButtons(session, "input_unit", selected = normalize_unit(rec$unit))
      }
    }
  }, ignoreInit = TRUE)

  apply_salts <- function(rec) {
    if (is.null(rec$salts)) {
      output$recipe_match_status <- renderUI(NULL)
      return()
    }

    avail <- rownames(nutrient_matrix)

    if (isTRUE(rec$salts)) {
      matched   <- avail
      unmatched <- character(0)
    } else {
      matched   <- rec$salts[rec$salts %in% avail]
      unmatched <- setdiff(rec$salts, matched)
    }

    # Source of truth: just update selected_salts()
    selected_salts(matched)

    output$recipe_match_status <- renderUI({
      if (length(unmatched)) {
        tags$p(
          HTML(sprintf(
            "Loaded <b>%d</b> salt(s). Unmatched: <code>%s</code>.",
            length(matched), paste(unmatched, collapse = ", ")
          )),
          class = "text-warning"
        )
      } else {
        tags$p(sprintf("Loaded %d salt(s).", length(matched)),
               class = "text-muted")
      }
    })
  }


  observeEvent(input$apply_recipe, {
    req(input$recipe_pick)
    rec <- recipes[[input$recipe_pick]]
    recipe_label <- rec$name %||% input$recipe_pick

    flip_needed <- !is.null(rec$unit) && normalize_unit(rec$unit) %in% c("mmol/L", "mg/L", "\u00b5mol/L") &&
      !identical(normalize_unit(input$input_unit), normalize_unit(rec$unit))

    if (flip_needed) {
      updateRadioButtons(session, "input_unit", selected = normalize_unit(rec$unit))
      session$onFlushed(function() {
        if (!is.null(rec$targets)) {
          for (nm in nutrients) {
            val <- rec$targets[[nm]]
            if (!is.null(val)) {
              updateTextInput(session, paste0("expr_", nm), value = format(val, trim = TRUE))
            }
          }

          unit_rec <- normalize_unit(rec$unit %||% canonical_unit)
          vals_rec <- setNames(rep(0, length(nutrients)), nutrients)
          for (nm in nutrients) {
            if (!is.null(rec$targets[[nm]])) {
              vals_rec[[nm]] <- rec$targets[[nm]]
            }
          }
          vals_mmol <- to_canonical_from_unit(vals_rec, unit_rec)
          targets_mmol(vals_mmol)
        }

        apply_salts(rec)
        updateTabsetPanel(session, "input_tabs", selected = "\U0001f3af Nutrient Targets")
        showNotification(sprintf("Recipe '%s' applied.", recipe_label), type = "message")
      }, once = TRUE)

    } else {
      if (!is.null(rec$targets)) {
        for (nm in nutrients) {
          val <- rec$targets[[nm]]
          if (!is.null(val)) {
            updateTextInput(session, paste0("expr_", nm), value = format(val, trim = TRUE))
          }
        }

        unit_rec <- normalize_unit(rec$unit %||% canonical_unit)
        vals_rec <- setNames(rep(0, length(nutrients)), nutrients)
        for (nm in nutrients) {
          if (!is.null(rec$targets[[nm]])) {
            vals_rec[[nm]] <- rec$targets[[nm]]
          }
        }
        vals_mmol <- to_canonical_from_unit(vals_rec, unit_rec)
        targets_mmol(vals_mmol)
      }

      apply_salts(rec)
      updateTabsetPanel(session, "input_tabs", selected = "\U0001f3af Nutrient Targets")
      showNotification(sprintf("Recipe '%s' applied.", recipe_label), type = "message")
    }

  })

  output$status <- renderUI({
    miss <- setdiff(nutrients, colnames(nutrient_matrix))
    if (length(miss)) {
      tags$p(class = "text-danger", paste("nutrient_matrix missing *columns* for:", paste(miss, collapse = ", ")))
    } else {
      tags$p(class = "text-success", "\u2713 Ready.")
    }
  })

  # When user switches to the Tissue tab, ensure unit is mg/L
  observeEvent(input$input_tabs, {
    if (identical(input$input_tabs, "\U0001f9ec Tissue")) {
      if (!identical(normalize_unit(input$input_unit), "mg/L")) {
        updateRadioButtons(session, "input_unit", selected = "mg/L")
      }
    }
  })

  # When the user clicks "Use as targets", copy mg/L into the target fields
  observeEvent(input$tissue_apply, {
    mgL <- tissue_mgL()

    # helper to actually write values + update internal targets
    apply_mgL <- function() {
      # 1) first set all target inputs to 0 (in mg/L)
      for (nm in nutrients) {
        updateTextInput(
          session,
          paste0("expr_", nm),
          value = "0"
        )
      }

      # 2) then fill the target inputs with the calculated mg/L
      for (nm in nutrients) {
        updateTextInput(
          session,
          paste0("expr_", nm),
          value = format(mgL[[nm]], trim = TRUE, scientific = FALSE)
        )
      }

      # 3) update internal canonical targets (mmol/L) from mg/L
      vals_mmol <- to_canonical_from_unit(mgL, "mg/L")
      targets_mmol(vals_mmol)

      # 4) jump back to the Targets tab
      updateTabsetPanel(session, "input_tabs", selected = "\U0001f3af Nutrient Targets")
    }

    # If unit is not mg/L, first switch unit,
    # wait until all observers (input_unit) are done,
    # THEN write our zeros + mg/L values.
    if (!identical(normalize_unit(input$input_unit), "mg/L")) {
      updateRadioButtons(session, "input_unit", selected = "mg/L")
      session$onFlushed(function() {
        apply_mgL()
      }, once = TRUE)
    } else {
      # already in mg/L \u2192 just apply directly
      apply_mgL()
    }
  })



  # -------- OPTIMIZATION RESULT --------
  result <- eventReactive(run_trigger(), {
    sel <- selected_salts(); req(length(sel) > 0)

    # Determine whether acids/bases are allowed
    use_acid <- isTRUE(input$use_acid_base)

    # Get acid/base flag for selected salts
    is_acid_base_sel <- salt_info$is_acid_base[match(sel, salt_info$salt)]
    is_acid_base_sel[is.na(is_acid_base_sel)] <- FALSE

    # Effective selection depending on toggle
    if (use_acid) {
      sel_effective <- sel
    } else {
      # Drop acid/base salts when toggle is OFF
      sel_effective <- sel[!is_acid_base_sel]
    }

    # Need at least one usable salt
    req(length(sel_effective) > 0)

    nm_sub <- nutrient_matrix[sel_effective, , drop = FALSE]
    solver_target <- eval_targets()
    adjusted_target <- adjusted_targets()
    target_final <- target_with_multiplier()
    water_vals <- water_mmol()

    used_dummy <- FALSE
    dummy_name <- ".DUMMY_ZERO_SALT"

    # Pad to 2 rows if needed (nnls quirk)
    if (nrow(nm_sub) == 1) {
      zero_row <- matrix(
        0,
        nrow = 1,
        ncol = ncol(nm_sub),
        dimnames = list(dummy_name, colnames(nm_sub))
      )
      nm_pad <- rbind(nm_sub, zero_row)
      used_dummy <- TRUE
    } else {
      nm_pad <- nm_sub
    }

    # Choose solver depending on toggle
    if (use_acid) {
      out <- two_stage_optimize_nutrients(
        nutrient_matrix = nm_pad,
        target          = solver_target,
        importance      = importance_vec()
      )
    } else {
      out <- optimize_nutrients(
        nutrient_matrix = nm_pad,
        target          = solver_target,
        importance      = importance_vec()
      )
    }

    # Clean dummy
    if (!is.null(out$amounts)) {
      if (is.null(names(out$amounts))) {
        names(out$amounts) <- rownames(nm_pad)
      }
      if (used_dummy && dummy_name %in% names(out$amounts)) {
        out$amounts <- out$amounts[setdiff(names(out$amounts), dummy_name)]
      }
    }

    achieved_from_salts <- out$achieved
    if (!is.null(out$amounts) && !is.null(names(out$amounts))) {
      salt_names <- intersect(names(out$amounts), rownames(nm_sub))
      if (length(salt_names)) {
        nm_calc <- nm_sub[salt_names, names(solver_target), drop = FALSE]
        achieved_from_salts <- as.vector(t(nm_calc) %*% out$amounts[salt_names])
        names(achieved_from_salts) <- names(solver_target)
      }
    }
    if (is.null(names(achieved_from_salts))) {
      names(achieved_from_salts) <- names(solver_target)
    }
    achieved_from_salts <- achieved_from_salts[names(solver_target)]

    achieved_final <- achieved_from_salts + water_vals[names(solver_target)]
    abs_error <- achieved_final - target_final
    percent_error <- abs_error / target_final * 100
    percent_error[is.nan(percent_error) | is.infinite(percent_error)] <- NA

    tol <- 1e-12
    zero_tgt <- target_final == 0
    percent_error[zero_tgt & abs(achieved_final) < tol] <- 0

    rel_error <- abs_error / target_final
    rel_error[is.nan(rel_error) | is.infinite(rel_error)] <- NA
    rel_error[zero_tgt & abs(achieved_final) < tol] <- 0

    out$solver_target <- solver_target
    out$target <- target_final
    out$target_after_water <- adjusted_target
    out$water <- drop_tiny(water_vals[names(solver_target)])
    out$achieved_from_salts <- drop_tiny(achieved_from_salts)
    out$achieved_final <- drop_tiny(achieved_final)
    out$achieved <- drop_tiny(achieved_from_salts)
    out$abs_error <- drop_tiny(abs_error)
    out$percent_error <- drop_tiny(percent_error)
    out$squared_error <- sum(out$abs_error^2, na.rm = TRUE)
    out$rel_squared_error <- sum(rel_error^2, na.rm = TRUE)

    out$salt_names <- rownames(nm_sub)
    out
  }, ignoreInit = TRUE)

  # -------- DELIVERY TAB UI --------
  output$delivery_ui <- renderUI({
    res <- result()
    if (is.null(res)) return(tags$p("No result yet."))

    water    <- strip_negative_zero(round(drop_tiny(as.numeric(res$water)), 6))
    target   <- strip_negative_zero(round(drop_tiny(as.numeric(res$target)),   6))
    achieved <- strip_negative_zero(round(drop_tiny(as.numeric(res$achieved)), 6))
    final    <- strip_negative_zero(round(drop_tiny(as.numeric(res$achieved_final)), 6))
    abs_err  <- strip_negative_zero(round(drop_tiny(as.numeric(res$abs_error)), 6))
    pct_err  <- strip_negative_zero(round(as.numeric(res$percent_error), 2))

    format_delivery_value <- function(x) {
      if (is.na(x) || !is.finite(x)) return("")
      if (abs(x) <= 0.1) {
        paste0(formatC(x * 1000, format = "f", digits = 2), " \u00b5M")
      } else {
        paste0(formatC(x, format = "f", digits = 2), " mM")
      }
    }

    nd <- data.frame(
      Nutrient          = names(res$target),
      Water             = vapply(water, format_delivery_value, character(1)),
      Target            = vapply(target, format_delivery_value, character(1)),
      Achieved          = vapply(achieved, format_delivery_value, character(1)),
      Final             = vapply(final, format_delivery_value, character(1)),
      "Abs. Error"      = abs_err,
      "Pct. Error"      = pct_err,
      stringsAsFactors  = FALSE,
      check.names       = FALSE
    )
    nd$Nutrient <- vapply(
      nd$Nutrient,
      function(s) pretty_nutrient_label_str(safe_chr1(s)),
      character(1)
    )

    output$tbl_n <- renderTable(
      nd,
      sanitize.text.function = function(x) x,
      align = "lrrrrrr"
    )

    output$raw_print <- renderPrint({
      res0 <- result()
      if (!is.null(res0)) {
        vol0 <- input$vol %||% 1
        if (!is.numeric(vol0) || is.na(vol0) || vol0 <= 0) vol0 <- 1
        print(res0, vol = vol0)
      }
    })

    tagList(
      tags$h5("\U0001f3af Delivery vs target"),
      tableOutput("tbl_n"),
      tags$p(strong("\U0001f9ee Total squared absolute error: "), round(res$squared_error, 6)),
      tags$p(strong("\U0001f4ca Relative squared percentage error (optimized): "), round(res$rel_squared_error, 6)),
      tags$hr(),
      tags$details(
        tags$summary("Show raw print() output"),
        verbatimTextOutput("raw_print")
      )
    )
  })

  # -------- pH TAB UI --------
  output$ph_ui <- renderUI({
    res <- result()
    if (is.null(res)) return(tags$p("No result yet."))
    ph_fn <- NULL
    if (exists("ph_from_achieved", mode = "function")) {
      ph_fn <- ph_from_achieved
    } else if (requireNamespace("nutricalc", quietly = TRUE)) {
      ph_fn <- ph_from_achieved
    }
    if (is.null(ph_fn)) {
      return(tags$p("pH calculation not available."))
    }

  final_for_ph <- res$achieved_final
  final_for_ph <- c(final_for_ph, KS4_3 = water_hco3(), CO2_aq = water_co2_aq())
  # Debug: grep("EDTA", names(final_for_ph)); final_for_ph[["EDTA"]]

  ph_res <- tryCatch(
    ph_fn(final_for_ph, phc_bracket = c(1, 13)),
    error = function(e) e
  )

    if (inherits(ph_res, "error")) {
      return(tags$p(class = "text-warning", paste("pH calculation failed:", ph_res$message)))
    }

    fmt_sci_html <- function(x) {
      if (!is.finite(x)) return(as.character(x))
      formatted <- formatC(x, format = "e", digits = 2)
      parts <- strsplit(formatted, "e", fixed = TRUE)[[1]]
      mantissa <- parts[[1]]
      exp_raw <- parts[[2]]
      exp_clean <- sub("^\\+?", "", exp_raw)
      paste0(mantissa, "\u00d710<sup>", exp_clean, "</sup>")
    }

    fmt_num <- function(x, threshold = 0.01) {
      if (!is.finite(x)) return(as.character(x))
      if (abs(x) < threshold && x != 0) {
        fmt_sci_html(x)
      } else {
        formatC(x, format = "f", digits = 2)
      }
    }

    totals_meq <- ph_res$charge_breakdown$totals_meq
    charge_equiv <- NA_real_
    if (!is.null(totals_meq) &&
        all(c("pos_total_meq", "neg_total_meq") %in% names(totals_meq))) {
      charge_equiv <- (totals_meq[["pos_total_meq"]] + totals_meq[["neg_total_meq"]]) / 2
    }

    output$ph_fixed_cations <- renderTable({
      data.frame(
        Ion = names(ph_res$charge_breakdown$fixed_cations_meq),
        `meq/L` = vapply(unname(ph_res$charge_breakdown$fixed_cations_meq), fmt_num, character(1)),
        check.names = FALSE
      )
    }, sanitize.text.function = function(x) x)

    output$ph_fixed_anions <- renderTable({
      data.frame(
        Ion = names(ph_res$charge_breakdown$fixed_anions_meq),
        `meq/L` = vapply(unname(ph_res$charge_breakdown$fixed_anions_meq), fmt_num, character(1)),
        check.names = FALSE
      )
    }, sanitize.text.function = function(x) x)

    output$ph_variable <- renderTable({
      data.frame(
        Component = names(ph_res$charge_breakdown$variable_meq),
        `meq/L` = vapply(unname(ph_res$charge_breakdown$variable_meq), fmt_num, character(1)),
        check.names = FALSE
      )
    }, sanitize.text.function = function(x) x)

    output$ph_species <- renderTable({
      species_vals <- ph_res$charge_breakdown$species_mM
      if ("B" %in% names(res$achieved_final)) {
        species_vals <- c(species_vals, B = as.numeric(res$achieved_final[["B"]]))
      }
      if ("Si" %in% names(res$achieved_final)) {
        species_vals <- c(species_vals, Si = as.numeric(res$achieved_final[["Si"]]))
      }
      chelate_keys <- c("EDTA", "DTPA", "EDDHA", "HBED")
      for (key in chelate_keys) {
        if (key %in% names(final_for_ph)) {
          species_vals <- c(species_vals, stats::setNames(as.numeric(final_for_ph[[key]]), key))
        }
      }

      data.frame(
        Species = names(species_vals),
        `mmol/L` = vapply(unname(species_vals), fmt_num, character(1)),
        check.names = FALSE
      )
    }, sanitize.text.function = function(x) x)

    output$ph_totals <- renderTable({
      data.frame(
        Total = names(ph_res$charge_breakdown$totals_meq),
        `meq/L` = vapply(unname(ph_res$charge_breakdown$totals_meq), fmt_num, character(1)),
        check.names = FALSE
      )
    }, sanitize.text.function = function(x) x)

    ec_res <- NULL
    ec_fn <- NULL
    if (exists("ec_from_ph", mode = "function")) {
      ec_fn <- ec_from_ph
    } else if (requireNamespace("nutricalc", quietly = TRUE)) {
      ec_fn <- ec_from_ph
    }
    if (!is.null(ec_fn)) {
      ec_res <- tryCatch(
        ec_fn(final_for_ph, ph_res),
        error = function(e) e
      )
    }

    output$ec_contrib <- renderTable({
      if (inherits(ec_res, "error") || is.null(ec_res)) return(NULL)
      df <- ec_res$contributions
      df <- df[, setdiff(names(df), c("D_1e5_cm2_s", "gamma", "alpha")), drop = FALSE]
      df$kappa_mS_cm <- vapply(df$kappa_mS_cm, fmt_num, character(1))
      df$c_mM <- vapply(df$c_mM, fmt_num, character(1))
      df$z <- vapply(
        df$z,
        function(value) {
          formatted <- format(value, trim = TRUE, nsmall = 0, scientific = FALSE)
          sprintf("<div style='text-align:right'>%s</div>", formatted)
        },
        character(1)
      )
      if ("Lambda0_eq" %in% names(df)) {
        names(df)[names(df) == "Lambda0_eq"] <- "\u03bb\u2080(eq)"
      } else if ("Lambda0" %in% names(df)) {
        names(df)[names(df) == "Lambda0"] <- "\u03bb\u2080(eq)"
      }
      names(df)[names(df) == "kappa_mS_cm"] <- "\u03ba (mS/cm)"
      names(df)[names(df) == "c_mM"] <- "c (mM)"
      df
    }, sanitize.text.function = function(x) x)

    tags$div(
      tags$h5("\U0001f9ea pH & EC"),
      fluidRow(
        column(
          5,
          tags$p(strong("pH:"), sprintf("%.2f", ph_res$pH)),
          tags$p(strong("Ionic strength (mmol/L):"), sprintf("%.2f", ph_res$I * 1000)),
          tags$p(
            strong("Total charge equivalents (meq/L):"),
            fmt_num(charge_equiv)
          ),
          tags$details(
            tags$summary("Show pH charge breakdown"),
            tags$h6("Fixed cations (meq/L)"),
            tableOutput("ph_fixed_cations"),
            tags$h6("Fixed anions (meq/L)"),
            tableOutput("ph_fixed_anions"),
            tags$h6("Variable components (meq/L)"),
            tableOutput("ph_variable")
          )
        ),
        column(
          7,
          if (inherits(ec_res, "error")) {
            tags$p(class = "text-warning", paste("EC calculation failed:", ec_res$message))
          } else if (is.null(ec_res)) {
            tags$p("EC calculation not available.")
          } else {
            tagList(
              tags$p(strong("EC (mS/cm):"), sprintf("%.2f", ec_res$EC_mS_cm)),
              tags$p(strong("EC (\u00b5S/cm):"), sprintf("%.2f", ec_res$EC_uS_cm)),
              tags$p(
                strong("Osmotic Potential (kPa):"),
                sprintf("%.2f", -0.036 * ec_res$EC_uS_cm)
              ),
              tags$details(
                tags$summary("Show EC contributions"),
                tableOutput("ec_contrib")
              )
            )
          }
        )
      ),
      tags$p(
        class = "text-muted",
        "Estimated at 25\u00b0C with Davies activity correction; no complexation assumed."
      )
    )
  })

  # -------- FERTILIZER TAB UI (A/B/Micro with alignment) --------
  output$fertilizer_ui <- renderUI({
    res <- result()
    if (is.null(res)) return(tags$p("No result yet."))

    # --- working solution volume ---
    vol <- input$vol %||% 1
    if (!is.numeric(vol) || is.na(vol) || vol <= 0) vol <- 1

    # --- stock parameters (Option A) ---
    stock_factor <- input$stock_factor %||% 1
    if (!is.numeric(stock_factor) || is.na(stock_factor) || stock_factor <= 0) {
      stock_factor <- NA_real_
    }

    stock_vol <- input$stock_vol %||% NA_real_
    if (!is.numeric(stock_vol) || is.na(stock_vol) || stock_vol <= 0) {
      stock_vol <- NA_real_
    }

    # BAK grouping
    bak <- assign_salts_bak(res, nutrient_matrix)

    amt_df <- bak$table
    if (nrow(amt_df) == 0) {
      return(tags$p("No fertilizers used in this solution."))
    }

    # order by tank (A, B, Micro) then salt
    tank_factor <- factor(
      amt_df$tank,
      levels = c("A", "B", "Micro")
    )
    ord <- order(tank_factor, amt_df$salt)
    amt_df <- amt_df[ord, , drop = FALSE]

    formulas <- amt_df$salt
    mmol_l   <- amt_df$mmol_l

    mmol_str <- ifelse(is.na(mmol_l), "", sprintf("%.2f", mmol_l))

    df_out <- data.frame(
      Formula    = formulas,
      "mmol l\u207b\u00b9" = mmol_str,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # columns that should be bold (only working-solution column)
    bold_cols <- character(0)

    if (exists("compute_molar_mass", mode = "function")) {
      mm   <- sapply(formulas, function(f) compute_molar_mass(f))
      mg_l <- mmol_l * mm

      # totals for working solution
      g_total_work  <- mg_l * vol / 1000    # g for macros
      mg_total_work <- mg_l * vol           # mg for micros

      df_out[["g mol\u207b\u00b9"]] <- sprintf("%.2f", mm)
      df_out[["mg l\u207b\u00b9"]]  <- sprintf("%.2f", mg_l)

      macro_rows <- amt_df$tank %in% c("A", "B")
      micro_rows <- amt_df$tank == "Micro"

      # ---- column: working solution total (this one should be bold) ----
      vol_label <- if (abs(vol - round(vol)) < 1e-9) as.integer(vol) else vol
      work_col  <- sprintf("%s l", vol_label)

      total_work <- character(length(mmol_l))
      total_work[macro_rows] <- sprintf("%.2f g ",  g_total_work[macro_rows])
      total_work[micro_rows] <- sprintf("%.2f mg", mg_total_work[micro_rows])
      total_work[is.na(total_work)] <- ""
      df_out[[work_col]] <- total_work

      bold_cols <- c(bold_cols, work_col)  # ONLY this column is bold

      # ---- column: stock solution total (not bold) ----
      if (!is.na(stock_factor) && !is.na(stock_vol)) {
        g_total_stock  <- mg_l * stock_factor * stock_vol / 1000
        mg_total_stock <- mg_l * stock_factor * stock_vol

        stock_vol_label <- if (abs(stock_vol - round(stock_vol)) < 1e-9) as.integer(stock_vol) else stock_vol
        stock_col <- sprintf(" %s l of %s-fold", stock_vol_label, stock_factor)

        total_stock <- character(length(mmol_l))
        total_stock[macro_rows] <- sprintf("%.2f g ",  g_total_stock[macro_rows])
        total_stock[micro_rows] <- sprintf("%.2f mg", mg_total_stock[micro_rows])
        total_stock[is.na(total_stock)] <- ""

        df_out[[stock_col]] <- total_stock
        # NOTE: we intentionally do NOT add stock_col to bold_cols
      }
    }

    # pretty Formula HTML
    df_out$Formula <- vapply(
      df_out$Formula,
      function(s) format_formula_html(safe_chr1(s)),
      character(1)
    )

    col_names <- colnames(df_out)
    n_cols    <- length(col_names)

    # align numeric columns via monospace + padding
    for (cn in col_names[col_names != "Formula"]) {
      vals <- as.character(df_out[[cn]])
      w <- max(nchar(vals, type = "width"), na.rm = TRUE)
      vals <- ifelse(
        vals == "" | is.na(vals),
        "",
        sprintf(paste0("%", w, "s"), vals)
      )
      df_out[[cn]] <- vals
    }

    output$tbl_amt_grouped <- renderUI({
      rows <- list()

      # header row
      header_row <- tags$tr(
        lapply(col_names, function(cn) tags$th(cn))
      )
      rows <- c(rows, list(header_row))

      add_group <- function(tank_code, label) {
        sub <- df_out[amt_df$tank == tank_code, , drop = FALSE]
        if (!nrow(sub)) return()

        group_row <- tags$tr(
          tags$td(
            colspan = n_cols,
            style   = "font-weight:bold; border-top:2px solid #6c757d; background-color:#f8f9fa;",
            label
          )
        )
        rows <<- c(rows, list(group_row))

        for (i in seq_len(nrow(sub))) {
          row_vals <- sub[i, , drop = FALSE]
          r <- tags$tr(
            lapply(seq_len(n_cols), function(j) {
              val <- row_vals[[j]]

              if (col_names[j] == "Formula") {
                tags$td(HTML(val))
              } else {
                # bold only in the working-solution total column ("for X l")
                is_bold_col <- col_names[j] %in% bold_cols
                style_str <- "font-family: monospace; white-space: pre;"
                if (is_bold_col) {
                  style_str <- paste0(style_str, " font-weight:bold;")
                }
                tags$td(
                  val,
                  style = style_str
                )
              }
            })
          )
          rows <<- c(rows, list(r))
        }
      }

      add_group("A",     "A-BAK")
      add_group("B",     "B-BAK")
      add_group("Micro", "Micro")

      tags$div(
        tags$table(
          class = "table table-sm",
          rows
        )
      )
    })

    tagList(
      tags$h5("\U0001f9c2 Fertilizer amounts"),
      uiOutput("tbl_amt_grouped")
    )
  })




  # ---- ternary plots ----
  output$ternary_anions <- renderPlot({
    req(result()); req(exists("plot_ternary", mode = "function"))
    validate(need(length(input$anion_keys) == 3, "Pick exactly 3 anions"))
    keys <- input$anion_keys
    plt <- plot_ternary(result = result(), keys = keys,
                        title = sprintf("Anions (%s : %s : %s)", keys[1], keys[2], keys[3]))
    if (!is.null(plt)) print(plt)
  })

  output$ternary_cations <- renderPlot({
    req(result()); req(exists("plot_ternary", mode = "function"))
    validate(need(length(input$cation_keys) == 3, "Pick exactly 3 cations"))
    keys <- input$cation_keys
    plt <- plot_ternary(result = result(), keys = keys,
                        title = sprintf("Cations (%s : %s : %s)", keys[1], keys[2], keys[3]))
    if (!is.null(plt)) print(plt)
  })
}

shinyApp(ui, server)
