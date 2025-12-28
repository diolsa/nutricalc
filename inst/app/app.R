# app.R — NutriCalc

library(shiny)
library(bslib)
library(Ternary)
library(xml2)

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

default_water_expr <- setNames(rep("0", length(nutrients)), nutrients)
default_water_hco3_expr <- "0"

default_water_mmol <- {
  v <- as.numeric(default_water_expr)
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

hco3_to_mmol <- function(val, unit_in) {
  unit_in <- normalize_unit(unit_in)
  molar_mass_hco3 <- 61.016
  if (unit_in == "mmol/L") {
    val
  } else if (unit_in == "µmol/L") {
    val / 1000
  } else if (unit_in == "mg/L") {
    val / molar_mass_hco3
  } else {
    stop("Unsupported input unit: ", unit_in, call. = FALSE)
  }
}

hco3_from_mmol <- function(val, unit_out) {
  unit_out <- normalize_unit(unit_out)
  molar_mass_hco3 <- 61.016
  if (unit_out == "mmol/L") {
    val
  } else if (unit_out == "µmol/L") {
    val * 1000
  } else if (unit_out == "mg/L") {
    val * molar_mass_hco3
  } else {
    stop("Unsupported output unit: ", unit_out, call. = FALSE)
  }
}

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



")),



  ),

  titlePanel("NutriCalc 🌿 Nutrient Optimization"),

  fluidRow(
    # LEFT: inputs
    column(
      width = 6,
      div(class = "card p-3",
          h5("⚗️ Nutrient Targets & Fertilizers"),
          tabsetPanel(id = "input_tabs", type = "tabs",
                      tabPanel("🎯 Nutrient Targets",
                               fluidRow(
          column(2, strong("Nutrient")),
          column(4,
                fluidRow(
                  column(6, strong(textOutput("target_col_header"))),
                  column(6, strong("Water"))
                ),
                div(
                                          radioButtons("input_unit", label = NULL,
                                                       choices = c("mmol/L", "µmol/L", "mg/L"),
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
                      tabPanel("💦 Water",
                               fluidRow(
                                 column(6,
                                        textInput("bwb_plz", "BWB postal code", placeholder = "e.g. 10115", width = "100%"),
                                        tags$small(class = "text-muted", "Fetch Berliner Wasserbetriebe Mittelwert data and fill water inputs under Targets.")),
                                 column(6,
                                        br(),
                                        actionButton("apply_bwb", "Load into water inputs", class = "btn btn-primary btn-sm"),
                                        tags$br(), tags$br(),
                                        tags$small(class = "text-muted", "Values respect the current unit selection above."))
                               )
                      ),
                      tabPanel("🧂 Fertilizers",
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
                      tabPanel("📚 Recipes",
                               fluidRow(
                                 column(
                                   width = 4,
                                   radioButtons(
                                     "recipe_category", div(strong("Category")),
                                     choices  = c(
                                       "🥬 Olericulture"           = "Olericulture",
                                       "🍓 Fruticulture"           = "Fruticulture",
                                       "🌸 Floriculture"           = "Floriculture",
                                       "⚙️ Standard Formulations"  = "Standard Formulations"
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
                      tabPanel("🧬 Tissue",
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
                                     lapply(nutrients, function(nm) {
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
                                              "Calculated mg L⁻¹ from tissue % × WUE."),
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
                "run", "🧪 Result",
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
          h5("🧪 Result & Plots"),
          tabsetPanel(id = "result_tabs", type = "tabs",
                      tabPanel("🗒Result",
                               uiOutput("delivery_ui")
                      ),
                      tabPanel("🧪 pH & EC",
                               uiOutput("ph_ui")
                      ),
                      tabPanel("🛢️ Solution",
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
                                     "stock_factor", "Stock factor (×)",
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
                      tabPanel("➖ Anions",
                               tags$p(class = "text-muted", "Select exactly three anions (defaults: NO3–N, P, S)."),
                               selectizeInput("anion_keys", label = NULL,
                                              choices = c("NO3_N", "P", "S", "Cl"),
                                              selected = c("NO3_N", "P", "S"),
                                              multiple = TRUE,
                                              options = list(maxItems = 3, plugins = list("remove_button"))
                               ),
                               div(style = "display:flex; justify-content:center; align-items:center;",
                                   plotOutput("ternary_anions", height = "666px"))
                      ),
                      tabPanel("➕ Cations",
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
  water_hco3 <- reactiveVal(hco3_to_mmol(as.numeric(default_water_hco3_expr), canonical_unit))
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
        column(2, tags$label("HCO3")),
        column(2, tags$div()),
        column(2, textInput(inputId = "water_expr_HCO3", label = NULL, value = default_water_hco3_expr)),
        column(6, tags$div())
      )
    )

    tagList(c(rows, list(hco3_row)))
  })

  inputs_ready <- reactive({
    all(vapply(nutrients, function(nm) {
      !is.null(input[[paste0("expr_", nm)]]) &&
        !is.null(input[[paste0("water_expr_", nm)]])
    }, logical(1))) && !is.null(input$water_expr_HCO3)
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
    for (nm in nutrients) {
      updateTextInput(session, paste0("expr_", nm), value = "0")
    }
    targets_mmol(setNames(rep(0, length(nutrients)), nutrients))
  })

  observeEvent(input$apply_bwb, {
    plz <- input$bwb_plz %||% ""
    plz <- trimws(plz)
    if (!nzchar(plz)) {
      showNotification("Please enter a postal code.", type = "error")
      return()
    }

    vals_mmol <- tryCatch(
      fetch_bwb_mittelwert(plz),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )

    if (is.null(vals_mmol)) return()

    vals_mmol[is.na(vals_mmol)] <- 0
    water_mmol(vals_mmol[nutrients])
    water_hco3_mmol <- vals_mmol[["Alkalinity"]]
    if (is.na(water_hco3_mmol)) water_hco3_mmol <- 0
    water_hco3(water_hco3_mmol)

    unit_out <- current_input_unit() %||% canonical_unit
    display_water <- targets_for_display(vals_mmol[nutrients], unit_out)
    for (nm in nutrients) {
      updateTextInput(
        session, paste0("water_expr_", nm),
        value = format(display_water[[nm]], trim = TRUE, scientific = FALSE)
      )
    }
    updateTextInput(
      session,
      "water_expr_HCO3",
      value = format(hco3_from_mmol(water_hco3_mmol, unit_out), trim = TRUE, scientific = FALSE)
    )

    showNotification("BWB water values applied.", type = "message")
    updateTabsetPanel(session, "input_tabs", selected = "🎯 Targets")
  })

  observeEvent(input$run, {
    req(inputs_ready())
    unit_in <- input$input_unit %||% canonical_unit
    vals_mmol <- parse_targets_from_inputs(input, nutrients, unit_in, prefix = "expr_")
    vals_water_mmol <- parse_targets_from_inputs(input, nutrients, unit_in, prefix = "water_expr_")
    targets_mmol(vals_mmol)
    water_mmol(vals_water_mmol)
    hco3_val <- safe_numeric_expr(input$water_expr_HCO3, default = 0)
    water_hco3(hco3_to_mmol(hco3_val, unit_in))
    run_trigger(isolate(run_trigger()) + 1L)
  })

  observeEvent(input$input_unit, {
    req(inputs_ready())

    new_unit <- input$input_unit %||% canonical_unit
    old_unit <- current_input_unit() %||% canonical_unit

    vals_mmol <- parse_targets_from_inputs(input, nutrients, old_unit, prefix = "expr_")
    vals_water_mmol <- parse_targets_from_inputs(input, nutrients, old_unit, prefix = "water_expr_")
    targets_mmol(vals_mmol)
    water_mmol(vals_water_mmol)
    hco3_val <- safe_numeric_expr(input$water_expr_HCO3, default = 0)
    water_hco3(hco3_to_mmol(hco3_val, old_unit))

    display_vals <- targets_for_display(vals_mmol, new_unit)
    display_water <- targets_for_display(vals_water_mmol, new_unit)

    for (nm in nutrients) {
      updateTextInput(
        session, paste0("expr_", nm),
        value = format(display_vals[[nm]], trim = TRUE, scientific = FALSE)
      )

      updateTextInput(
        session, paste0("water_expr_", nm),
        value = format(display_water[[nm]], trim = TRUE, scientific = FALSE)
      )
    }
    updateTextInput(
      session,
      "water_expr_HCO3",
      value = format(hco3_from_mmol(water_hco3(), new_unit), trim = TRUE, scientific = FALSE)
    )

    current_input_unit(new_unit)
  }, ignoreInit = TRUE)

  eval_targets <- reactive({
    vals <- targets_mmol()
    water_vals <- water_mmol()
    validate(
      need(!is.null(vals), "Targets not initialized yet."),
      need(!is.null(water_vals), "Water not initialized yet.")
    )

    pmax(vals - water_vals, 0)
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
    if (is.na(w)) return("WUE: —")
    sprintf("WUE: %.2f g dry mass per L water", w)
  })


  tissue_mgL <- reactive({
    w <- tissue_wue()
    if (is.na(w)) {
      out <- setNames(rep(0, length(nutrients)), nutrients)
      return(out)
    }

    # % in tissue (g per 100 g dry mass)
    perc <- sapply(nutrients, function(nm) {
      val <- input[[paste0("tissue_", nm)]] %||% NA_real_
      ifelse(is.na(val), 0, as.numeric(val))
    })
    names(perc) <- nutrients

    # convert % -> mg/g : 1% = 10 mg/g
    mg_per_g <- perc * 10

    # mg/L = (mg/g) * (g/L)
    mgL <- mg_per_g * w
    names(mgL) <- nutrients
    mgL
  })

  output$tissue_table <- renderTable({
    w <- tissue_wue()


    perc <- sapply(nutrients, function(nm) {
      val <- input[[paste0("tissue_", nm)]] %||% NA_real_
      ifelse(is.na(val), NA_real_, as.numeric(val))
    })
    names(perc) <- nutrients

    mgL <- tissue_mgL()

    data.frame(
      Nutrient   = vapply(nutrients, pretty_nutrient_label_str, character(1)),
      `Tissue %` = round(perc, 3),
      `mg l⁻¹`   = round(mgL, 3),
      check.names = FALSE
    )
  }, sanitize.text.function = function(x) x)





  # ---- Salts --------------------------
  all_salts <- reactive(rownames(nutrient_matrix))

  filtered_salts <- reactive({
    salts <- all_salts()
    q <- input$salt_search

    # 1) no search → return all salts (we still sort below)
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
      # no salt_info → just sort by formula
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

    if (!is.null(rec$unit) && rec$unit %in% c("mmol/L", "mg/L", "µmol/L", "umol/L")) {
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

    flip_needed <- !is.null(rec$unit) && normalize_unit(rec$unit) %in% c("mmol/L", "mg/L", "µmol/L") &&
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
        updateTabsetPanel(session, "input_tabs", selected = "🎯 Targets")
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
      updateTabsetPanel(session, "input_tabs", selected = "🎯 Targets")
      showNotification(sprintf("Recipe '%s' applied.", recipe_label), type = "message")
    }

  })

  output$status <- renderUI({
    miss <- setdiff(nutrients, colnames(nutrient_matrix))
    if (length(miss)) {
      tags$p(class = "text-danger", paste("nutrient_matrix missing *columns* for:", paste(miss, collapse = ", ")))
    } else {
      tags$p(class = "text-success", "✓ Ready.")
    }
  })

  # When user switches to the Tissue tab, ensure unit is mg/L
  observeEvent(input$input_tabs, {
    if (identical(input$input_tabs, "🧬 Tissue")) {
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
      updateTabsetPanel(session, "input_tabs", selected = "🎯 Targets")
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
      # already in mg/L → just apply directly
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
        target          = eval_targets(),
        importance      = importance_vec()
      )
    } else {
      out <- optimize_nutrients(
        nutrient_matrix = nm_pad,
        target          = eval_targets(),
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

    out$salt_names <- rownames(nm_sub)
    out
  }, ignoreInit = TRUE)

  # -------- DELIVERY TAB UI --------
  output$delivery_ui <- renderUI({
    res <- result()
    if (is.null(res)) return(tags$p("No result yet."))

    target   <- strip_negative_zero(round(drop_tiny(as.numeric(res$target)),   6))
    achieved <- strip_negative_zero(round(drop_tiny(as.numeric(res$achieved)), 6))
    abs_err  <- strip_negative_zero(round(drop_tiny(as.numeric(res$abs_error)), 6))
    pct_err  <- strip_negative_zero(round(as.numeric(res$percent_error), 2))

    nd <- data.frame(
      Nutrient          = names(res$target),
      Target            = target,
      Achieved          = achieved,
      "Absolute Error"  = abs_err,
      "Percent Error"   = pct_err,
      stringsAsFactors  = FALSE,
      check.names       = FALSE
    )
    nd$Nutrient <- vapply(
      nd$Nutrient,
      function(s) pretty_nutrient_label_str(safe_chr1(s)),
      character(1)
    )

    output$tbl_n <- renderTable(nd, sanitize.text.function = function(x) x)

    output$raw_print <- renderPrint({
      res0 <- result()
      if (!is.null(res0)) {
        vol0 <- input$vol %||% 1
        if (!is.numeric(vol0) || is.na(vol0) || vol0 <= 0) vol0 <- 1
        print(res0, vol = vol0)
      }
    })

    tagList(
      tags$h5("🎯 Delivery vs target (mmol l⁻¹)"),
      tableOutput("tbl_n"),
      tags$p(strong("🧮 Total squared absolute error: "), round(res$squared_error, 6)),
      tags$p(strong("📊 Relative squared percentage error (optimized): "), round(res$rel_squared_error, 6)),
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
      ph_fn <- nutricalc::ph_from_achieved
    }
    if (is.null(ph_fn)) {
      return(tags$p("pH calculation not available."))
    }

  final_for_ph <- water_mmol()
  add <- res$achieved
  for (nm in names(add)) {
    final_for_ph[[nm]] <- (final_for_ph[[nm]] %||% 0) + as.numeric(add[[nm]])
  }
  final_for_ph <- c(final_for_ph, Alkalinity = water_hco3())

  ph_res <- tryCatch(
    ph_fn(final_for_ph, phc_bracket = c(1, 13)),
    error = function(e) e
  )

    if (inherits(ph_res, "error")) {
      return(tags$p(class = "text-warning", paste("pH calculation failed:", ph_res$message)))
    }

    fmt_small <- function(x) {
      ifelse(abs(x) < 0.001, format(signif(x, 6), scientific = TRUE), format(signif(x, 6), trim = TRUE))
    }
    fmt_total <- function(x) {
      format(round(x, 6), nsmall = 4, trim = TRUE)
    }

    output$ph_fixed_cations <- renderTable({
      data.frame(
        Ion = names(ph_res$charge_breakdown$fixed_cations_meq),
        `meq/L` = fmt_small(unname(ph_res$charge_breakdown$fixed_cations_meq)),
        check.names = FALSE
      )
    }, sanitize.text.function = function(x) x)

    output$ph_fixed_anions <- renderTable({
      data.frame(
        Ion = names(ph_res$charge_breakdown$fixed_anions_meq),
        `meq/L` = fmt_small(unname(ph_res$charge_breakdown$fixed_anions_meq)),
        check.names = FALSE
      )
    }, sanitize.text.function = function(x) x)

    output$ph_variable <- renderTable({
      data.frame(
        Component = names(ph_res$charge_breakdown$variable_meq),
        `meq/L` = fmt_small(unname(ph_res$charge_breakdown$variable_meq)),
        check.names = FALSE
      )
    }, sanitize.text.function = function(x) x)

    output$ph_species <- renderTable({
      species_vals <- ph_res$charge_breakdown$species_mM
      if ("B" %in% names(res$achieved)) {
        species_vals <- c(species_vals, B = as.numeric(res$achieved[["B"]]))
      }
      if ("Si" %in% names(res$achieved)) {
        species_vals <- c(species_vals, Si = as.numeric(res$achieved[["Si"]]))
      }

      data.frame(
        Species = names(species_vals),
        `mmol/L` = fmt_small(unname(species_vals)),
        check.names = FALSE
      )
    }, sanitize.text.function = function(x) x)

    output$ph_totals <- renderTable({
      data.frame(
        Total = names(ph_res$charge_breakdown$totals_meq),
        `meq/L` = fmt_total(unname(ph_res$charge_breakdown$totals_meq)),
        check.names = FALSE
      )
    }, sanitize.text.function = function(x) x)

    ec_res <- NULL
    ec_fn <- NULL
    if (exists("ec_from_ph", mode = "function")) {
      ec_fn <- ec_from_ph
    } else if (requireNamespace("nutricalc", quietly = TRUE)) {
      ec_fn <- nutricalc::ec_from_ph
    }
    if (!is.null(ec_fn)) {
      ec_res <- tryCatch(
      ec_fn(res$achieved, ph_res),
      error = function(e) e
    )
  }

    output$ec_contrib <- renderTable({
      if (inherits(ec_res, "error") || is.null(ec_res)) return(NULL)
      df <- ec_res$contributions
      df$kappa_mS_cm <- fmt_small(df$kappa_mS_cm)
      df$c_mM <- fmt_small(df$c_mM)
      df
    }, sanitize.text.function = function(x) x)

    tags$div(
      tags$h5("🧪 pH & EC"),
      tags$p(strong("pH:"), sprintf("%.2f", ph_res$pH)),
      tags$p(strong("Ionic strength (mol/L):"), sprintf("%.2f", ph_res$I)),
      tags$hr(),
      tags$h6("Fixed cations (meq/L)"),
      tableOutput("ph_fixed_cations"),
      tags$h6("Fixed anions (meq/L)"),
      tableOutput("ph_fixed_anions"),
      tags$h6("Variable components (meq/L)"),
      tableOutput("ph_variable"),
      tags$h6("Charge totals (meq/L)"),
      tableOutput("ph_totals"),
      tags$hr(),
      tags$h6("Estimated EC (25°C)"),
      if (inherits(ec_res, "error")) {
        tags$p(class = "text-warning", paste("EC calculation failed:", ec_res$message))
      } else if (is.null(ec_res)) {
        tags$p("EC calculation not available.")
      } else {
        tagList(
          tags$p(strong("EC (mS/cm):"), sprintf("%.2f", ec_res$EC_mS_cm)),
          tags$p(strong("EC (µS/cm):"), sprintf("%.2f", ec_res$EC_uS_cm)),
          tags$h6("EC contributions"),
          tableOutput("ec_contrib")
        )
      },
      tags$p(
        class = "text-muted",
        "Estimated at 25°C with Davies activity correction; no carbonate or complexation assumed."
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
      "mmol l⁻¹" = mmol_str,
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

      df_out$`g mol⁻¹` <- sprintf("%.2f", mm)
      df_out$`mg l⁻¹`  <- sprintf("%.2f", mg_l)

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
      tags$h5("🧂 Fertilizer amounts"),
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
