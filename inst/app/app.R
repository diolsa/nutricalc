# app.R — NutriCalc with Results/Plots tabs and exact-3 ion selection via selectizeInput

library(shiny)
library(bslib)
library(Ternary)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- helpers (minimal, no extra packages) ----
# Pretty-print chemical formulas (for salts and ions)
format_formula_html <- function(x) {
  if (is.na(x) || !nzchar(x)) return(x)
  # Hydrate middle dot
  x <- gsub("·", "&middot;", x, fixed = TRUE)
  # Charges like ^2- or ^+ -> <sup>2-</sup>, <sup>+</sup>
  x <- gsub("\\^([0-9]+)?([+-])", "<sup>\\1\\2</sup>", x, perl = TRUE)
  # Trailing lone charge like 'Cl-' -> <sup>-</sup>
  x <- sub("([+-])$", "<sup>\\1</sup>", x, perl = TRUE)
  # Subscript ONLY numbers that follow an element symbol (A, Ab) or ')'
  repeat {
    new <- gsub("((?:[A-Z][a-z]?|\\)))(\\d+)", "\\1<sub>\\2</sub>", x, perl = TRUE)
    if (identical(new, x)) break
    x <- new
  }
  x
}

# Guard so vapply() never chokes on weird inputs
safe_chr1 <- function(x) {
  x <- as.character(x)
  if (length(x) != 1 || is.na(x)) return("")
  x
}

# Optional: prettier nutrient labels (only used in the left labels)
pretty_nutrient_label_str <- function(nm) {
  switch(nm,
         "NO3_N" = "NO<sub>3</sub>&ndash;N",
         "NH4_N" = "NH<sub>4</sub>&ndash;N",
         nm
  )
}

# ---- defaults ----
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

# ---- UI ----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(tags$style(HTML("
  body { font-size: 13px; }
  .form-group { margin-bottom: 3px; }
  .shiny-input-container { margin-bottom: 3px; }
  .irs { margin-top: -8px; }
  .container-fluid { max-width: 1300px; }
  input.form-control { height: 26px; padding: 2px 4px; font-size: 12px; }
  .irs-single, .irs-min, .irs-max { font-size: 10px; }
  .form-group.shiny-input-slider { margin-top: -8px; }
  .importance-slider .irs-min, .importance-slider .irs-max, .importance-slider .irs-single { display: none !important; }
  .importance-slider .irs { margin-top: -16px; }
  .importance-slider { margin-bottom: 10px; }
  #salt-search-row { margin-bottom: 6px; }
  #selected_saltes { max-height: 800px; overflow-y: auto; padding: 6px 1px 1px; }
  #input_unit label { color: #6c757d !important; font-size: 0.9em !important; font-weight: normal !important; margin-right: 6px !important; }
  #input_unit .radio-inline { margin-right: 6px !important; }
  #select_all_visible, #deselect_all_salts, #clear_search { width: 30px !important; height: 30px !important; padding: 0 !important; display: inline-flex; align-items: center; justify-content: center; background-color: #e9ecef !important; border: 1px solid rgba(0,0,0,0.25) !important; border-radius: 4px !important; box-shadow: inset 0 1px 1px rgba(0,0,0,0.075); margin: 3px; }
  #select_all_visible i, #deselect_all_salts i, #clear_search i { color: #212529 !important; font-size: 16px; line-height: 1; margin: 3px; }
  #select_all_visible:hover, #deselect_all_salts:hover, #clear_search:hover { background-color: #dee2e6 !important; }
  .search-inline .shiny-input-container { margin-bottom: 0 !important; flex: 1; }
  .search-inline .form-control { height: 30px !important; padding: 4px 8px !important; line-height: 1.2 !important; }
  #clear_search { height: 30px !important; padding: 0 10px !important; display: inline-flex; align-items: center; justify-content: center; }
"))),

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
                                 column(3, strong("Nutrient")),
                                 column(4,
                                        div(
                                          strong(textOutput("target_col_header")),
                                          radioButtons("input_unit", label = NULL,
                                                       choices = c("mmol/L", "mg/L"), selected = "mmol/L", inline = TRUE)
                                        )
                                 ),
                                 column(5,
                                        div(strong("Priority"), tags$br(), tags$small(class = "text-muted", "Weight for target matching"))
                                 )
                               ),
                               tags$hr(style = "margin:4px 0;"),
                               uiOutput("nutrient_rows"),
                               br(),
                               actionButton("set_all_zero", "Set all to 0", class = "btn btn-light"),
                               span("  "), actionButton("reset", "Reset to defaults"),
                               span("  "), actionButton("run", "🧪 Results", class = "btn btn-primary"),
                               br(), br(), uiOutput("status")
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
                               tags$div(style = "margin:12px 0;"),
                               uiOutput("salt_picker"),
                               uiOutput("sel_status")
                      )
          )
      )
    ),

    # RIGHT: Results & Plots tabs
    column(
      width = 6,
      div(class = "card p-3",
          h5("🧪 Results & Plots"),
          tabsetPanel(id = "results_tabs", type = "tabs",
                      # Results
                      tabPanel("🧪 Results",
                               numericInput("vol", label = "Volume (L)", value = 1, min = 0.00, step = 1, width = "200px"),
                               uiOutput("results_ui")
                      ),
                      # Anions (select exactly 3)
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
                      # Cations (select exactly 3)
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
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Source-of-truth selection (search never mutates this automatically)
  selected_salts <- reactiveVal(rownames(nutrient_matrix))

  # Header label reacts to unit selection
  output$target_col_header <- renderText({
    unit <- if (is.null(input$input_unit)) "mmol/L" else input$input_unit
    sprintf("Target (%s)", unit)
  })

  # build rows dynamically
  output$nutrient_rows <- renderUI({
    tagList(lapply(nutrients, function(nm) {
      fluidRow(
        column(3, tags$label(HTML(pretty_nutrient_label_str(nm)))),
        column(4, textInput(inputId = paste0("expr_", nm), label = NULL, value = default_expr[[nm]])),
        column(5, div(class = "importance-slider",
                      sliderInput(inputId = paste0("imp_", nm), label = NULL, min = -2, max = 2, step = 1, value = default_importance[[nm]])))
      )
    }))
  })

  # reset button
  observeEvent(input$reset, {
    for (nm in nutrients) {
      updateTextInput(session, paste0("expr_", nm), value = default_expr[[nm]])
      updateSliderInput(session, paste0("imp_", nm), value = default_importance[[nm]])
    }
  })

  # Set all target expressions to zero
  observeEvent(input$set_all_zero, {
    for (nm in nutrients) updateTextInput(session, paste0("expr_", nm), value = "0")
  })

  # evaluate target expressions -> numeric (in selected unit), then convert to mmol/L for solver
  eval_targets <- reactive({
    vals <- sapply(nutrients, function(nm) {
      txt <- input[[paste0("expr_", nm)]]
      val <- try(eval(parse(text = txt), envir = .GlobalEnv), silent = TRUE)
      if (inherits(val, "try-error") || !is.numeric(val) || length(val) != 1)
        stop(sprintf("Error evaluating target for %s: '%s'", nm, txt))
      as.numeric(val)
    })
    names(vals) <- nutrients

    unit_in <- if (is.null(input$input_unit)) "mmol/L" else input$input_unit

    if (unit_in == "mg/L") {
      validate(need(exists("convert_units", mode = "function"),
                    "convert_units() not found (check R/converte_unit.R)."))
      vals <- convert_units(vals, to = "mmol/L")  # mg/L -> mmol/L
    }

    vals  # mmol/L
  })

  # Helper: ensure inputs exist before running conversion observer
  inputs_ready <- reactive({
    all(vapply(nutrients, function(nm) !is.null(input[[paste0("expr_", nm)]]), logical(1)))
  })

  # Convert all visible inputs when the unit toggle changes (mmol/L <-> mg/L)
  observeEvent(input$input_unit, {
    req(inputs_ready())
    req(exists("convert_units", mode = "function"))
    req(exists("nutrient_element_map", inherits = TRUE))
    req(exists("element_molar_mass_df", inherits = TRUE))

    to_unit <- input$input_unit

    current_vals <- sapply(nutrients, function(nm) {
      txt <- input[[paste0("expr_", nm)]]
      val <- try(eval(parse(text = txt), envir = .GlobalEnv), silent = TRUE)
      if (inherits(val, "try-error") || !is.numeric(val) || length(val) != 1) {
        v2 <- suppressWarnings(as.numeric(txt))
        if (is.na(v2)) v2 <- 0
        val <- v2
      }
      as.numeric(val)
    })
    names(current_vals) <- nutrients

    converted_vals <- try(convert_units(current_vals, to = to_unit), silent = TRUE)
    if (inherits(converted_vals, "try-error")) return(NULL)

    for (nm in nutrients) {
      updateTextInput(session, paste0("expr_", nm),
                      value = format(round(converted_vals[[nm]], 2), trim = TRUE, nsmall = 2)
      )
    }
  }, ignoreInit = TRUE)

  importance_vec <- reactive({
    vals <- sapply(nutrients, function(nm) input[[paste0("imp_", nm)]])
    names(vals) <- nutrients
    pmin(pmax(vals, -2), 2)
  })

  # ----------------- Salts: search + filtered checkboxes + select/deselect -----
  all_salts <- reactive(rownames(nutrient_matrix))

  filtered_salts <- reactive({
    salts <- all_salts()
    q <- input$salt_search
    if (is.null(q) || !nzchar(trimws(q))) return(salts)
    q <- trimws(q)
    raw_hit <- grepl(q, salts, ignore.case = TRUE)
    plain   <- gsub("[^A-Za-z0-9()+.-]", " ", salts)
    plain_hit <- grepl(q, plain, ignore.case = TRUE)
    salts[ raw_hit | plain_hit ]
  })

  output$salt_picker <- renderUI({
    choices <- filtered_salts()
    cur_selected_visible <- intersect(selected_salts(), choices)

    if (length(choices) == 0) {
      return(tagList(
        tags$p(class = "text-muted", "No salts match the search."),
        checkboxGroupInput("selected_saltes", label = "Select fertilizers to include in optimization:",
                           choiceNames  = list(), choiceValues = character(0), selected = character(0))
      ))
    }

    checkboxGroupInput("selected_saltes", label = NULL,
                       choiceNames  = lapply(choices, function(s) htmltools::HTML(format_formula_html(s))),
                       choiceValues = choices,
                       selected     = cur_selected_visible
    )
  })

  observeEvent(input$selected_saltes, {
    vis <- filtered_salts()
    old <- selected_salts()
    new_sel <- union(setdiff(old, vis), input$selected_saltes %||% character(0))
    selected_salts(new_sel)
  }, ignoreInit = FALSE)

  observeEvent(input$deselect_all_salts, {
    vis <- filtered_salts()
    old <- selected_salts()
    new_sel <- setdiff(old, vis)
    selected_salts(new_sel)
    updateCheckboxGroupInput(session, "selected_saltes", selected = intersect(new_sel, vis))
  })

  observeEvent(input$select_all_visible, {
    vis <- filtered_salts()
    old <- selected_salts()
    new_sel <- union(old, vis)
    selected_salts(new_sel)
    updateCheckboxGroupInput(session, "selected_saltes", selected = intersect(new_sel, vis))
  })

  observeEvent(input$clear_search, { updateTextInput(session, "salt_search", value = "") })

  output$sel_status <- renderUI({
    total   <- length(all_salts())
    visible <- length(filtered_salts())
    n_sel   <- length(selected_salts())
    tags$p(sprintf("Using %d selected of %d visible (total %d).", n_sel, visible, total))
  })

  # ----------------- status check for nutrient_matrix ------------------------
  output$status <- renderUI({
    miss <- setdiff(nutrients, colnames(nutrient_matrix))
    if (length(miss)) {
      tags$p(class = "text-danger", paste("nutrient_matrix missing *columns* for:", paste(miss, collapse = ", ")))
    } else {
      tags$p(class = "text-success", "✓ Ready.")
    }
  })

  # ----------------- solver (pad 1-salt with dummy zero-salt) ---------------
  result <- eventReactive(input$run, {
    sel <- selected_salts(); req(length(sel) > 0)
    nm_sub <- nutrient_matrix[sel, , drop = FALSE]
    used_dummy <- FALSE; dummy_name <- ".DUMMY_ZERO_SALT"

    if (nrow(nm_sub) == 1) {
      zero_row <- matrix(0, nrow = 1, ncol = ncol(nm_sub), dimnames = list(dummy_name, colnames(nm_sub)))
      nm_pad <- rbind(nm_sub, zero_row); used_dummy <- TRUE
    } else nm_pad <- nm_sub

    out <- optimize_nutrients(nutrient_matrix = nm_pad, target = eval_targets(), importance = importance_vec())

    if (!is.null(out$amounts)) {
      if (is.null(names(out$amounts))) names(out$amounts) <- rownames(nm_pad)
      if (used_dummy && dummy_name %in% names(out$amounts)) out$amounts <- out$amounts[setdiff(names(out$amounts), dummy_name)]
    }

    out$salt_names <- rownames(nm_sub)
    out
  }, ignoreInit = TRUE)

  # ---- display results ----
  output$results_ui <- renderUI({
    res <- result(); if (is.null(res)) return(tags$p("No results yet."))

    blocks <- list()
    amounts <- res$amounts; if (is.null(amounts)) amounts <- numeric(0)
    if (is.null(names(amounts)) || any(!nzchar(names(amounts)))) {
      nm_fallback <- res$salt_names
      if (!is.null(nm_fallback) && length(nm_fallback) == length(amounts)) names(amounts) <- nm_fallback
      else if (length(amounts)) names(amounts) <- paste0("Salt_", seq_along(amounts))
    }

    pos <- which(amounts > 0)
    if (length(pos)) {
      formulas <- names(amounts)[pos]
      mmol_l   <- unname(amounts[pos])

      df_amt <- data.frame(Formula = formulas, `mmol l⁻¹` = round(mmol_l, 4), check.names = FALSE, stringsAsFactors = FALSE)

      if (exists("compute_molar_mass", mode = "function")) {
        mm <- sapply(formulas, function(f) { f <- if (is.na(f) || !nzchar(f)) NA_character_ else f; if (is.na(f)) NA_real_ else compute_molar_mass(f) })
        mg_l <- mmol_l * mm
        df_amt$`g mol⁻¹` <- round(mm, 2)
        df_amt$`mg l⁻¹`  <- round(mg_l, 2)
        vol <- input$vol %||% 1
        if (is.numeric(vol) && !is.na(vol) && vol > 1) {
          vol_label <- if (abs(vol - round(vol)) < 1e-9) as.integer(vol) else vol
          g_total   <- mg_l * vol / 1000
          colname   <- sprintf("g %s l⁻¹", vol_label)
          df_amt[[colname]] <- round(g_total, 3)
        }
      }

      df_amt$Formula <- vapply(df_amt$Formula, function(s) { format_formula_html(safe_chr1(s)) }, character(1))
      output$tbl_amt <- renderTable(df_amt, sanitize.text.function = function(x) x)
      blocks <- c(blocks, list(tags$h5("🧂 Fertilizer amounts"), tableOutput("tbl_amt")))
    }

    nd <- data.frame(
      Nutrient      = names(res$target),
      Target        = round(as.numeric(res$target), 6),
      Achieved      = round(as.numeric(res$achieved), 6),
      Abs_Error     = round(as.numeric(res$abs_error), 6),
      Percent_Error = round(as.numeric(res$percent_error), 2),
      check.names   = FALSE,
      stringsAsFactors = FALSE
    )
    nd$Nutrient <- vapply(nd$Nutrient, function(s) { pretty_nutrient_label_str(safe_chr1(s)) }, character(1))
    output$tbl_n <- renderTable(nd, sanitize.text.function = function(x) x)
    blocks <- c(blocks, list(tags$h5("🎯 Delivery vs target (mmol/L)"), tableOutput("tbl_n")))

    output$raw_print <- renderPrint({
      res <- result()
      if (!is.null(res)) {
        vol <- input$vol %||% 1
        if (!is.numeric(vol) || is.na(vol) || vol <= 0) vol <- 1
        print(res, vol = vol)
      }
    })

    blocks <- c(blocks,
                list(
                  tags$p(strong("🧮 Total squared absolute error: "), round(res$squared_error, 6)),
                  tags$p(strong("📊 Relative squared percentage error (optimized): "), round(res$rel_squared_error, 6)),
                  tags$hr(),
                  tags$details(tags$summary("Show raw print() output"), verbatimTextOutput("raw_print"))
                )
    )

    do.call(tagList, blocks)
  })

  # ---- ternary plots (exactly 3 via selectize + validate) ----
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
