# app.R — interface to optimize_nutrients()
# Each nutrient row has: Nutrient | Target expression | Importance slider (−2..2)


library(shiny)
library(bslib)



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
  Fe = 1, Mn = 1, Zn = 1, B = 1, Cu = 1, Mo = 1, Si = 1
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

  /* Move sliders slightly up for alignment */
  .form-group.shiny-input-slider { margin-top: -8px; }

  /* Hide value/min/max labels only for importance sliders */
  .importance-slider .irs-min,
  .importance-slider .irs-max,
  .importance-slider .irs-single {
    display: none !important;
  }

  /* Lift importance slider track a bit more */
  .importance-slider .irs { margin-top: -16px; }
 .importance-slider { margin-bottom: 10px; }

")))
  ,

  titlePanel("NutriCalc 🌿 Nutrient Optimization"),

  # Main two-column layout
  fluidRow(
    # LEFT: inputs
    column(
      width = 6,
      div(class = "card p-3",
          tags$h5("⚗️ Nutrient Targets & Importance"),
          fluidRow(
            column(3, strong("Nutrient")),
            column(
              4,
              div(
                strong(textOutput("target_col_header")),
                radioButtons(
                  "input_unit",
                  label = NULL,
                  choices = c("mmol/L", "mg/L"),
                  selected = "mmol/L",
                  inline = TRUE
                )
              )
              ),
            column(5, strong("Importance (−2..2)"))
          ),
          tags$hr(style="margin:4px 0;"),




          uiOutput("nutrient_rows"),
          br(),
          actionButton("reset", "Reset to defaults"),
          span("  "),
          actionButton("run", "🧪 Results", class = "btn btn-primary"),
          br(), br(),
          uiOutput("status")
      )
    ),

    # RIGHT: results
    column(
      width = 6,
      div(class = "card p-3",
          h5("🧪 Results"),
          uiOutput("results_ui")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # Header label reacts to unit selection
  output$target_col_header <- renderText({
    unit <- if (is.null(input$input_unit)) "mmol/L" else input$input_unit
    sprintf("Target (%s)", unit)
  })

  # build rows dynamically
  output$nutrient_rows <- renderUI({
    tagList(
      lapply(nutrients, function(nm) {
        fluidRow(
          column(3, tags$label(nm)),
          column(4, textInput(
            inputId = paste0("expr_", nm),
            label = NULL,
            value = default_expr[[nm]]
          )),
          column(
            5,
            div(
              class = "importance-slider",
              sliderInput(
                inputId = paste0("imp_", nm),
                label = NULL,
                min = -2, max = 2, step = 1,
                value = default_importance[[nm]]
              )
            )
          )

        )
      })
    )
  })

  # reset button
  observeEvent(input$reset, {
    for (nm in nutrients) {
      updateTextInput(session, paste0("expr_", nm), value = default_expr[[nm]])
      updateSliderInput(session, paste0("imp_", nm), value = default_importance[[nm]])
    }
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

    # Evaluate each cell (so expressions like converte("P2O5","P",5) work)
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
      updateTextInput(
        session, paste0("expr_", nm),
        value = format(round(converted_vals[[nm]], 2), trim = TRUE, nsmall = 2)
      )
    }
  }, ignoreInit = TRUE)

  importance_vec <- reactive({
    vals <- sapply(nutrients, function(nm) input[[paste0("imp_", nm)]])
    names(vals) <- nutrients
    pmin(pmax(vals, -2), 2)
  })

  # status check
  output$status <- renderUI({
    miss <- setdiff(nutrients, colnames(nutrient_matrix))  # nutrients must be columns
    if (length(miss)) {
      tags$p(class="text-danger",
             paste("nutrient_matrix missing *columns* for:", paste(miss, collapse=", ")))
    } else {
      tags$p(class="text-success","✓ Ready.")
    }
  })

  # run solver
  result <- eventReactive(input$run, {
    optimize_nutrients(
      nutrient_matrix = nutrient_matrix,
      target          = eval_targets(),
      importance      = importance_vec()
    )
  }, ignoreInit = TRUE)

  # display results
  output$results_ui <- renderUI({
    res <- result()
    if (is.null(res)) return(tags$p("No results yet."))

    blocks <- list()

    pos <- which(res$amounts > 0)
    if (length(pos)) {
      formulas <- names(res$amounts)[pos]
      df_amt <- data.frame(
        Formula    = formulas,
        `mmol l⁻¹` = round(res$amounts[pos], 4),
        check.names = FALSE
      )
      if (exists("compute_molar_mass", mode = "function")) {
        mm <- sapply(formulas, compute_molar_mass)
        df_amt$`g mol⁻¹` <- round(mm, 2)
        df_amt$`mg l⁻¹`  <- round(res$amounts[pos] * mm, 2)
      }
      output$tbl_amt <- renderTable(df_amt)
      blocks <- c(blocks, list(tags$h5("🧂 Fertilizer amounts"), tableOutput("tbl_amt")))
    }

    nd <- data.frame(
      Nutrient      = names(res$target),
      Target        = round(as.numeric(res$target), 6),
      Achieved      = round(as.numeric(res$achieved), 6),
      Abs_Error     = round(as.numeric(res$abs_error), 6),
      Percent_Error = round(as.numeric(res$percent_error), 2),
      check.names = FALSE
    )
    output$tbl_n <- renderTable(nd)
    blocks <- c(blocks, list(
      tags$h5("🎯 Delivery vs target (mmol/L)"),
      tableOutput("tbl_n"),
      tags$p(strong("🧮 Total squared error: "), round(res$squared_error, 6))
    ))

    output$raw_print <- renderPrint({ print(res) })
    blocks <- c(blocks,
                list(tags$hr(),
                     tags$details(tags$summary("Show raw print() output"),
                                  verbatimTextOutput("raw_print"))))
    do.call(tagList, blocks)
  })
}

shinyApp(ui, server)
