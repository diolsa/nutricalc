# app.R — interface to optimize_nutrients()
# Each nutrient row has: Nutrient | Target expression | Importance slider (−2..2)

# ---- packages ----
pkgs <- c("shiny", "bslib")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
if (!requireNamespace("nnls", quietly = TRUE)) install.packages("nnls")

library(shiny)
library(bslib)

# ---- load your code & data ----
source_all_r <- function(dir = "R") {
  if (dir.exists(dir)) {
    r_files <- list.files(dir, pattern = "\\.[rR]$", full.names = TRUE)
    for (f in r_files) try(source(f, chdir = TRUE), silent = TRUE)
  }
}
load_all_data <- function(dir = "data") {
  if (dir.exists(dir)) {
    data_files <- list.files(dir, pattern = "\\.(rda|RData)$", full.names = TRUE)
    for (f in data_files) try(load(f, envir = .GlobalEnv), silent = TRUE)
  }
}
source_all_r("R")
load_all_data("data")

stopifnot(exists("optimize_nutrients", mode = "function"))
stopifnot(exists("nutrient_matrix"))

# ---- defaults ----
nutrients <- c("NO3_N","NH4_N","P","K","Ca","Mg","S","Na","Cl",
               "Fe","Mn","Zn","B","Cu","Mo","Si")

default_expr <- c(
  "14.96","1.25","converte(\"P2O5\",\"P\",5)","8.88","3.16","1.07","1.3","0","0",
  "0.015","0.01","0.005","0.015","0.00075","0.0005","0"
)
names(default_expr) <- nutrients

default_importance <- c(
  NO3_N = 1, NH4_N = 0, P = 0, K = 1, Ca = 0, Mg = 0, S = 0, Na = 0, Cl = 0,
  Fe = 1, Mn = 1, Zn = 1, B = 1, Cu = 1, Mo = 1, Si = 1
)

# ---- UI ----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("NutriCalc — Nutrient Optimization"),
  tags$head(tags$style(HTML("
    .form-group { margin-bottom: 6px; }
    .shiny-input-container { margin-bottom: 6px; }
    .irs { margin-top: -6px; }
    .container-fluid { max-width: 1100px; }
  "))),
  tags$p("Enter target expressions (e.g. ", code('converte("P2O5","P",5)'),
         ") and adjust importance (−2 to +2). Units: mmol/L (converted to mol/L for the solver)."),
  fluidRow(
    column(12,
           div(class = "card p-3",
               fluidRow(
                 column(3, strong("Nutrient")),
                 column(4, strong("Target (R expression, mmol/L)")),
                 column(5, strong("Importance (−2..2)"))
               ),
               tags$hr(style="margin:4px 0;"),
               uiOutput("nutrient_rows")
           )
    )
  ),
  br(),
  fluidRow(
    column(
      12,
      actionButton("reset", "Reset to defaults"),
      span("  "),
      actionButton("run", "Run optimize_nutrients()", class = "btn btn-primary"),
      br(), br(),
      uiOutput("status")
    )
  ),
  hr(),
  h4("Results"),
  uiOutput("results_ui")
)

# ---- Server ----
server <- function(input, output, session) {

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
          column(5, sliderInput(
            inputId = paste0("imp_", nm),
            label = NULL,
            min = -2, max = 2, step = 1,
            value = default_importance[[nm]]
          ))
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

  # evaluate target expressions
  eval_targets <- reactive({
    vals <- sapply(nutrients, function(nm) {
      txt <- input[[paste0("expr_", nm)]]
      val <- try(eval(parse(text = txt), envir = .GlobalEnv), silent = TRUE)
      if (inherits(val, "try-error") || !is.numeric(val) || length(val) != 1)
        stop(sprintf("Error evaluating target for %s: '%s'", nm, txt))
      val
    })
    names(vals) <- nutrients
    vals / 1000  # mmol/L -> mol/L
  })

  importance_vec <- reactive({
    vals <- sapply(nutrients, function(nm) input[[paste0("imp_", nm)]])
    names(vals) <- nutrients
    pmin(pmax(vals, -2), 2)
  })

  # status check
  output$status <- renderUI({
    miss <- setdiff(nutrients, rownames(nutrient_matrix))
    if (length(miss))
      tags$p(class="text-danger",
             paste("nutrient_matrix missing rows for:", paste(miss, collapse=", ")))
    else
      tags$p(class="text-success","✓ Ready.")
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
      blocks <- c(blocks, list(tags$h5("🧪 Fertilizer amounts"), tableOutput("tbl_amt")))
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
      tags$h5("🎯 Delivery vs target (mol/L)"),
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
