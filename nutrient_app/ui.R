#' Launch the Nutrient Optimizer Shiny app
#'
#' @export
run_nutrient_app <- function() {
  app <- nutrient_optimizer_app()
  shiny::runApp(app, launch.browser = TRUE)
}

# Internal: builds the Shiny app object
nutrient_optimizer_app <- function() {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # Fixed set of nutrients for field inputs
  nutrient_defaults_mmol <- c(
    NO3_N = 14.96,
    NH4_N = 1.25,
    P     = 5,
    K     = 8.88,
    Ca    = 3.16,
    Mg    = 1.07,
    S     = 1.3,
    Na    = 0.0,
    Cl    = 0.0,
    Fe    = 0.015,
    Mn    = 0.01,
    Zn    = 0.005,
    B     = 0.015,
    Cu    = 0.00075,
    Mo    = 0.0005,
    Si    = 0.0
  )
  nutrient_names <- names(nutrient_defaults_mmol)

  ui <- shiny::fluidPage(
    title = "Nutrient Optimizer",
    shiny::tags$head(shiny::tags$style("
      .mt-2 { margin-top: 0.5rem; }
      .mt-3 { margin-top: 1rem; }
      .mt-4 { margin-top: 1.5rem; }
      .grid { display:grid; grid-template-columns: repeat(3, 1fr); gap:.5rem .75rem; }
      @media (max-width: 1100px){ .grid { grid-template-columns: repeat(2, 1fr);} }
      @media (max-width: 700px){ .grid { grid-template-columns: 1fr; } }
      .tight-help { margin-top:-.5rem; color:#666; }
    ")),
    shiny::titlePanel("🧪 Nutrient Optimizer (NNLS)"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("1) Nutrient matrix"),
        shiny::helpText("Upload CSV (nutrients in rows, compounds in columns). First column = nutrient names."),
        shiny::fileInput("matrix_file", "CSV file", accept = c(".csv")),
        shiny::div(
          shiny::actionLink("load_example", "Load example matrix"),
          style = "margin-top:-.4rem"
        ),
        shiny::h4(class = "mt-3", "2) Targets (mmol/L)"),
        shiny::div(class = "tight-help",
                   shiny::helpText("Set a target for each nutrient (mmol/L). Internally converted to mol/L.")
        ),
        shiny::uiOutput("targets_fields"),

        shiny::h4(class = "mt-3", "3) Importance (−2 .. +2)"),
        shiny::div(class = "tight-help",
                   shiny::helpText("Relative weight in the objective; leave at 0 unless you want to emphasize/de-emphasize.")
        ),
        shiny::uiOutput("importance_fields"),

        shiny::div(class = "mt-3",
                   shiny::actionButton("run_btn", "Optimize", class = "btn btn-primary"),
                   shiny::checkboxInput("show_nonzero_only", "Show only non-zero compounds", TRUE)
        ),
        shiny::hr(),
        shiny::helpText("Tip: Matrix entries are mol nutrient per mol compound.")
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel("Matrix (paste/preview)",
                          shiny::h5("Paste matrix CSV (optional)"),
                          shiny::textAreaInput(
                            "matrix_text", NULL, rows = 12, width = "100%",
                            placeholder = paste(
                              "Paste CSV. First row: header (compounds). First column: nutrient names.",
                              "Example:",
                              "Nutrient,KNO3,CaNO3,MgSO4,KH2PO4",
                              "N,1,2,0,1",
                              "P,0,0,0,1",
                              "K,1,0,0,1",
                              "Ca,0,1,0,0",
                              "Mg,0,0,1,0",
                              "S,0,0,1,0", sep = "\n")
                          )
          ),
          shiny::tabPanel("Results",
                          shiny::h4("Fertilizer amounts (solution)"),
                          DT::DTOutput("amounts_tbl"),
                          shiny::h4(class = "mt-4", "Nutrient delivery vs target"),
                          DT::DTOutput("nutrients_tbl"),
                          shiny::h5(class = "mt-4", "Total squared error"),
                          shiny::verbatimTextOutput("tse")
          ),
          shiny::tabPanel("Diagnostics",
                          shiny::verbatimTextOutput("diagnostics")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    # ---------- Helpers ----------
    has_pkg <- function(p) isTRUE(requireNamespace(p, quietly = TRUE))

    read_matrix_file <- function(f) readr::read_csv(f$datapath, show_col_types = FALSE)

    df_to_matrix <- function(df) {
      if (ncol(df) < 2) stop("Matrix needs a name column + at least one compound column.")
      rn <- df[[1]]
      mat <- as.matrix(df[,-1, drop = FALSE])
      storage.mode(mat) <- "numeric"
      rownames(mat) <- rn
      mat
    }

    build_example_df <- function() {
      data.frame(
        Nutrient = c("NO3_N","NH4_N","P","K","Ca","Mg","S","Na","Cl","Fe","Mn","Zn","B","Cu","Mo","Si"),
        KNO3     = c(1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0),     # N as NO3_N, K
        NH4NO3   = c(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     # NO3_N + NH4_N (toy)
        CaNO3    = c(2,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),     # 2 N (as NO3_N), Ca
        MgSO4    = c(0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0),     # Mg, S
        KH2PO4   = c(0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0),     # P, K
        NaCl     = c(0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0),     # Na, Cl
        FeEDTA   = c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0),     # Fe (toy)
        MnSO4    = c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),     # Mn
        ZnSO4    = c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0),     # Zn
        H3BO3    = c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),     # B
        CuSO4    = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0),     # Cu
        Na2MoO4  = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),     # Mo
        K2SiO3   = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),     # Si
        check.names = FALSE
      )
    }

    # ---------- Matrix input ----------
    nutrient_matrix <- shiny::reactive({
      # Priority: uploaded file > pasted text
      if (!is.null(input$matrix_file)) {
        df <- read_matrix_file(input$matrix_file)
      } else {
        txt <- input$matrix_text
        if (!nzchar(txt)) return(NULL)
        con <- textConnection(txt)
        on.exit(close(con), add = TRUE)
        df <- tryCatch(readr::read_csv(con, show_col_types = FALSE),
                       error = function(e) stop("Could not read pasted matrix as CSV: ", e$message))
      }
      mat <- df_to_matrix(df)
      if (anyDuplicated(rownames(mat))) stop("Duplicate nutrient names in the first column.")
      mat
    })

    shiny::observeEvent(input$load_example, {
      df <- build_example_df()
      updateTextAreaInput(session, "matrix_text",
                          value = paste(c(
                            paste(colnames(df), collapse = ","),
                            apply(df, 1, function(r) paste(r, collapse = ","))
                          ), collapse = "\n")
      )
    })

    # ---------- Targets & importance fields ----------
    output$targets_fields <- shiny::renderUI({
      # Build numericInput for each nutrient (mmol/L)
      divs <- lapply(nutrient_names, function(nm) {
        shiny::numericInput(
          inputId = paste0("tgt_", nm),
          label = nm,
          value = nutrient_defaults_mmol[[nm]],
          min = 0, step = 0.001
        )
      })
      shiny::div(class = "grid", divs)
    })

    output$importance_fields <- shiny::renderUI({
      divs <- lapply(nutrient_names, function(nm) {
        shiny::numericInput(
          inputId = paste0("imp_", nm),
          label = nm,
          value = 0, min = -2, max = 2, step = 1
        )
      })
      shiny::div(class = "grid", divs)
    })

    target_vec <- shiny::reactive({
      # Read mmol/L fields and convert to mol/L
      vals_mmol <- vapply(nutrient_names, function(nm) input[[paste0("tgt_", nm)]], numeric(1))
      names(vals_mmol) <- nutrient_names
      vals_mmol / 1000
    })

    importance_vec <- shiny::reactive({
      vals <- vapply(nutrient_names, function(nm) input[[paste0("imp_", nm)]], numeric(1))
      names(vals) <- nutrient_names
      vals
    })

    # Align matrix rows to our nutrient list (only those present in matrix)
    A_sub <- shiny::reactive({
      A <- nutrient_matrix()
      req(A)
      tnames <- intersect(nutrient_names, rownames(A))
      if (!length(tnames)) stop("None of the predefined nutrients were found as rows in the matrix.")
      # Warn about missing nutrients (just print in diagnostics)
      A[tnames, , drop = FALSE]
    })

    # ---------- Optimization ----------
    result <- shiny::eventReactive(input$run_btn, {
      if (!has_pkg("nnls")) stop("Package 'nnls' is required.")

      A <- A_sub()
      # Use only targets for nutrients present in matrix
      t_mol <- target_vec()
      i_vec <- importance_vec()

      present <- rownames(A)
      t_mol <- t_mol[present]
      i_vec <- i_vec[present]

      optimize_nutrients(nutrient_matrix = A, target = t_mol, importance = i_vec)
    }, ignoreInit = TRUE)

    # ---------- Outputs ----------
    output$amounts_tbl <- DT::renderDT({
      res <- result()
      if (is.null(res)) return(NULL)

      amounts_molL <- res$amounts
      if (isTRUE(input$show_nonzero_only)) {
        amounts_molL <- amounts_molL[amounts_molL > 0]
      }
      formulas <- names(amounts_molL)

      mm <- tryCatch({
        if (exists("compute_molar_mass", mode = "function")) {
          sapply(formulas, compute_molar_mass)
        } else {
          rep(NA_real_, length(formulas))
        }
      }, error = function(e) rep(NA_real_, length(formulas)))

      mmol_per_L <- amounts_molL * 1000
      mg_l <- mmol_per_L * mm  # mg/L = mmol/L * g/mol

      df <- data.frame(
        Formula      = formulas,
        `mmol l⁻¹`   = round(mmol_per_L, 4),
        `g mol⁻¹`    = round(mm, 2),
        `mg l⁻¹`     = round(mg_l, 2),
        check.names  = FALSE
      )
      DT::datatable(df, options = list(pageLength = 10))
    })

    output$nutrients_tbl <- DT::renderDT({
      res <- result()
      if (is.null(res)) return(NULL)
      df <- data.frame(
        Nutrient         = names(res$target),
        Target_mmolL     = round(as.numeric(res$target) * 1000, 6),
        Achieved_mmolL   = round(as.numeric(res$achieved) * 1000, 6),
        Abs_Error_mmolL  = round(as.numeric(res$abs_error) * 1000, 6),
        Percent_Error    = round(as.numeric(res$percent_error), 2),
        check.names = FALSE
      )
      DT::datatable(df, options = list(pageLength = 10))
    })

    output$tse <- shiny::renderText({
      res <- result()
      if (is.null(res)) return("—")
      paste0(round(res$squared_error, 8))
    })

    output$diagnostics <- shiny::renderPrint({
      A <- nutrient_matrix()
      present <- if (!is.null(A)) intersect(nutrient_names, rownames(A)) else character(0)
      missing <- setdiff(nutrient_names, present)

      cat("Packages available: nnls =", has_pkg("nnls"), "\n")
      cat("\nNutrients present in matrix rows:\n")
      print(present)
      cat("\nNutrients missing from matrix rows:\n")
      print(missing)

      cat("\nTarget vector (mol/L, only present nutrients used):\n")
      tv <- target_vec()
      print(tv[present])

      cat("\nImportance vector (only present nutrients used):\n")
      iv <- importance_vec()
      print(iv[present])

      if (!is.null(A)) {
        cat("\nSub-matrix dims (nutrients x compounds):\n")
        print(dim(A[ present, , drop = FALSE ]))
        cat("\nCompounds:\n")
        print(colnames(A))
      }
    })
  }

  # Basic dependency checks
  if (!isTRUE(requireNamespace("shiny", quietly = TRUE))) stop("Package 'shiny' is required.")
  if (!isTRUE(requireNamespace("DT", quietly = TRUE)))    stop("Package 'DT' is required.")
  if (!isTRUE(requireNamespace("readr", quietly = TRUE))) stop("Package 'readr' is required.")

  shiny::shinyApp(ui, server)
}
