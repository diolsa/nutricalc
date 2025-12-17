# Shiny modules for NutriCalc UI and server logic

input_panel_ui <- function(id) {
  ns <- NS(id)
  column(
    width = 6,
    div(
      class = "card p-3",
      h5("⚗️ Nutrient Targets & Fertilizers"),
      tabsetPanel(
        id = ns("input_tabs"),
        type = "tabs",
        tabPanel(
          "🎯 Nutrient Targets",
          fluidRow(
            column(3, strong("Nutrient")),
            column(
              4,
              div(
                strong(textOutput(ns("target_col_header"))),
                radioButtons(
                  ns("input_unit"),
                  label = NULL,
                  choices = c("mmol/L", "µmol/L", "mg/L"),
                  selected = "mmol/L",
                  inline = TRUE
                )
              )
            ),
            column(
              5,
              div(
                strong("Priority"),
                tags$br(),
                tags$small(class = "text-muted", "Weight for target matching")
              )
            )
          ),
          tags$hr(style = "margin:4px 0;"),
          uiOutput(ns("nutrient_rows"))
        ),
        tabPanel(
          "🧂 Fertilizers",
          fluidRow(
            column(
              10,
              div(
                class = "search-inline",
                style = "display:flex; align-items:center; gap:6px;",
                textInput(ns("salt_search"), label = NULL, placeholder = "Search salts...", width = "100%"),
                actionButton(ns("clear_search"), label = NULL, icon = icon("times-circle"), class = "btn btn-light btn-sm", title = "Clear search")
              )
            )
          ),
          tags$hr(style = "margin:6px 0;"),
          div(
            class = "btn-group",
            role = "group",
            actionButton(ns("select_all_visible"), label = NULL, icon = icon("check-square"), class = "btn btn-light btn-sm", title = "Select all visible"),
            actionButton(ns("deselect_all_salts"), label = NULL, icon = icon("square"), class = "btn btn-light btn-sm", title = "Deselect all")
          ),
          tags$div(style = "margin:13px 0;"),
          uiOutput(ns("salt_picker")),
          uiOutput(ns("sel_status"))
        ),
        tabPanel(
          "📚 Recipes",
          fluidRow(
            column(
              width = 4,
              radioButtons(
                ns("recipe_category"),
                div(strong("Category")),
                choices = c(
                  "🥬 Olericulture" = "Olericulture",
                  "🍓 Fruticulture" = "Fruticulture",
                  "🌸 Floriculture" = "Floriculture",
                  "⚙️ Standard Formulations" = "Standard Formulations"
                ),
                selected = "Olericulture"
              )
            ),
            column(
              width = 8,
              uiOutput(ns("recipe_select_ui")),
              br(),
              actionButton(ns("apply_recipe"), "Apply recipe", class = "btn btn-primary btn-sm")
            )
          ),
          tags$hr(style = "margin:6px 0;"),
          uiOutput(ns("recipe_notes")),
          tags$small(
            class = "text-muted",
            "Pick a category, then a recipe. Applying a recipe will set the unit, fill targets, and (if defined) select defined salts."
          )
        ),
        tabPanel(
          "🧬 Tissue",
          fluidRow(
            column(
              width = 4,
              h5("Tissue (%)"),
              tags$small(class = "text-muted", "Nutrient concentration in dry mass"),
              br(),
              fluidRow(
                column(6, strong("Nutrient")),
                column(6, strong("Tissue %"))
              ),
              tags$hr(style = "margin:4px 0;"),
              tags$div(
                lapply(
                  nutrients,
                  function(nm) {
                    fluidRow(
                      column(
                        width = 6,
                        tags$label(HTML(pretty_nutrient_label_str(nm)))
                      ),
                      column(
                        width = 6,
                        numericInput(
                          inputId = ns(paste0("tissue_", nm)),
                          label = NULL,
                          value = NA,
                          min = 0,
                          step = 0.01,
                          width = "100%"
                        )
                      )
                    )
                  }
                )
              )
            ),
            column(
              width = 3,
              h5("Water Use Efficiency (WUE)"),
              tags$small(class = "text-muted", "WUE = plant dry mass / water transpired."),
              numericInput(ns("tissue_dm"), "Plant dry mass (g)", value = NA, min = 0, step = 0.1, width = "100%"),
              numericInput(ns("tissue_water"), "Water transpired (L)", value = NA, min = 0, step = 0.1, width = "100%"),
              br(),
              strong(textOutput(ns("tissue_wue_text")))
            ),
            column(
              width = 5,
              h5("mg/L from tissue"),
              tags$small(class = "text-muted", "Assuming water use efficiency above"),
              tableOutput(ns("tissue_table")),
              br(),
              actionButton(ns("tissue_apply"), "Use as targets", class = "btn btn-primary btn-sm")
            )
          )
        )
      ),
      tags$hr(style = "margin:6px 0;"),
      fluidRow(
        id = ns("button_row"),
        column(
          width = 3,
          actionButton(ns("set_all_zero"), "Set all to 0", class = "btn btn-light btn-sm", style = "width:100%; margin:0;")
        ),
        column(
          width = 3,
          actionButton(ns("reset"), "Reset to defaults", class = "btn btn-light btn-sm", style = "width:100%; margin:0;")
        ),
        column(
          width = 3,
          div(
            style = "margin-top: 2px;",
            checkboxInput(ns("use_acid_base"), "Use acids/bases", value = FALSE, width = "100%")
          )
        ),
        column(
          width = 3,
          actionButton(ns("run"), "🧪 Result", class = "btn btn-primary btn-sm", style = "width:100%; margin:0;")
        )
      ),
      br(),
      uiOutput(ns("status"))
    )
  )
}

results_panel_ui <- function(id) {
  ns <- NS(id)
  column(
    width = 6,
    div(
      class = "card p-3",
      h5("🧪 Result & Plots"),
      tabsetPanel(
        id = ns("result_tabs"),
        type = "tabs",
        tabPanel("🗒Result", uiOutput(ns("delivery_ui"))),
        tabPanel(
          "🛢️ Solution",
          fluidRow(
            column(
              width = 4,
              numericInput(ns("vol"), "Working volume (L)", value = 1, min = 0, step = 1, width = "100%")
            ),
            column(
              width = 4,
              numericInput(ns("stock_factor"), "Stock factor (×)", value = 100, min = 1, step = 1, width = "100%")
            ),
            column(
              width = 4,
              numericInput(ns("stock_vol"), "Stock volume (L)", value = 1, min = 0, step = 0.1, width = "100%")
            )
          ),
          uiOutput(ns("fertilizer_ui"))
        ),
        tabPanel(
          "➖ Anions",
          tags$p(class = "text-muted", "Select exactly three anions (defaults: NO3–N, P, S)."),
          selectizeInput(
            ns("anion_keys"),
            label = NULL,
            choices = c("NO3_N", "P", "S", "Cl"),
            selected = c("NO3_N", "P", "S"),
            multiple = TRUE,
            options = list(maxItems = 3, plugins = list("remove_button"))
          ),
          div(style = "display:flex; justify-content:center; align-items:center;", plotOutput(ns("ternary_anions"), height = "666px"))
        ),
        tabPanel(
          "➕ Cations",
          tags$p(class = "text-muted", "Select exactly three cations (defaults: K, Ca, Mg)."),
          selectizeInput(
            ns("cation_keys"),
            label = NULL,
            choices = c("K", "Ca", "Mg", "NH4_N", "Na"),
            selected = c("K", "Ca", "Mg"),
            multiple = TRUE,
            options = list(maxItems = 3, plugins = list("remove_button"))
          ),
          div(style = "display:flex; justify-content:center; align-items:center;", plotOutput(ns("ternary_cations"), height = "666px"))
        )
      )
    )
  )
}

input_panel_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    run_trigger <- reactiveVal(0)
    targets_mmol <- reactiveVal(default_targets_mmol)
    current_input_unit <- reactiveVal(canonical_unit)
    selected_salts <- reactiveVal(rownames(nutrient_matrix))

    recipe_categories <- vapply(
      recipes,
      function(r) r$category %||% "Standard Formulations",
      character(1)
    )

    output$target_col_header <- renderText({
      unit <- if (is.null(input$input_unit)) "mmol/L" else input$input_unit
      sprintf("Target (%s)", unit)
    })

    output$nutrient_rows <- renderUI({
      tagList(lapply(nutrients, function(nm) {
        div(
          class = "nutrient-row",
          fluidRow(
            column(3, tags$label(HTML(pretty_nutrient_label_str(nm)))),
            column(4, textInput(inputId = paste0("expr_", nm), label = NULL, value = default_expr[[nm]])),
            column(
              5,
              div(
                class = "importance-slider",
                sliderInput(
                  inputId = paste0("imp_", nm),
                  label = NULL,
                  ticks = TRUE,
                  min = -2,
                  max = 2,
                  step = 1,
                  value = default_importance[[nm]]
                )
              )
            )
          )
        )
      }))
    })

    inputs_ready <- reactive({
      all(vapply(nutrients, function(nm) !is.null(input[[paste0("expr_", nm)]]), logical(1)))
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

    observeEvent(input$run, {
      req(inputs_ready())
      unit_in <- input$input_unit %||% canonical_unit
      vals_mmol <- parse_targets_from_inputs(input, nutrients, unit_in)
      targets_mmol(vals_mmol)
      run_trigger(isolate(run_trigger()) + 1L)
    })

    observeEvent(input$input_unit, {
      req(inputs_ready())

      new_unit <- input$input_unit %||% canonical_unit
      old_unit <- current_input_unit() %||% canonical_unit

      vals_mmol <- parse_targets_from_inputs(input, nutrients, old_unit)
      targets_mmol(vals_mmol)

      display_vals <- targets_for_display(vals_mmol, new_unit)

      for (nm in nutrients) {
        updateTextInput(
          session, paste0("expr_", nm),
          value = format(display_vals[[nm]], trim = TRUE, scientific = FALSE)
        )
      }

      current_input_unit(new_unit)
    }, ignoreInit = TRUE)

    eval_targets <- reactive({
      vals <- targets_mmol()
      validate(need(!is.null(vals), "Targets not initialized yet."))
      vals
    })

    importance_vec <- reactive({
      vals <- sapply(nutrients, function(nm) input[[paste0("imp_", nm)]])
      names(vals) <- nutrients
      pmin(pmax(vals, -2), 2)
    })

    tissue_wue <- reactive({
      dm <- input$tissue_dm %||% NA_real_
      wat <- input$tissue_water %||% NA_real_
      if (is.na(dm) || is.na(wat) || wat <= 0) return(NA_real_)
      dm / wat
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

      perc <- sapply(nutrients, function(nm) {
        val <- input[[paste0("tissue_", nm)]] %||% NA_real_
        ifelse(is.na(val), 0, as.numeric(val))
      })
      names(perc) <- nutrients

      mg_per_g <- perc * 10
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
        Nutrient = vapply(nutrients, pretty_nutrient_label_str, character(1)),
        `Tissue %` = round(perc, 3),
        `mg l⁻¹` = round(mgL, 3),
        check.names = FALSE
      )
    }, sanitize.text.function = function(x) x)

    all_salts <- reactive(rownames(nutrient_matrix))

    filtered_salts <- reactive({
      salts <- all_salts()
      qry <- trimws(input$salt_search %||% "")

      if (nzchar(qry)) {
        matches <- grepl(qry, salts, ignore.case = TRUE)
        idx <- matches

        salt_desc <- paste(nutrient_matrix[, , drop = TRUE])
        matches_desc <- grepl(qry, salt_desc, ignore.case = TRUE)
        idx <- idx | matches_desc

        matches_info <- grepl(qry, salt_info$name, ignore.case = TRUE) | grepl(qry, salt_info$formula, ignore.case = TRUE)
        idx <- idx | matches_info

        salts <- salts[idx]
      }

      salts
    })

    output$salt_picker <- renderUI({
      sel <- selected_salts()
      choices <- filtered_salts()

      missing_cols <- setdiff(nutrients, colnames(nutrient_matrix))
      if (length(missing_cols)) {
        return(tags$p(
          class = "text-danger",
          paste("nutrient_matrix missing columns for:", paste(missing_cols, collapse = ", "))
        ))
      }

      if (!length(choices)) {
        return(tags$p(class = "text-warning", "No salts match your search."))
      }

      salts_sorted <- sort(choices)

      salts_with_cb <- lapply(salts_sorted, function(salt) {
        salt_info_row <- salt_info[salt_info$salt == salt, , drop = FALSE]
        is_ab <- ifelse(nrow(salt_info_row) && isTRUE(salt_info_row$is_acid_base), "(acid/base)", "")
        tags$div(
          class = "form-check",
          tags$label(
            class = "form-check-label",
            tags$input(
              type = "checkbox",
              class = "form-check-input",
              name = paste0("sel_salt_", salt),
              checked = salt %in% sel,
              onchange = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'});", ns("salt_toggle"), salt)
            ),
            tags$span(salt, style = "margin-left:6px;"),
            tags$span(
              class = "text-muted",
              style = "margin-left:6px; font-size:12px;",
              salt_info_row$name,
              if (nzchar(is_ab)) paste("", is_ab)
            ),
            tags$span(
              class = "text-muted",
              style = "margin-left:6px; font-size:12px;",
              salt_info_row$formula
            )
          )
        )
      })

      tags$div(id = ns("selected_salts"), salts_with_cb)
    })

    observeEvent(input$select_all_visible, {
      selected_salts(filtered_salts())
      runjs(sprintf("Shiny.setInputValue('%s', null);", ns("salt_toggle")))
    })

    observeEvent(input$deselect_all_salts, {
      selected_salts(character(0))
      runjs(sprintf("Shiny.setInputValue('%s', null);", ns("salt_toggle")))
    })

    observeEvent(input$salt_toggle, {
      salt <- input$salt_toggle
      if (is.null(salt)) return()
      cur <- selected_salts()
      if (salt %in% cur) {
        selected_salts(setdiff(cur, salt))
      } else {
        selected_salts(c(cur, salt))
      }
    })

    observeEvent(input$clear_search, {
      updateTextInput(session, "salt_search", value = "")
    })

    output$sel_status <- renderUI({
      cur <- selected_salts()
      n <- length(cur)
      tags$p(class = "text-muted", sprintf("Selected salts: %s", paste(cur, collapse = ", ")))
    })

    output$recipe_select_ui <- renderUI({
      cats <- recipe_categories[match(names(recipes), names(recipe_categories))]
      rec_names <- names(recipes)[cats == input$recipe_category]
      selectInput(
        ns("recipe_pick"),
        label = div(strong("Recipe")),
        choices = rec_names,
        selected = rec_names[1]
      )
    })

    output$recipe_notes <- renderUI({
      req(input$recipe_pick)
      notes <- recipes[[input$recipe_pick]]$notes
      if (is.null(notes) || !nzchar(notes)) return(NULL)
      tags$div(tags$strong("Notes:"), tags$p(notes))
    })

    apply_salts <- function(rec) {
      if (!is.null(rec$salts)) {
        salts <- rec$salts[rec$salts %in% rownames(nutrient_matrix)]
        selected_salts(unique(salts))
      }
    }

    observeEvent(input$apply_recipe, {
      req(input$recipe_pick)
      rec <- recipes[[input$recipe_pick]]

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
          updateTabsetPanel(session, "input_tabs", selected = "🎯 Nutrient Targets")
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
        updateTabsetPanel(session, "input_tabs", selected = "🎯 Nutrient Targets")
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

    observeEvent(input$input_tabs, {
      if (identical(input$input_tabs, "🧬 Tissue")) {
        if (!identical(normalize_unit(input$input_unit), "mg/L")) {
          updateRadioButtons(session, "input_unit", selected = "mg/L")
        }
      }
    })

    observeEvent(input$tissue_apply, {
      mgL <- tissue_mgL()

      apply_mgL <- function() {
        for (nm in nutrients) {
          updateTextInput(
            session,
            paste0("expr_", nm),
            value = "0"
          )
        }

        for (nm in nutrients) {
          updateTextInput(
            session,
            paste0("expr_", nm),
            value = format(mgL[[nm]], trim = TRUE, scientific = FALSE)
          )
        }

        vals_mmol <- to_canonical_from_unit(mgL, "mg/L")
        targets_mmol(vals_mmol)

        updateTabsetPanel(session, "input_tabs", selected = "🎯 Nutrient Targets")
      }

      if (!identical(normalize_unit(input$input_unit), "mg/L")) {
        updateRadioButtons(session, "input_unit", selected = "mg/L")
        session$onFlushed(function() {
          apply_mgL()
        }, once = TRUE)
      } else {
        apply_mgL()
      }
    })

    list(
      run_trigger = run_trigger,
      eval_targets = eval_targets,
      importance_vec = importance_vec,
      selected_salts = selected_salts,
      use_acid_base = reactive(isTRUE(input$use_acid_base))
    )
  })
}

results_panel_server <- function(id, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- eventReactive(inputs$run_trigger(), {
      sel <- inputs$selected_salts(); req(length(sel) > 0)

      use_acid <- isTRUE(inputs$use_acid_base())

      is_acid_base_sel <- salt_info$is_acid_base[match(sel, salt_info$salt)]
      is_acid_base_sel[is.na(is_acid_base_sel)] <- FALSE

      sel_effective <- if (use_acid) sel else sel[!is_acid_base_sel]

      req(length(sel_effective) > 0)

      nm_sub <- nutrient_matrix[sel_effective, , drop = FALSE]

      used_dummy <- FALSE
      dummy_name <- ".DUMMY_ZERO_SALT"

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

      if (use_acid) {
        out <- two_stage_optimize_nutrients(
          nutrient_matrix = nm_pad,
          target = inputs$eval_targets(),
          importance = inputs$importance_vec()
        )
      } else {
        out <- optimize_nutrients(
          nutrient_matrix = nm_pad,
          target = inputs$eval_targets(),
          importance = inputs$importance_vec()
        )
      }

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

    output$delivery_ui <- renderUI({
      res <- result()
      if (is.null(res)) return(tags$p("No result yet."))

      target <- strip_negative_zero(round(drop_tiny(as.numeric(res$target)), 6))
      achieved <- strip_negative_zero(round(drop_tiny(as.numeric(res$achieved)), 6))
      abs_err <- strip_negative_zero(round(drop_tiny(as.numeric(res$abs_error)), 6))
      pct_err <- strip_negative_zero(round(as.numeric(res$percent_error), 2))

      nd <- data.frame(
        Nutrient = names(res$target),
        Target = target,
        Achieved = achieved,
        "Absolute Error" = abs_err,
        "Percent Error" = pct_err,
        stringsAsFactors = FALSE,
        check.names = FALSE
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
        tableOutput(ns("tbl_n")),
        tags$p(strong("🧮 Total squared absolute error: "), round(res$squared_error, 6)),
        tags$p(strong("📊 Relative squared percentage error (optimized): "), round(res$rel_squared_error, 6)),
        tags$hr(),
        tags$details(
          tags$summary("Show raw print() output"),
          verbatimTextOutput(ns("raw_print"))
        )
      )
    })

    output$fertilizer_ui <- renderUI({
      res <- result()
      if (is.null(res)) return(tags$p("No result yet."))

      nm_sub <- nutrient_matrix[res$salt_names, , drop = FALSE]
      amounts <- res$amounts
      vol <- input$vol %||% 1
      vol <- ifelse(is.na(vol) || !is.numeric(vol) || vol <= 0, 1, vol)

      stock_factor <- input$stock_factor %||% 1
      stock_factor <- ifelse(is.na(stock_factor) || stock_factor < 1, 1, stock_factor)

      stock_vol <- input$stock_vol %||% 1
      stock_vol <- ifelse(is.na(stock_vol) || stock_vol < 0, 1, stock_vol)

      if (!length(amounts)) {
        return(tags$p("No amounts computed."))
      }

      mmol_l <- as.numeric(res$achieved)
      names(mmol_l) <- names(res$achieved)

      out <- calc_amounts(amounts = amounts, nm = nm_sub, vol = vol)
      if (is.null(out) || !nrow(out)) {
        return(tags$p("calc_amounts() returned no data."))
      }

      out$Formula <- vapply(out$Formula, safe_chr1, character(1))

      df_out <- out[, c("Salt", "Formula", "g"), drop = FALSE]
      bold_cols <- "g"

      macro_rows <- out$`Macro or Micro` == "Macro"
      micro_rows <- out$`Macro or Micro` == "Micro"

      g_total <- out$g
      g_total[is.na(g_total)] <- 0

      mg_total <- g_total * 1000

      g_total_stock <- g_total * (stock_vol / stock_factor)
      mg_total_stock <- g_total_stock * 1000

      total_col <- sprintf("for %s l", vol)
      total_vals <- character(length(mmol_l))
      total_vals[macro_rows] <- sprintf("%.2f g", g_total[macro_rows])
      total_vals[micro_rows] <- sprintf("%.2f mg", mg_total[micro_rows])
      total_vals[is.na(total_vals)] <- ""
      df_out[[total_col]] <- total_vals
      bold_cols <- c(bold_cols, total_col)

      if (!is.na(stock_vol) && !is.na(stock_factor)) {
        stock_vol_label <- format(stock_vol, trim = TRUE, scientific = FALSE)
        stock_col <- sprintf(" %s l of %s-fold", stock_vol_label, stock_factor)

        total_stock <- character(length(mmol_l))
        total_stock[macro_rows] <- sprintf("%.2f g ", g_total_stock[macro_rows])
        total_stock[micro_rows] <- sprintf("%.2f mg", mg_total_stock[micro_rows])
        total_stock[is.na(total_stock)] <- ""

        df_out[[stock_col]] <- total_stock
      }

      df_out$Formula <- vapply(
        df_out$Formula,
        function(s) format_formula_html(safe_chr1(s)),
        character(1)
      )

    col_names <- colnames(df_out)
    n_cols <- length(col_names)

    amt_df <- out

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
              style = "font-weight:bold; border-top:2px solid #6c757d; background-color:#f8f9fa;",
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

        add_group("A", "A-BAK")
        add_group("B", "B-BAK")
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
        uiOutput(ns("tbl_amt_grouped"))
      )
    })

    output$ternary_anions <- renderPlot({
      req(result()); req(exists("plot_ternary", mode = "function"))
      validate(need(length(input$anion_keys) == 3, "Pick exactly 3 anions"))
      keys <- input$anion_keys
      plt <- plot_ternary(result = result(), keys = keys, title = sprintf("Anions (%s : %s : %s)", keys[1], keys[2], keys[3]))
      if (!is.null(plt)) print(plt)
    })

    output$ternary_cations <- renderPlot({
      req(result()); req(exists("plot_ternary", mode = "function"))
      validate(need(length(input$cation_keys) == 3, "Pick exactly 3 cations"))
      keys <- input$cation_keys
      plt <- plot_ternary(result = result(), keys = keys, title = sprintf("Cations (%s : %s : %s)", keys[1], keys[2], keys[3]))
      if (!is.null(plt)) print(plt)
    })
  })
}
