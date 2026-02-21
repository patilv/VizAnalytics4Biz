library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)

# Gonzaga branding constants
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ---------------------------------------------------------------------------
# Helper: wrap output in a spinner if shinycssloaders is available
# ---------------------------------------------------------------------------
with_spinner <- function(ui_element) {
  if (requireNamespace("shinycssloaders", quietly = TRUE)) {
    shinycssloaders::withSpinner(ui_element, color = NAVY)
  } else {
    ui_element
  }
}

# ===========================================================================
# UI
# ===========================================================================
ui <- page_fillable(
  title = "Interactive Viz Toolkit",
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),

  # ---- Header banner ----
  div(
    style = paste0(
      "background:", NAVY, "; color:white; padding:14px 24px;",
      "border-radius:8px; margin-bottom:12px;"
    ),
    h3("Interactive Visualization Toolkit", style = "margin:0 0 2px 0;"),
    p("Week 06 — plotly, DT, and correlation heatmaps",
      style = "margin:0; font-size:0.85rem; opacity:0.85;")
  ),

  # ---- Instruction accordion ----
  accordion(
    id = "instructions_acc",
    open = FALSE,
    accordion_panel(
      title = "How to Use This Explorer",
      icon  = bsicons::bs_icon("info-circle"),
      p("This app has three tabs, each demonstrating a different interactive
        visualization technique you can use in R and Shiny:"),
      tags$ol(
        tags$li(
          tags$strong("plotly"), " — Toggle between a static ggplot2 chart and
          an interactive plotly version. Customize colors, tooltips, point size,
          and transparency."
        ),
        tags$li(
          tags$strong("DT"), " — Explore the mpg dataset in a fully interactive
          table. Toggle search, filters, and pagination options to see how
          DT::datatable() configuration changes."
        ),
        tags$li(
          tags$strong("Correlation Heatmap"), " — Select variables from the
          mtcars dataset and visualize their pairwise correlations as an
          interactive heatmap. Switch between color scales."
        )
      ),
      p("Each tab includes a guidance panel explaining", tags$em("when"),
        "you would reach for that tool in practice.")
    )
  ),

  # ---- Tabbed main content ----
  navset_card_tab(
    id       = "main_tabs",
    full_screen = TRUE,

    # ===================================================================
    # TAB 1 — plotly
    # ===================================================================
    nav_panel(
      title = "plotly",
      icon  = bsicons::bs_icon("graph-up"),
      layout_sidebar(
        sidebar = sidebar(
          width = 280, bg = "#f1f5f9",

          radioButtons(
            "plotly_mode", "Visualization mode:",
            choices  = c("Static ggplot2" = "static",
                         "Interactive plotly" = "plotly"),
            selected = "static"
          ),
          tags$hr(),

          selectInput(
            "plotly_color", "Color by:",
            choices = c("Vehicle class" = "class",
                        "Drive type"    = "drv",
                        "Year"          = "year"),
            selected = "class"
          ),
          tags$hr(),

          p(strong("Tooltip shows:"),
            style = "margin-bottom:4px; font-size:0.85rem;"),
          checkboxInput("tt_model", "Vehicle model",   TRUE),
          checkboxInput("tt_class", "Vehicle class",   TRUE),
          checkboxInput("tt_x",     "Engine size (x)", FALSE),
          checkboxInput("tt_y",     "Highway MPG (y)", FALSE),
          tags$hr(),

          sliderInput("pt_size",  "Point size:",
                      min = 1, max = 6, value = 2.5, step = 0.5),
          sliderInput("pt_alpha", "Transparency:",
                      min = 0.1, max = 1, value = 0.7, step = 0.05)
        ),

        # Main panel
        layout_columns(
          col_widths = 12,
          uiOutput("plotly_plot_area"),
          card(
            card_header(
              "When to use plotly",
              class = "text-white",
              style = paste0("background:", GOLD, ";")
            ),
            card_body(
              style = "font-size:0.85rem;",
              tags$ul(
                tags$li(
                  tags$strong("Exploratory analysis:"),
                  " plotly is ideal when you or your audience need to hover,
                  zoom, and pan to investigate individual data points."
                ),
                tags$li(
                  tags$strong("Dashboards & Shiny apps:"),
                  " One call to ", tags$code("ggplotly()"),
                  " turns any ggplot into a fully interactive widget."
                ),
                tags$li(
                  tags$strong("When NOT to use it:"),
                  " For polished, publication-quality figures (journals, printed
                  reports), stick with static ggplot2 — you get finer control
                  over every element."
                )
              )
            )
          )
        )
      )
    ),

    # ===================================================================
    # TAB 2 — DT
    # ===================================================================
    nav_panel(
      title = "DT",
      icon  = bsicons::bs_icon("table"),
      layout_sidebar(
        sidebar = sidebar(
          width = 280, bg = "#f1f5f9",

          p(strong("Table features"),
            style = "font-size:0.9rem; margin-bottom:6px;"),
          checkboxInput("dt_search",  "Show search box",        TRUE),
          checkboxInput("dt_filters", "Show column filters",    FALSE),
          checkboxInput("dt_length",  "Show page-length selector", TRUE),
          tags$hr(),

          sliderInput("dt_pageLen", "Rows per page:",
                      min = 5, max = 50, value = 10, step = 5),
          tags$hr(),

          p(strong("Generated code:"),
            style = "font-size:0.85rem; margin-bottom:4px;"),
          verbatimTextOutput("dt_code_out")
        ),

        layout_columns(
          col_widths = 12,
          with_spinner(DTOutput("dt_table")),
          card(
            card_header(
              "When to use DT",
              class = "text-white",
              style = paste0("background:", GOLD, ";")
            ),
            card_body(
              style = "font-size:0.85rem;",
              tags$ul(
                tags$li(
                  tags$strong("Data exploration:"),
                  " DT lets users search, sort, and filter tabular data
                  instantly — perfect for letting stakeholders slice data on
                  their own."
                ),
                tags$li(
                  tags$strong("Shiny dashboards:"),
                  " Combine DT tables with reactive plots so clicking a row
                  updates a chart."
                ),
                tags$li(
                  tags$strong("When NOT to use it:"),
                  " If your dataset has thousands of rows, consider server-side
                  processing (", tags$code("server = TRUE"),
                  ") or aggregating first. For very wide tables, a summary
                  visualization is usually clearer than a raw table."
                )
              )
            )
          )
        )
      )
    ),

    # ===================================================================
    # TAB 3 — Correlation Heatmap
    # ===================================================================
    nav_panel(
      title = "Correlation Heatmap",
      icon  = bsicons::bs_icon("grid-3x3"),
      layout_sidebar(
        sidebar = sidebar(
          width = 280, bg = "#f1f5f9",

          p(strong("Select variables:"),
            style = "font-size:0.9rem; margin-bottom:4px;"),
          checkboxGroupInput(
            "hm_vars", NULL,
            choices  = names(mtcars),
            selected = c("mpg", "cyl", "disp", "hp", "wt", "qsec")
          ),
          tags$hr(),

          radioButtons(
            "hm_palette", "Color scale:",
            choices  = c("Viridis"        = "viridis",
                         "RdBu diverging" = "rdbu"),
            selected = "rdbu"
          ),

          tags$hr(),
          actionButton("hm_select_all", "Select all",
                       class = "btn-sm btn-outline-primary me-1"),
          actionButton("hm_clear_all", "Clear all",
                       class = "btn-sm btn-outline-secondary")
        ),

        layout_columns(
          col_widths = 12,
          with_spinner(plotlyOutput("hm_plot", height = "520px")),
          card(
            card_header(
              "When to use heatmaps",
              class = "text-white",
              style = paste0("background:", GOLD, ";")
            ),
            card_body(
              style = "font-size:0.85rem;",
              tags$ul(
                tags$li(
                  tags$strong("Correlation overviews:"),
                  " A heatmap is the fastest way to scan pairwise relationships
                  across many numeric variables at once."
                ),
                tags$li(
                  tags$strong("Feature selection:"),
                  " In predictive modeling, heatmaps help spot multicollinearity
                  before fitting a model."
                ),
                tags$li(
                  tags$strong("When NOT to use it:"),
                  " If you have only 2-3 variables, scatter plots communicate
                  relationships better. Heatmaps also hide non-linear patterns —
                  always pair them with scatter-plot checks."
                )
              )
            )
          )
        )
      )
    )
  ),

  # ---- Footer ----
  div(
    style = paste0(
      "text-align:center; padding:10px 0 6px 0; margin-top:10px;",
      "font-size:0.8rem; color:#64748b; border-top:1px solid #e2e8f0;"
    ),
    "Gonzaga University | Dr. Vivek H. Patil"
  )
)


# ===========================================================================
# SERVER
# ===========================================================================
server <- function(input, output, session) {

  # -----------------------------------------------------------------
  # TAB 1 — plotly
  # -----------------------------------------------------------------
  plotly_base <- reactive({
    req(input$plotly_color)
    col_var <- sym(input$plotly_color)

    mpg_tt <- mpg |>
      mutate(
        color_var = as.factor(!!col_var),
        tooltip_txt = paste0(
          if (input$tt_model) paste0("<b>", model, "</b><br>") else "",
          if (input$tt_class) paste0("Class: ", class, "<br>") else "",
          if (input$tt_x)     paste0("Displacement: ", displ, "L<br>") else "",
          if (input$tt_y)     paste0("Hwy MPG: ", hwy) else ""
        )
      )

    ggplot(mpg_tt, aes(
      x     = displ,
      y     = hwy,
      color = color_var,
      text  = tooltip_txt
    )) +
      geom_point(size = input$pt_size, alpha = input$pt_alpha) +
      scale_color_brewer(palette = "Dark2") +
      labs(
        title    = "Engine Displacement vs. Highway MPG",
        subtitle = paste("Colored by:", input$plotly_color),
        x        = "Engine Displacement (L)",
        y        = "Highway MPG",
        color    = tools::toTitleCase(input$plotly_color)
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(color = NAVY, face = "bold"),
        plot.subtitle = element_text(color = "#475569")
      )
  })

  output$plotly_plot_area <- renderUI({
    if (input$plotly_mode == "static") {
      with_spinner(plotOutput("plotly_static", height = "440px"))
    } else {
      with_spinner(plotlyOutput("plotly_interactive", height = "440px"))
    }
  })

  output$plotly_static <- renderPlot({ plotly_base() }, res = 110)

  output$plotly_interactive <- renderPlotly({
    ggplotly(plotly_base(), tooltip = "text") |>
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
  })

  # -----------------------------------------------------------------
  # TAB 2 — DT
  # -----------------------------------------------------------------
  dt_options <- reactive({
    dom_parts <- c("t", "i", "p")
    if (input$dt_search)  dom_parts <- c("f", dom_parts)
    if (input$dt_length)  dom_parts <- c("l", dom_parts)
    dom_str <- paste0(dom_parts, collapse = "")

    list(
      dom        = dom_str,
      pageLength = input$dt_pageLen,
      lengthMenu = c(5, 10, 15, 20, 25, 50)
    )
  })

  output$dt_table <- renderDT({
    opts <- dt_options()
    filter_val <- if (input$dt_filters) "top" else "none"
    datatable(
      mpg,
      options  = opts,
      filter   = filter_val,
      rownames = FALSE,
      class    = "display compact stripe hover",
      caption  = htmltools::tags$caption(
        style = paste0("caption-side:top; font-weight:bold; font-size:1rem;",
                       "color:", NAVY, "; padding-bottom:6px;"),
        "mpg dataset — Fuel economy data (1999 & 2008)"
      )
    )
  })

  output$dt_code_out <- renderText({
    opts <- dt_options()
    filter_val <- if (input$dt_filters) '"top"' else '"none"'
    paste0(
      'datatable(\n',
      '  mpg,\n',
      '  filter   = ', filter_val, ',\n',
      '  rownames = FALSE,\n',
      '  options  = list(\n',
      '    dom        = "', opts$dom, '",\n',
      '    pageLength = ', opts$pageLength, '\n',
      '  )\n',
      ')'
    )
  })

  # -----------------------------------------------------------------
  # TAB 3 — Correlation Heatmap
  # -----------------------------------------------------------------

  # Select-all / Clear-all buttons
  observeEvent(input$hm_select_all, {
    updateCheckboxGroupInput(session, "hm_vars", selected = names(mtcars))
  })
  observeEvent(input$hm_clear_all, {
    updateCheckboxGroupInput(session, "hm_vars", selected = character(0))
  })

  hm_cor_data <- reactive({
    req(length(input$hm_vars) >= 2)
    validate(need(
      length(input$hm_vars) >= 2,
      "Please select at least 2 variables to compute correlations."
    ))
    cor_mat <- cor(mtcars[, input$hm_vars, drop = FALSE], use = "complete.obs")
    cor_mat |>
      as.data.frame() |>
      tibble::rownames_to_column("Var1") |>
      pivot_longer(-Var1, names_to = "Var2", values_to = "r") |>
      mutate(
        Var1  = factor(Var1, levels = input$hm_vars),
        Var2  = factor(Var2, levels = rev(input$hm_vars)),
        label = sprintf("%.2f", r)
      )
  })

  output$hm_plot <- renderPlotly({
    df <- hm_cor_data()

    p <- ggplot(df, aes(
      x    = Var1,
      y    = Var2,
      fill = r,
      text = paste0(Var1, " vs ", Var2, "\nr = ", label)
    )) +
      geom_tile(color = "white", linewidth = 0.6) +
      geom_text(aes(label = label), size = 3.2, color = "black") +
      labs(
        title = "Pairwise Correlation Matrix",
        x     = NULL,
        y     = NULL,
        fill  = "r"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title       = element_text(color = NAVY, face = "bold"),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        panel.grid       = element_blank()
      ) +
      coord_fixed()

    if (input$hm_palette == "viridis") {
      p <- p + scale_fill_viridis_c(limits = c(-1, 1), option = "D")
    } else {
      p <- p + scale_fill_gradient2(
        low = "#2166AC", mid = "white", high = "#B2182B",
        midpoint = 0, limits = c(-1, 1)
      )
    }

    ggplotly(p, tooltip = "text") |>
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
  })
}

# ===========================================================================
# Run
# ===========================================================================
shinyApp(ui, server)
