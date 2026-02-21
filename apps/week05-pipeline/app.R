library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(shinycssloaders)

NAVY <- "#002967"
RED  <- "#C41E3A"
TOTAL_ROWS  <- nrow(ggplot2::mpg)
TOTAL_STEPS <- 5

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),

  # --- Sidebar -------------------------------------------------------------
  sidebar = sidebar(
    width = 290, bg = "#f1f5f9",

    # Instruction accordion
    accordion(
      id = "help_acc", open = FALSE,
      accordion_panel(
        title = "How to Use This Explorer",
        icon  = icon("circle-question"),
        p("Toggle the switches below to build a",
          tags$code("dplyr"), "pipeline step-by-step."),
        tags$ul(
          tags$li("Each switch adds the next verb to the pipeline."),
          tags$li("The table and plot update automatically."),
          tags$li("The generated R code appears at the bottom."),
          tags$li("Use the", tags$b("Table"), "and", tags$b("Plot"),
                  "tabs to explore results.")
        ),
        style = "font-size:0.82rem; color:#475569;"
      )
    ),

    tags$hr(style = "margin:6px 0;"),

    # Pipeline step progress bar
    p(strong("Pipeline Progress"), style = "color:#002967; margin-bottom:4px; font-size:0.85rem;"),
    uiOutput("step_progress"),
    tags$hr(style = "margin:8px 0;"),

    p(strong("Toggle pipeline steps:"),
      style = "color:#002967; margin-bottom:6px;"),
    p("Each switch adds the next dplyr verb. Watch the table change.",
      style = "font-size:0.8rem; color:#64748b; margin-bottom:10px;"),
    tags$hr(style = "margin:6px 0;"),

    # Step 1
    checkboxInput("step1", HTML("<b>Step 1:</b> filter() &mdash; keep selected classes"), TRUE),
    conditionalPanel(
      "input.step1",
      checkboxGroupInput(
        "keep_class", NULL,
        choices  = c("suv", "compact", "midsize", "pickup",
                     "subcompact", "2seater", "minivan"),
        selected = c("suv", "compact")
      )
    ),
    tags$hr(style = "margin:6px 0;"),

    # Step 2
    checkboxInput("step2", HTML("<b>Step 2:</b> select() &mdash; keep useful columns"), FALSE),
    tags$hr(style = "margin:6px 0;"),

    # Step 3
    checkboxInput("step3", HTML("<b>Step 3:</b> mutate() &mdash; create avg_mpg"), FALSE),
    tags$hr(style = "margin:6px 0;"),

    # Step 4
    checkboxInput("step4", HTML("<b>Step 4:</b> group_by() + summarise()"), FALSE),
    tags$hr(style = "margin:6px 0;"),

    # Step 5
    checkboxInput("step5", HTML("<b>Step 5:</b> arrange(desc(avg_hwy))"), FALSE),
    tags$hr(style = "margin:6px 0;"),

    # Pipeline code card
    card(
      card_header("Pipeline code", class = "bg-primary text-white",
                  style = "padding:6px 10px; font-size:0.85rem;"),
      card_body(
        verbatimTextOutput("pipe_code"),
        style = "padding:8px; font-size:0.78rem;"
      )
    )
  ),

  # --- Main panel -----------------------------------------------------------
  card(
    full_screen = TRUE,
    card_header(
      class = "bg-primary text-white",
      div(
        style = "display:flex; justify-content:space-between; align-items:center;",
        span("dplyr Pipeline Builder"),
        uiOutput("step_label", inline = TRUE)
      )
    ),
    card_body(
      # Row-count progress bar
      uiOutput("row_progress"),
      tags$div(style = "margin-top:10px;"),

      # Tabset: Table + Plot
      navset_tab(
        nav_panel(
          title = tagList(icon("table"), "Table"),
          tags$div(style = "margin-top:8px;"),
          withSpinner(DTOutput("data_tbl"), type = 6, color = NAVY)
        ),
        nav_panel(
          title = tagList(icon("chart-bar"), "Plot"),
          tags$div(style = "margin-top:8px;"),
          withSpinner(plotOutput("auto_plot", height = "480px"), type = 6, color = NAVY)
        )
      )
    ),
    card_footer(
      div(
        style = "display:flex; justify-content:space-between; align-items:center; font-size:0.8rem; color:#64748b;",
        span(paste0("Starting data: mpg dataset \u2014 ", TOTAL_ROWS,
                     " rows, ", ncol(ggplot2::mpg), " columns.")),
        span(
          style = paste0("color:", NAVY, "; font-weight:600;"),
          "Gonzaga University | Dr. Vivek H. Patil"
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # --- Active step count ---------------------------------------------------
  active_steps <- reactive({
    sum(c(
      isTRUE(input$step1),
      isTRUE(input$step2),
      isTRUE(input$step3),
      isTRUE(input$step4),
      isTRUE(input$step5)
    ))
  })

  # --- Pipeline progress bar -----------------------------------------------
  output$step_progress <- renderUI({
    n <- active_steps()
    pct <- round(n / TOTAL_STEPS * 100)
    bar_color <- if (n == 0) "#cbd5e1" else if (n < 3) NAVY else if (n < 5) "#1e6f5c" else RED
    div(
      style = "margin-bottom:2px;",
      div(
        style = paste0(
          "background:#e2e8f0; border-radius:6px; height:18px; ",
          "overflow:hidden; position:relative;"
        ),
        div(
          style = paste0(
            "background:", bar_color, "; width:", pct, "%; height:100%; ",
            "border-radius:6px; transition:width 0.4s ease;"
          )
        ),
        span(
          style = paste0(
            "position:absolute; top:0; left:0; right:0; text-align:center; ",
            "font-size:0.72rem; line-height:18px; font-weight:600; color:",
            if (pct > 40) "white" else "#334155", ";"
          ),
          paste0(n, " / ", TOTAL_STEPS, " steps enabled")
        )
      )
    )
  })

  # --- Core pipeline -------------------------------------------------------
  pipeline <- reactive({
    d <- ggplot2::mpg

    if (isTRUE(input$step1) && length(input$keep_class) > 0)
      d <- d |> filter(class %in% input$keep_class)

    if (isTRUE(input$step2))
      d <- d |> select(manufacturer, model, year, class, cty, hwy)

    if (isTRUE(input$step3))
      d <- d |> mutate(avg_mpg = round((cty + hwy) / 2, 1))

    if (isTRUE(input$step4))
      d <- d |>
        group_by(class) |>
        summarise(
          n       = n(),
          avg_cty = round(mean(cty), 1),
          avg_hwy = round(mean(hwy), 1),
          avg_mpg = round(mean((cty + hwy) / 2), 1),
          .groups = "drop"
        )

    if (isTRUE(input$step5))
      d <- d |> arrange(desc(if ("avg_hwy" %in% names(d)) avg_hwy else hwy))

    d
  })

  # --- Row / column label ---------------------------------------------------
  output$step_label <- renderUI({
    req(pipeline())
    d <- pipeline()
    tags$span(
      style = "font-size:0.82rem; font-weight:400;",
      icon("table"), " ",
      strong(nrow(d)), " rows \u00d7 ",
      strong(ncol(d)), " columns"
    )
  })

  # --- Row-count progress bar -----------------------------------------------
  output$row_progress <- renderUI({
    req(pipeline())
    d <- pipeline()
    current  <- nrow(d)
    pct      <- round(current / TOTAL_ROWS * 100)
    bar_col  <- if (pct > 66) NAVY else if (pct > 33) "#d97706" else RED
    div(
      div(
        style = "display:flex; justify-content:space-between; font-size:0.78rem; color:#475569; margin-bottom:2px;",
        span(paste0("Rows remaining: ", current, " / ", TOTAL_ROWS)),
        span(paste0(pct, "%"))
      ),
      div(
        style = paste0(
          "background:#e2e8f0; border-radius:4px; height:12px; ",
          "overflow:hidden; position:relative;"
        ),
        div(
          style = paste0(
            "background:", bar_col, "; width:", pct, "%; height:100%; ",
            "border-radius:4px; transition:width 0.4s ease;"
          )
        )
      )
    )
  })

  # --- Data table -----------------------------------------------------------
  output$data_tbl <- renderDT({
    req(pipeline())
    d <- pipeline()
    validate(need(nrow(d) > 0, "No rows match the current filter. Adjust your selections."))
    datatable(
      d,
      options  = list(
        pageLength = 8,
        scrollX    = TRUE,
        dom        = "tip",
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      class    = "compact stripe hover"
    )
  })

  # --- Auto-generated plot --------------------------------------------------
  output$auto_plot <- renderPlot({
    req(pipeline())
    d <- pipeline()
    validate(need(nrow(d) > 0, "No data to plot. Adjust your pipeline steps."))

    # Identify column types
    cat_cols <- names(d)[vapply(d, function(x) is.character(x) || is.factor(x), logical(1))]
    num_cols <- names(d)[vapply(d, is.numeric, logical(1))]

    validate(need(length(num_cols) > 0, "No numeric columns available for plotting."))

    if (length(cat_cols) > 0) {
      # Bar chart: first categorical vs first numeric (mean)
      cat_var <- cat_cols[1]
      num_var <- num_cols[1]

      plot_data <- d |>
        group_by(across(all_of(cat_var))) |>
        summarise(value = mean(.data[[num_var]], na.rm = TRUE), .groups = "drop") |>
        arrange(desc(value))

      p <- ggplot(plot_data, aes(
        x    = reorder(.data[[cat_var]], value),
        y    = value,
        fill = .data[[cat_var]]
      )) +
        geom_col(width = 0.7, show.legend = FALSE) +
        geom_text(aes(label = round(value, 1)),
                  hjust = -0.15, size = 3.5, color = "#334155") +
        coord_flip() +
        scale_fill_manual(
          values = colorRampPalette(c(NAVY, "#4a90d9", RED))(nrow(plot_data))
        ) +
        labs(
          title    = paste0("Mean ", num_var, " by ", cat_var),
          subtitle = paste0("Based on ", nrow(d), " rows in current pipeline result"),
          x = cat_var,
          y = paste0("Mean ", num_var)
        ) +
        theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          plot.title    = element_text(face = "bold", color = NAVY, size = 15),
          plot.subtitle = element_text(color = "#64748b", size = 11),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text  = element_text(color = "#334155"),
          axis.title = element_text(color = NAVY, face = "bold")
        )
    } else {
      # Histogram of first numeric column
      num_var <- num_cols[1]
      p <- ggplot(d, aes(x = .data[[num_var]])) +
        geom_histogram(
          bins  = min(30, max(5, nrow(d) %/% 3)),
          fill  = NAVY,
          color = "white",
          alpha = 0.85
        ) +
        labs(
          title    = paste0("Distribution of ", num_var),
          subtitle = paste0("Based on ", nrow(d), " rows in current pipeline result"),
          x = num_var,
          y = "Count"
        ) +
        theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          plot.title    = element_text(face = "bold", color = NAVY, size = 15),
          plot.subtitle = element_text(color = "#64748b", size = 11),
          panel.grid.minor = element_blank(),
          axis.text  = element_text(color = "#334155"),
          axis.title = element_text(color = NAVY, face = "bold")
        )
    }

    p
  }, res = 96)

  # --- Pipeline code --------------------------------------------------------
  output$pipe_code <- renderText({
    lines <- c("mpg")

    if (isTRUE(input$step1)) {
      cls <- paste0('"', paste(input$keep_class, collapse = '", "'), '"')
      lines <- c(lines, paste0('  filter(class %in% c(', cls, '))'))
    }
    if (isTRUE(input$step2))
      lines <- c(lines, '  select(manufacturer, model, year, class, cty, hwy)')
    if (isTRUE(input$step3))
      lines <- c(lines, '  mutate(avg_mpg = (cty + hwy) / 2)')
    if (isTRUE(input$step4))
      lines <- c(lines,
        '  group_by(class) |>',
        '  summarise(n = n(),',
        '            avg_cty = mean(cty),',
        '            avg_hwy = mean(hwy))')
    if (isTRUE(input$step5))
      lines <- c(lines, '  arrange(desc(avg_hwy))')

    if (length(lines) == 1) return("# Start: mpg (234 rows x 11 cols)\nmpg")
    paste(c(lines[1], paste0(lines[-1])), collapse = " |>\n")
  })
}

shinyApp(ui, server)
