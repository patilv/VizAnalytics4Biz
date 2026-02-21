library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(forcats)
library(ggridges)
library(scales)

# Optional packages: graceful fallback if not installed
has_spinner  <- requireNamespace("shinycssloaders", quietly = TRUE)
has_gapminder <- requireNamespace("gapminder", quietly = TRUE)

# ---------------------------------------------------------------------------
# Gonzaga branding
# ---------------------------------------------------------------------------
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ---------------------------------------------------------------------------
# Chart metadata — tips, Cleveland & McGill ranking, best-for examples
# ---------------------------------------------------------------------------
chart_meta <- list(
  bar = list(
    tip      = "Use when comparing quantities across categories. Keep bars sorted by value unless order is meaningful.",
    encoding = "Position along a common scale (most accurate per Cleveland & McGill). Length encodes magnitude.",
    best_for = "Comparing revenue by region; survey response counts by answer choice."
  ),
  lollipop = list(
    tip      = "Same purpose as a bar chart, but cleaner when values are close together. Reduces ink without losing information.",
    encoding = "Position along a common scale (dot) plus length (segment). High perceptual accuracy with less clutter.",
    best_for = "Ranked performance metrics; slim alternative to bar charts when there are many categories."
  ),
  violin = list(
    tip      = "Use when comparing distributions across groups \u2014 shows shape, spread, and central tendency simultaneously.",
    encoding = "Position (center line) and area/width (density). Encodes distributional shape more richly than a box plot.",
    best_for = "Comparing salary distributions across departments; response-time distributions by server."
  ),

  ridgeline = list(
    tip      = "Use when comparing distributions across many ordered groups (e.g., months, departments). Very effective for showing shifts.",
    encoding = "Position along a common axis plus area (density curves). Overlapping layout maximises data density.",
    best_for = "Temperature distributions by month; sentiment score shifts over product releases."
  ),
  heatmap = list(
    tip      = "Use when showing a matrix of values across two categorical dimensions (e.g., manufacturer \u00d7 class).",
    encoding = "Color saturation / hue (lower accuracy per Cleveland & McGill). Best with a sequential or diverging palette.",
    best_for = "Correlation matrices; sales by product \u00d7 quarter; website clicks by day \u00d7 hour."
  ),
  bubble = list(
    tip      = "Use when showing relationships between three continuous variables (x, y, and bubble size).",
    encoding = "Position on two axes (high accuracy) plus area (lower accuracy). People underestimate area differences.",
    best_for = "GDP vs. life expectancy sized by population; ad spend vs. conversions sized by revenue."
  ),
  boxplot = list(
    tip      = "Use when you need to show median, IQR, and outliers at a glance. Good for non-statistician audiences.",
    encoding = "Position (median, quartiles) plus length (IQR whiskers). Compact summary of distribution.",
    best_for = "Comparing test scores across schools; process cycle times by production line."
  )
)

chart_choices <- c(
  "Bar chart"    = "bar",
  "Lollipop"     = "lollipop",
  "Violin"       = "violin",
  "Ridgeline"    = "ridgeline",
  "Heatmap"      = "heatmap",
  "Bubble chart"  = "bubble",
  "Box plot"     = "boxplot"
)

# ---------------------------------------------------------------------------
# Dataset options
# ---------------------------------------------------------------------------
dataset_choices <- c("mpg (fuel economy)" = "mpg")
if (has_gapminder) {
  dataset_choices <- c(dataset_choices, "gapminder (countries)" = "gapminder")
}

# ---------------------------------------------------------------------------
# Helper: wrap a UI element in a spinner if shinycssloaders is available
# ---------------------------------------------------------------------------
maybe_spinner <- function(ui_element) {
  if (has_spinner) {
    shinycssloaders::withSpinner(ui_element, color = NAVY, type = 6)
  } else {
    ui_element
  }
}

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

  # --- Sidebar ---
  sidebar = sidebar(
    width = 280, bg = "#f1f5f9",

    # Instruction accordion at top
    accordion(
      id = "instructions_acc",
      open = FALSE,
      accordion_panel(
        title = "How to Use This Explorer",
        icon  = bsicons::bs_icon("info-circle"),
        p("1. Choose a", tags$strong("dataset"), "from the dropdown."),
        p("2. Pick a", tags$strong("chart type"), "from the radio buttons."),
        p("3. Adjust", tags$strong("transparency"), "and toggle",
          tags$strong("flip coordinates"), "to see the effect."),
        p("4. Check", tags$strong("Enable comparison"), "to view two charts side by side."),
        p("5. Read the", tags$strong("Why this chart?"), "accordion below for perceptual guidance."),
        style = "font-size: 0.82rem; color: #374151; line-height: 1.55;"
      )
    ),

    tags$hr(),

    # Dataset selector
    selectInput("dataset", "Dataset:",
                choices  = dataset_choices,
                selected = "mpg"),

    tags$hr(),

    # Primary chart selector
    radioButtons("chart_type", "Chart type:",
                 choices  = chart_choices,
                 selected = "bar"),

    tags$hr(),

    # Comparison toggle
    checkboxInput("enable_compare", "Enable comparison", FALSE),

    # Secondary chart selector — only shown when comparison is on
    conditionalPanel(
      condition = "input.enable_compare",
      radioButtons("chart_type2", "Comparison chart:",
                   choices  = chart_choices,
                   selected = "lollipop")
    ),

    tags$hr(),

    # Controls
    sliderInput("alpha_val", "Transparency:", 0.3, 1.0, 0.75, step = 0.05),
    checkboxInput("flip_coords", "Flip coordinates", TRUE),

    tags$hr(),

    # "Why this chart?" accordion
    accordion(
      id   = "why_acc",
      open = "When to use",
      accordion_panel(
        title = "When to use",
        uiOutput("tip_when"),
        value = "When to use"
      ),
      accordion_panel(
        title = "Cleveland & McGill ranking",
        uiOutput("tip_encoding"),
        value = "Cleveland & McGill ranking"
      ),
      accordion_panel(
        title = "Best for",
        uiOutput("tip_bestfor"),
        value = "Best for"
      )
    )
  ),

  # --- Main panel ---
  # Primary chart card
  uiOutput("main_chart_ui"),

  # Footer
  tags$footer(
    style = paste0(
      "text-align:center; padding:10px 0 6px 0; margin-top:12px; ",
      "font-size:0.78rem; color:#64748b; border-top:1px solid #e2e8f0;"
    ),
    tags$span(
      style = paste0("color:", NAVY, "; font-weight:600;"),
      "Gonzaga University"
    ),
    " | Dr. Vivek H. Patil"
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Reactive: prepared data ------------------------------------------

  # Summarised data for bar / lollipop (one row per group)
  summary_data <- reactive({
    req(input$dataset)
    if (input$dataset == "gapminder" && has_gapminder) {
      gapminder::gapminder |>
        filter(year == max(year)) |>
        group_by(continent) |>
        summarise(avg_val = mean(lifeExp, na.rm = TRUE), .groups = "drop") |>
        mutate(group = fct_reorder(continent, avg_val))
    } else {
      mpg |>
        group_by(class) |>
        summarise(avg_val = mean(hwy), .groups = "drop") |>
        mutate(group = fct_reorder(class, avg_val))
    }
  })

  # Row-level data for distribution charts
  row_data <- reactive({
    req(input$dataset)
    if (input$dataset == "gapminder" && has_gapminder) {
      gapminder::gapminder |>
        filter(year == max(year)) |>
        mutate(
          group   = continent,
          y_val   = lifeExp,
          x_cont  = gdpPercap,
          y_cont  = lifeExp,
          size_var = pop / 1e6,
          color_var = continent,
          heat_g1 = continent,
          heat_g2 = factor(year)
        )
    } else {
      mpg |>
        mutate(
          group   = class,
          y_val   = hwy,
          x_cont  = displ,
          y_cont  = hwy,
          size_var = cty,
          color_var = class,
          heat_g1 = manufacturer,
          heat_g2 = drv
        )
    }
  })

  # Heatmap data
  heat_data <- reactive({
    req(input$dataset)
    if (input$dataset == "gapminder" && has_gapminder) {
      gapminder::gapminder |>
        filter(year >= 1992) |>
        group_by(continent, year) |>
        summarise(avg_val = mean(lifeExp, na.rm = TRUE), .groups = "drop") |>
        mutate(year = factor(year))
    } else {
      mpg |>
        group_by(manufacturer, drv) |>
        summarise(avg_val = mean(hwy), .groups = "drop")
    }
  })

  # Labels that depend on dataset
  ds_labels <- reactive({
    req(input$dataset)
    if (input$dataset == "gapminder" && has_gapminder) {
      list(
        bar_title    = "Average Life Expectancy by Continent",
        bar_x        = "Continent", bar_y = "Avg Life Expectancy",
        lollipop_title = "Average Life Expectancy by Continent (Lollipop)",
        violin_title = "Life Expectancy Distribution by Continent",
        violin_y     = "Life Expectancy",
        ridge_title  = "Life Expectancy Distribution (Ridgeline)",
        ridge_x      = "Life Expectancy",
        heat_title   = "Avg Life Expectancy: Continent \u00d7 Year",
        heat_x       = "Year", heat_y = "Continent",
        bubble_title = "GDP per Capita vs. Life Expectancy (Pop. as Size)",
        bubble_x     = "GDP per Capita", bubble_y = "Life Expectancy",
        bubble_size  = "Pop (M)", bubble_color = "Continent",
        box_title    = "Life Expectancy Distribution (Box Plot)",
        box_x        = "Continent", box_y = "Life Expectancy",
        footer       = "Charts use the gapminder dataset (latest year for most charts)."
      )
    } else {
      list(
        bar_title    = "Average Highway MPG by Vehicle Class",
        bar_x        = "Class", bar_y = "Avg Highway MPG",
        lollipop_title = "Average Highway MPG by Vehicle Class (Lollipop)",
        violin_title = "Highway MPG Distribution by Vehicle Class",
        violin_y     = "Highway MPG",
        ridge_title  = "Highway MPG Distribution (Ridgeline)",
        ridge_x      = "Highway MPG",
        heat_title   = "Avg Highway MPG: Manufacturer \u00d7 Drive Type",
        heat_x       = "Drive Type", heat_y = "Manufacturer",
        bubble_title = "Engine Size, Hwy MPG, City MPG (Bubble Size)",
        bubble_x     = "Engine Displacement (L)", bubble_y = "Highway MPG",
        bubble_size  = "City MPG", bubble_color = "Class",
        box_title    = "Highway MPG Distribution (Box Plot)",
        box_x        = "Vehicle Class", box_y = "Highway MPG",
        footer       = "Charts use the built-in mpg dataset."
      )
    }
  })

  # ---- Build a single chart given a chart type --------------------------
  build_chart <- function(chart_type) {
    req(chart_type)
    a   <- input$alpha_val
    lab <- ds_labels()
    sdat <- summary_data()
    rdat <- row_data()
    hdat <- heat_data()
    is_gm <- (input$dataset == "gapminder" && has_gapminder)

    p <- switch(chart_type,

      bar = ggplot(sdat, aes(x = group, y = avg_val)) +
        geom_col(fill = NAVY, alpha = a, width = 0.7) +
        labs(title = lab$bar_title, x = lab$bar_x, y = lab$bar_y),

      lollipop = ggplot(sdat, aes(x = group, y = avg_val)) +
        geom_segment(aes(xend = group, y = 0, yend = avg_val),
                     color = "#94a3b8", linewidth = 0.8) +
        geom_point(color = NAVY, size = 4, alpha = a) +
        labs(title = lab$lollipop_title, x = lab$bar_x, y = lab$bar_y),

      violin = ggplot(rdat, aes(x = group, y = y_val, fill = group)) +
        geom_violin(alpha = a, color = NA) +
        geom_boxplot(width = 0.12, fill = "white", outlier.size = 1) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = lab$violin_title, x = lab$bar_x, y = lab$violin_y) +
        theme(legend.position = "none"),

      ridgeline = ggplot(rdat, aes(x = y_val,
                                    y = fct_reorder(group, y_val),
                                    fill = after_stat(x))) +
        geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01,
                                     alpha = a) +
        scale_fill_viridis_c(option = "plasma", guide = "none") +
        labs(title = lab$ridge_title, x = lab$ridge_x, y = lab$bar_x),

      heatmap = {
        if (is_gm) {
          ggplot(hdat, aes(x = year, y = continent, fill = avg_val)) +
            geom_tile(color = "white", linewidth = 0.4) +
            geom_text(aes(label = round(avg_val, 1)), size = 3,
                      color = "white") +
            scale_fill_viridis_c(option = "mako", direction = -1,
                                 name = "Avg\nLife Exp") +
            labs(title = lab$heat_title, x = lab$heat_x, y = lab$heat_y)
        } else {
          ggplot(hdat, aes(x = drv, y = manufacturer, fill = avg_val)) +
            geom_tile(color = "white", linewidth = 0.4) +
            geom_text(aes(label = round(avg_val, 0)), size = 3,
                      color = "white") +
            scale_fill_viridis_c(option = "mako", direction = -1,
                                 name = "Avg\nHwy MPG") +
            scale_x_discrete(labels = c("4" = "4WD", "f" = "FWD",
                                        "r" = "RWD")) +
            labs(title = lab$heat_title, x = lab$heat_x, y = lab$heat_y)
        }
      },

      bubble = ggplot(rdat, aes(x = x_cont, y = y_cont,
                                 size = size_var, color = color_var)) +
        geom_point(alpha = a) +
        scale_size_area(max_size = 10, name = lab$bubble_size) +
        scale_color_brewer(palette = "Dark2") +
        labs(title = lab$bubble_title, x = lab$bubble_x, y = lab$bubble_y,
             color = lab$bubble_color),

      boxplot = ggplot(rdat, aes(x = fct_reorder(group, y_val),
                                  y = y_val, fill = group)) +
        geom_boxplot(alpha = a, outlier.color = RED, outlier.size = 2) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = lab$box_title, x = lab$box_x, y = lab$box_y) +
        theme(legend.position = "none")
    )

    validate(need(!is.null(p), "Could not render chart. Please try a different selection."))

    # Apply coord_flip if requested (skip for heatmap, ridgeline, bubble)
    if (input$flip_coords &&
        !chart_type %in% c("heatmap", "ridgeline", "bubble")) {
      p <- p + coord_flip()
    }

    p + theme_minimal(base_size = 13) +
      theme(plot.title = element_text(color = NAVY, face = "bold"))
  }

  # ---- Dynamic main-panel UI -------------------------------------------
  output$main_chart_ui <- renderUI({
    req(input$chart_type)
    lab <- ds_labels()
    compare <- isTRUE(input$enable_compare)

    if (compare) {
      # Side-by-side view
      card(
        full_screen = TRUE,
        card_header("Chart Type Explorer \u2014 Comparison View",
                    class = "bg-primary text-white"),
        card_body(
          fluidRow(
            column(6, maybe_spinner(
              plotOutput("chart_left", height = "430px")
            )),
            column(6, maybe_spinner(
              plotOutput("chart_right", height = "430px")
            ))
          )
        ),
        card_footer(
          paste(lab$footer,
                "Flip coordinates to see horizontal vs. vertical orientations."),
          style = "font-size:0.8rem; color:#64748b;"
        )
      )
    } else {
      # Single view
      card(
        full_screen = TRUE,
        card_header("Chart Type Explorer", class = "bg-primary text-white"),
        card_body(
          maybe_spinner(plotOutput("chart_single", height = "430px"))
        ),
        card_footer(
          paste(lab$footer,
                "Flip coordinates to see horizontal vs. vertical orientations."),
          style = "font-size:0.8rem; color:#64748b;"
        )
      )
    }
  })

  # ---- Render plots -----------------------------------------------------

  # Single-view plot
  output$chart_single <- renderPlot({
    req(input$chart_type, input$dataset, input$alpha_val)
    build_chart(input$chart_type)
  }, res = 110)

  # Left (primary) comparison plot
  output$chart_left <- renderPlot({
    req(input$enable_compare, input$chart_type, input$dataset, input$alpha_val)
    build_chart(input$chart_type)
  }, res = 110)

  # Right (secondary) comparison plot
  output$chart_right <- renderPlot({
    req(input$enable_compare, input$chart_type2, input$dataset, input$alpha_val)
    build_chart(input$chart_type2)
  }, res = 110)

  # ---- "Why this chart?" accordion text ---------------------------------

  # Helper to render a styled paragraph
  meta_p <- function(text) {
    tags$p(text,
           style = "font-size:0.82rem; color:#374151; line-height:1.55;")
  }

  output$tip_when <- renderUI({
    req(input$chart_type)
    meta <- chart_meta[[input$chart_type]]
    validate(need(!is.null(meta), "No metadata available for this chart type."))
    meta_p(meta$tip)
  })

  output$tip_encoding <- renderUI({
    req(input$chart_type)
    meta <- chart_meta[[input$chart_type]]
    validate(need(!is.null(meta), "No metadata available for this chart type."))
    meta_p(meta$encoding)
  })

  output$tip_bestfor <- renderUI({
    req(input$chart_type)
    meta <- chart_meta[[input$chart_type]]
    validate(need(!is.null(meta), "No metadata available for this chart type."))
    meta_p(meta$best_for)
  })
}

# ---------------------------------------------------------------------------
shinyApp(ui, server)
