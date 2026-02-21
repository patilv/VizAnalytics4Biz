library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(forcats)
library(patchwork)
library(scales)

# Optional: graceful fallback if shinycssloaders is not installed
has_spinner <- requireNamespace("shinycssloaders", quietly = TRUE)

# ---------------------------------------------------------------------------
# Gonzaga branding
# ---------------------------------------------------------------------------
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ---------------------------------------------------------------------------
# Chart type labels (used in UI and feedback lookup)
# ---------------------------------------------------------------------------
chart_labels <- c(
  "Bar chart (horizontal, sorted)",
  "Pie chart",
  "Lollipop chart",
  "Cleveland dot plot",
  "Waffle (proportional squares)",
  "Heatmap (single-row tiles)"
)

# ---------------------------------------------------------------------------
# Feedback bank: for each variable + chart combination
# ---------------------------------------------------------------------------
# Keys: variable -> chart index (1-6)
# Each entry has "quality" (good / okay / poor) and "explanation"
build_feedback <- function() {
  # --- Count of vehicles by class ---
  count_fb <- list(
    `1` = list(
      quality = "good",
      text = paste(
        "Excellent choice. A sorted horizontal bar chart uses position along a",
        "common scale -- the most accurate perceptual channel (Cleveland & McGill).",
        "Sorting by value lets readers rank categories instantly without scanning",
        "back and forth. This is the gold standard for comparing counts across",
        "unordered categories."
      )
    ),
    `2` = list(
      quality = "poor",
      text = paste(
        "Pie charts encode values as angles and areas -- both rank low on the",
        "Cleveland & McGill accuracy hierarchy. Comparing adjacent slices is",
        "feasible, but comparing non-adjacent slices (e.g., 'compact' vs.",
        "'midsize') requires mental rotation. With 7 vehicle classes the chart",
        "is crowded and labels overlap. A bar chart would convey the same",
        "information far more precisely."
      )
    ),
    `3` = list(
      quality = "good",
      text = paste(
        "A lollipop chart is functionally equivalent to a bar chart -- it uses",
        "position along a common scale -- but replaces the bar with a thin",
        "segment and a dot. This reduces visual clutter ('data ink') when values",
        "are close together, though the thinner marks can be slightly harder to",
        "scan quickly than full bars."
      )
    ),
    `4` = list(
      quality = "good",
      text = paste(
        "The Cleveland dot plot is one of the most efficient designs for comparing",
        "values across categories. It encodes magnitude purely through position,",
        "with minimal non-data ink. William Cleveland's research showed that dot",
        "plots outperform bar charts when the zero baseline is not meaningful, and",
        "perform equally well when it is."
      )
    ),
    `5` = list(
      quality = "okay",
      text = paste(
        "Waffle-style charts encode proportions through counted unit squares.",
        "They are better than pie charts because humans can count discrete units",
        "more accurately than estimate angles. However, for 7 categories the",
        "colors become hard to distinguish and precise comparison is slower than",
        "a bar chart. Best used with 2-4 categories for part-to-whole stories."
      )
    ),
    `6` = list(
      quality = "okay",
      text = paste(
        "A single-row heatmap encodes values through color intensity. Color",
        "saturation ranks lower than position on the perceptual accuracy scale,",
        "so readers can see broad patterns (dark vs. light) but struggle to",
        "make precise comparisons. This works better as part of a larger matrix;",
        "as a standalone chart for one variable, a bar chart is clearer."
      )
    )
  )

  # --- Average highway MPG by class ---
  hwy_fb <- list(
    `1` = list(
      quality = "good",
      text = paste(
        "A sorted bar chart clearly shows which vehicle classes have the best",
        "and worst highway fuel economy. The common baseline and aligned bars",
        "make differences easy to judge. Sorting reveals the ranking instantly."
      )
    ),
    `2` = list(
      quality = "poor",
      text = paste(
        "Pie charts are designed for part-to-whole relationships (proportions",
        "summing to 100%). Average MPG values do not form a meaningful whole,",
        "so the pie framing is misleading. Readers will incorrectly interpret",
        "each slice as a 'share' of something. A bar chart is strictly better",
        "for this comparison."
      )
    ),
    `3` = list(
      quality = "good",
      text = paste(
        "The lollipop chart works well here. Because all MPG values are in a",
        "similar numeric range, the thin segments reduce visual weight and let",
        "the dots -- positioned along a common scale -- do the communicating.",
        "Sorting makes the ranking immediately readable."
      )
    ),
    `4` = list(
      quality = "good",
      text = paste(
        "The Cleveland dot plot is arguably the best choice for comparing means",
        "across categories. It focuses attention on position (the most accurate",
        "channel) and removes unnecessary ink. The grid lines provide a common",
        "scale reference without heavy chart furniture."
      )
    ),
    `5` = list(
      quality = "poor",
      text = paste(
        "Waffle charts represent proportions through unit squares, but average",
        "MPG is a continuous summary, not a proportion. Binning continuous",
        "values into discrete squares loses precision and distorts differences.",
        "Stick with position-based encodings (bar, dot, lollipop) for continuous",
        "comparisons."
      )
    ),
    `6` = list(
      quality = "okay",
      text = paste(
        "The heatmap tile encodes MPG through color intensity. You can see that",
        "some classes are darker (higher MPG) than others, but precise ordering",
        "and magnitude differences are hard to judge from color alone. Better",
        "for a quick 'hot vs. cold' overview than for exact comparisons."
      )
    )
  )

  # --- Average city MPG by class ---
  cty_fb <- list(
    `1` = list(
      quality = "good",
      text = paste(
        "Sorted bars excel at showing city MPG rankings across classes. The",
        "horizontal layout gives room for long class labels and the common",
        "baseline makes magnitude comparison effortless."
      )
    ),
    `2` = list(
      quality = "poor",
      text = paste(
        "A pie chart is a poor choice for city MPG averages. These values do",
        "not represent parts of a whole, so the circular layout adds no",
        "insight and actively misleads. Angle-based judgments are less accurate",
        "than position-based ones."
      )
    ),
    `3` = list(
      quality = "good",
      text = paste(
        "The lollipop chart provides a clean, low-ink alternative to the bar",
        "chart. For city MPG averages, which span a narrow numeric range, the",
        "dot positions are easy to compare along the common scale."
      )
    ),
    `4` = list(
      quality = "good",
      text = paste(
        "Cleveland dot plots shine when differences are subtle. City MPG",
        "averages are relatively close together, and the dot plot's minimal",
        "design helps readers focus on the precise positions rather than being",
        "distracted by bar area."
      )
    ),
    `5` = list(
      quality = "poor",
      text = paste(
        "Waffle squares poorly represent continuous averages like city MPG.",
        "The rounding to whole squares obscures real differences -- e.g., a",
        "class averaging 15.8 looks the same as one averaging 15.2. Use bars",
        "or dots for continuous values."
      )
    ),
    `6` = list(
      quality = "okay",
      text = paste(
        "The heatmap provides a quick visual impression of which classes are",
        "more fuel-efficient, but the color gradient makes it hard to rank",
        "classes precisely. Useful as a compact summary element in a dashboard,",
        "but not ideal as the sole visualization for this question."
      )
    )
  )

  list(
    count = count_fb,
    hwy   = hwy_fb,
    cty   = cty_fb
  )
}

feedback_bank <- build_feedback()

# ---------------------------------------------------------------------------
# Spinner helper
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
    width = 290, bg = "#f1f5f9",

    # Instruction accordion
    accordion(
      id = "help_acc",
      open = FALSE,
      accordion_panel(
        title = "How to Use This Explorer",
        icon  = icon("circle-question"),
        p("This app shows the", tags$strong("same data"), "in six different",
          "chart types simultaneously so you can see how chart choice shapes",
          "the story."),
        tags$ol(
          tags$li("Choose a", tags$strong("variable"), "from the dropdown below."),
          tags$li("Examine the 2\u00d73 grid -- all six charts show identical data."),
          tags$li("Pick which chart you think", tags$strong("best answers the question"),
                  "using the radio buttons."),
          tags$li("Read the", tags$strong("feedback callout"), "to learn why your",
                  "choice is effective (or not).")
        ),
        p("Key insight: the data never changes, but the",
          tags$em("emphasis, clarity, and potential for misinterpretation"),
          "all depend on chart type.",
          style = "font-size:0.82rem; color:#475569; line-height:1.55;"),
        style = "font-size:0.82rem; color:#374151; line-height:1.55;"
      )
    ),

    tags$hr(style = "margin:8px 0;"),

    # Variable selector
    selectInput(
      "variable", "What to display:",
      choices = c(
        "Count of vehicles by class"     = "count",
        "Average highway MPG by class"   = "hwy",
        "Average city MPG by class"      = "cty"
      ),
      selected = "count"
    ),

    tags$hr(style = "margin:8px 0;"),

    # "Which tells the clearest story?" prompt
    tags$p(
      icon("magnifying-glass-chart"),
      tags$strong(" Which chart best answers the question?"),
      style = paste0("color:", NAVY, "; font-size:0.88rem; margin-bottom:6px;")
    ),
    radioButtons(
      "best_chart", NULL,
      choices  = setNames(as.character(seq_along(chart_labels)), chart_labels),
      selected = character(0)
    ),

    tags$hr(style = "margin:8px 0;"),

    # Feedback callout (appears after selection)
    uiOutput("feedback_callout")
  ),

  # --- Main panel ---
  card(
    full_screen = TRUE,
    card_header(
      "Same Data, Different Story",
      class = "bg-primary text-white"
    ),
    card_body(
      maybe_spinner(
        plotOutput("chart_grid", height = "680px")
      )
    ),
    card_footer(
      "All six charts display identical data from the built-in mpg dataset. ",
      "Only the chart type changes.",
      style = "font-size:0.8rem; color:#64748b;"
    )
  ),

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

  # ---- Reactive: summarised data ------------------------------------------
  chart_data <- reactive({
    req(input$variable)

    if (input$variable == "count") {
      ggplot2::mpg |>
        count(class, name = "value") |>
        mutate(class = fct_reorder(class, value))
    } else if (input$variable == "hwy") {
      ggplot2::mpg |>
        group_by(class) |>
        summarise(value = mean(hwy, na.rm = TRUE), .groups = "drop") |>
        mutate(class = fct_reorder(class, value))
    } else {
      ggplot2::mpg |>
        group_by(class) |>
        summarise(value = mean(cty, na.rm = TRUE), .groups = "drop") |>
        mutate(class = fct_reorder(class, value))
    }
  })

  # ---- Reactive: y-axis label ---------------------------------------------
  y_label <- reactive({
    switch(input$variable,
           count = "Number of Vehicles",
           hwy   = "Avg Highway MPG",
           cty   = "Avg City MPG")
  })

  # ---- Shared base theme --------------------------------------------------
  base_theme <- function(sz = 11) {
    theme_minimal(base_size = sz) +
      theme(
        plot.title       = element_text(color = NAVY, face = "bold", size = sz + 1,
                                        margin = margin(b = 4)),
        plot.title.position = "plot",
        axis.title       = element_text(size = sz - 1),
        panel.grid.minor = element_blank()
      )
  }

  # ---- Build individual chart functions -----------------------------------

  # 1. Bar chart (horizontal, sorted)
  make_bar <- function(df, ylab) {
    ggplot(df, aes(x = class, y = value)) +
      geom_col(fill = NAVY, alpha = 0.85, width = 0.7) +
      coord_flip() +
      labs(title = "1 \u2022 Bar Chart (sorted)", x = NULL, y = ylab) +
      base_theme()
  }

  # 2. Pie chart
  make_pie <- function(df, ylab) {
    pie_df <- df |>
      arrange(desc(value)) |>
      mutate(
        prop  = value / sum(value),
        label = paste0(class, "\n", round(prop * 100, 1), "%")
      )

    ggplot(pie_df, aes(x = "", y = value, fill = class)) +
      geom_col(width = 1, color = "white", linewidth = 0.5) +
      coord_polar(theta = "y") +
      scale_fill_manual(
        values = colorRampPalette(c(NAVY, GOLD, RED, "#5B8FA8", "#8B5E3C",
                                    "#6A994E", "#BC4749"))(nrow(pie_df))
      ) +
      labs(title = "2 \u2022 Pie Chart", fill = "Class") +
      theme_void(base_size = 11) +
      theme(
        plot.title      = element_text(color = NAVY, face = "bold", size = 12,
                                       margin = margin(b = 4)),
        legend.position = "right",
        legend.title    = element_text(size = 9),
        legend.text     = element_text(size = 8),
        legend.key.size = unit(0.35, "cm")
      )
  }

  # 3. Lollipop chart
  make_lollipop <- function(df, ylab) {
    ggplot(df, aes(x = class, y = value)) +
      geom_segment(aes(xend = class, y = 0, yend = value),
                   color = "#94a3b8", linewidth = 0.8) +
      geom_point(color = NAVY, size = 3.5) +
      coord_flip() +
      labs(title = "3 \u2022 Lollipop Chart", x = NULL, y = ylab) +
      base_theme()
  }

  # 4. Cleveland dot plot
  make_dotplot <- function(df, ylab) {
    ggplot(df, aes(x = value, y = class)) +
      geom_point(color = RED, size = 3.5) +
      labs(title = "4 \u2022 Cleveland Dot Plot", x = ylab, y = NULL) +
      base_theme() +
      theme(
        panel.grid.major.y = element_line(color = "#e2e8f0", linewidth = 0.4)
      )
  }

  # 5. Waffle-style proportional squares
  make_waffle <- function(df, ylab) {
    # Round values to integers for square-counting
    waffle_df <- df |>
      mutate(n_squares = round(value)) |>
      arrange(desc(n_squares))

    # Build a grid of unit squares
    total <- sum(waffle_df$n_squares)
    ncols <- ceiling(sqrt(total * 1.3))

    squares <- data.frame(
      class = rep(waffle_df$class, waffle_df$n_squares)
    )
    squares$idx <- seq_len(nrow(squares))
    squares$x   <- (squares$idx - 1) %% ncols
    squares$y   <- (squares$idx - 1) %/% ncols

    palette <- colorRampPalette(c(NAVY, GOLD, RED, "#5B8FA8", "#8B5E3C",
                                  "#6A994E", "#BC4749"))(nrow(waffle_df))
    names(palette) <- waffle_df$class

    ggplot(squares, aes(x = x, y = y, fill = class)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_manual(values = palette) +
      coord_equal() +
      labs(title = "5 \u2022 Waffle (proportional squares)", fill = "Class") +
      theme_void(base_size = 11) +
      theme(
        plot.title      = element_text(color = NAVY, face = "bold", size = 12,
                                       margin = margin(b = 4)),
        legend.position = "right",
        legend.title    = element_text(size = 9),
        legend.text     = element_text(size = 8),
        legend.key.size = unit(0.35, "cm")
      )
  }

  # 6. Heatmap (single row with geom_tile)
  make_heatmap <- function(df, ylab) {
    ggplot(df, aes(x = class, y = 1, fill = value)) +
      geom_tile(color = "white", linewidth = 0.8, height = 0.6) +
      geom_text(aes(label = round(value, 1)), color = "white",
                fontface = "bold", size = 3.5) +
      scale_fill_gradient(low = "#b0c4de", high = NAVY, name = ylab) +
      scale_x_discrete(position = "top") +
      labs(title = "6 \u2022 Heatmap (single-row tiles)", x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title       = element_text(color = NAVY, face = "bold", size = 12,
                                        margin = margin(b = 4)),
        axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        panel.grid       = element_blank(),
        legend.position  = "right",
        legend.title     = element_text(size = 9),
        legend.text      = element_text(size = 8),
        legend.key.height = unit(0.4, "cm")
      )
  }

  # ---- Render the 3x2 chart grid -----------------------------------------
  output$chart_grid <- renderPlot({
    df   <- chart_data()
    ylab <- y_label()

    validate(
      need(nrow(df) > 0, "No data available for the selected variable.")
    )

    p1 <- make_bar(df, ylab)
    p2 <- make_pie(df, ylab)
    p3 <- make_lollipop(df, ylab)
    p4 <- make_dotplot(df, ylab)
    p5 <- make_waffle(df, ylab)
    p6 <- make_heatmap(df, ylab)

    question_text <- switch(
      input$variable,
      count = "Question: How many vehicles are in each class?",
      hwy   = "Question: Which class has the best highway fuel economy?",
      cty   = "Question: Which class has the best city fuel economy?"
    )

    combined <- (p1 | p2) / (p3 | p4) / (p5 | p6) +
      plot_annotation(
        title    = question_text,
        subtitle = "Same data \u2014 six chart types \u2014 which tells the clearest story?",
        theme    = theme(
          plot.title    = element_text(color = NAVY, face = "bold", size = 16,
                                       hjust = 0.5, margin = margin(b = 2)),
          plot.subtitle = element_text(color = "#64748b", size = 12,
                                       hjust = 0.5, margin = margin(b = 10))
        )
      )

    combined
  }, res = 100)

  # ---- Feedback callout ---------------------------------------------------
  output$feedback_callout <- renderUI({
    req(input$best_chart)
    req(input$variable)

    idx <- input$best_chart
    fb  <- feedback_bank[[input$variable]][[idx]]

    validate(
      need(!is.null(fb), "No feedback available for this combination.")
    )

    # Style the callout based on quality
    border_color <- switch(fb$quality,
                           good = "#16a34a",
                           okay = GOLD,
                           poor = RED)
    icon_name <- switch(fb$quality,
                        good = "check-circle",
                        okay = "exclamation-triangle",
                        poor = "times-circle")
    quality_label <- switch(fb$quality,
                            good = "Strong choice",
                            okay = "Acceptable but limited",
                            poor = "Poor choice for this data")

    tags$div(
      style = paste0(
        "background:#f8fafc; border-left:4px solid ", border_color, "; ",
        "padding:12px 14px; border-radius:4px; margin-top:4px;"
      ),
      tags$div(
        style = paste0("font-weight:700; color:", border_color,
                       "; font-size:0.92rem; margin-bottom:6px;"),
        icon(icon_name),
        " ", quality_label
      ),
      tags$p(
        fb$text,
        style = "font-size:0.82rem; color:#334155; line-height:1.6; margin:0;"
      )
    )
  })
}

# ---------------------------------------------------------------------------
shinyApp(ui, server)
