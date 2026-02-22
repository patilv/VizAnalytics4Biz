library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Optional packages: graceful fallback if not installed
has_spinner   <- requireNamespace("shinycssloaders", quietly = TRUE)
if (has_spinner) library(shinycssloaders)
has_gapminder <- requireNamespace("gapminder", quietly = TRUE)

# ── Gonzaga brand colors ────────────────────────────────────────────────────
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ── Data setup ──────────────────────────────────────────────────────────────
# Use gapminder 2007 if available, otherwise build comparable synthetic data
if (has_gapminder) {
  gm <- gapminder::gapminder %>% filter(year == 2007)
} else {
  set.seed(2026)
  gm <- data.frame(
    country   = paste("Country", 1:50),
    continent = sample(c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                       50, replace = TRUE, prob = c(.25, .2, .25, .2, .1)),
    year      = 2007L,
    lifeExp   = rnorm(50, 68, 12),
    pop       = as.integer(runif(50, 5e5, 1.3e9)),
    gdpPercap = exp(rnorm(50, 8.5, 1.2)),
    stringsAsFactors = FALSE
  )
}

mpg_data <- ggplot2::mpg

# ── Theme presets ───────────────────────────────────────────────────────────
theme_presets <- list(
  professional = list(
    label       = "Professional",
    title_face  = "bold",
    title_color = NAVY,
    title_size  = 18,
    subtitle_color = "#475569",
    font_family = "sans",
    base_size   = 12,
    plot_bg     = "#ffffff",
    panel_bg    = "#f8fafc",
    grid_color  = "#e2e8f0",
    accent1     = NAVY,
    accent2     = GOLD,
    accent3     = "#3b82f6",
    accent4     = RED,
    header_bg   = NAVY,
    header_fg   = "#ffffff",
    card_border = NAVY,
    body_bg     = "#ffffff",
    text_color  = "#1e293b",
    rubric_bg   = "#f1f5f9"
  ),
  academic = list(
    label       = "Academic",
    title_face  = "bold.italic",
    title_color = "#1a1a2e",
    title_size  = 17,
    subtitle_color = "#555555",
    font_family = "serif",
    base_size   = 12,
    plot_bg     = "#fffef7",
    panel_bg    = "#fffef7",
    grid_color  = "#d4d0c8",
    accent1     = "#1a1a2e",
    accent2     = "#6b4c3b",
    accent3     = "#4a6741",
    accent4     = "#8b0000",
    header_bg   = "#1a1a2e",
    header_fg   = "#f5f0e8",
    card_border = "#1a1a2e",
    body_bg     = "#fffef7",
    text_color  = "#2c2c2c",
    rubric_bg   = "#f5f0e8"
  ),
  creative = list(
    label       = "Creative",
    title_face  = "bold",
    title_color = "#6d28d9",
    title_size  = 20,
    subtitle_color = "#7c3aed",
    font_family = "sans",
    base_size   = 12,
    plot_bg     = "#fefce8",
    panel_bg    = "#fefce8",
    grid_color  = "#e9d5ff",
    accent1     = "#6d28d9",
    accent2     = "#db2777",
    accent3     = "#059669",
    accent4     = "#ea580c",
    header_bg   = "#6d28d9",
    header_fg   = "#ffffff",
    card_border = "#6d28d9",
    body_bg     = "#fefce8",
    text_color  = "#1c1917",
    rubric_bg   = "#f3e8ff"
  ),
  dashboard = list(
    label       = "Dashboard-style",
    title_face  = "bold",
    title_color = "#111827",
    title_size  = 16,
    subtitle_color = "#6b7280",
    font_family = "sans",
    base_size   = 11,
    plot_bg     = "#ffffff",
    panel_bg    = "#f9fafb",
    grid_color  = "#e5e7eb",
    accent1     = "#2563eb",
    accent2     = "#16a34a",
    accent3     = "#d97706",
    accent4     = "#dc2626",
    header_bg   = "#111827",
    header_fg   = "#f9fafb",
    card_border = "#374151",
    body_bg     = "#f3f4f6",
    text_color  = "#111827",
    rubric_bg   = "#f3f4f6"
  )
)

theme_choices <- c(
  "Professional"    = "professional",
  "Academic"        = "academic",
  "Creative"        = "creative",
  "Dashboard-style" = "dashboard"
)

chart_choices <- c(
  "Scatter plot"  = "scatter",
  "Bar chart"     = "bar",
  "Line chart"    = "line",
  "Faceted plot"  = "faceted"
)

# ── Rubric criteria ─────────────────────────────────────────────────────────
rubric <- data.frame(
  criterion   = c("Technical Quality", "Design Principles",
                   "Narrative & Analysis", "Reflection"),
  weight      = c("30%", "25%", "25%", "20%"),
  description = c(
    "Correct use of R/ggplot2, clean code, reproducibility, appropriate chart types for the data.",
    "Effective use of color, layout, typography, and Tufte/Few design principles covered in class.",
    "Clear written analysis that interprets the visualization, tells a data story, and draws insight.",
    "Thoughtful Ignatian reflection connecting the project to ethical data use and the common good."
  ),
  stringsAsFactors = FALSE
)

# ── ggplot theme builder ────────────────────────────────────────────────────
build_ggtheme <- function(preset) {
  theme_minimal(base_size = preset$base_size, base_family = preset$font_family) +
    theme(
      plot.background  = element_rect(fill = preset$plot_bg, color = NA),
      panel.background = element_rect(fill = preset$panel_bg, color = NA),
      panel.grid.major = element_line(color = preset$grid_color, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = preset$title_face,
                                      color = preset$title_color,
                                      size  = preset$title_size,
                                      margin = margin(b = 4)),
      plot.subtitle    = element_text(color = preset$subtitle_color,
                                      size  = preset$base_size,
                                      margin = margin(b = 8)),
      axis.title       = element_text(color = preset$text_color, size = preset$base_size - 1),
      axis.text        = element_text(color = preset$text_color, size = preset$base_size - 2),
      legend.position  = "bottom",
      legend.text      = element_text(size = preset$base_size - 2),
      plot.margin      = margin(12, 12, 12, 12)
    )
}

# ── Chart builders ──────────────────────────────────────────────────────────
build_chart <- function(chart_type, preset) {
  gg_theme <- build_ggtheme(preset)

  switch(chart_type,

    scatter = {
      ggplot(mpg_data, aes(x = displ, y = hwy, color = class)) +
        geom_point(size = 2.5, alpha = 0.75) +
        scale_color_manual(values = c(
          preset$accent1, preset$accent2, preset$accent3, preset$accent4,
          "#64748b", "#0891b2", "#84cc16"
        )) +
        labs(
          title    = "Engine Size vs. Highway MPG",
          subtitle = "Each point is a vehicle model; color encodes class",
          x = "Displacement (litres)", y = "Highway MPG", color = "Class"
        ) +
        gg_theme
    },

    bar = {
      bar_df <- gm %>%
        group_by(continent) %>%
        summarise(avg_life = mean(lifeExp, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(avg_life))

      ggplot(bar_df, aes(x = reorder(continent, avg_life), y = avg_life)) +
        geom_col(fill = preset$accent1, alpha = 0.85, width = 0.65) +
        geom_text(aes(label = sprintf("%.1f", avg_life)),
                  hjust = -0.15, size = 3.5, color = preset$text_color) +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title    = "Average Life Expectancy by Continent (2007)",
          subtitle = "Data from Gapminder; bars sorted by value",
          x = NULL, y = "Life expectancy (years)"
        ) +
        gg_theme
    },

    line = {
      if (has_gapminder) {
        line_df <- gapminder::gapminder %>%
          filter(continent %in% c("Africa", "Americas", "Asia", "Europe")) %>%
          group_by(continent, year) %>%
          summarise(avg_life = mean(lifeExp, na.rm = TRUE), .groups = "drop")
      } else {
        line_df <- expand.grid(
          continent = c("Africa", "Americas", "Asia", "Europe"),
          year = seq(1952, 2007, by = 5),
          stringsAsFactors = FALSE
        )
        set.seed(99)
        line_df$avg_life <- 45 + cumsum(runif(nrow(line_df), 0.3, 1.5))
      }

      ggplot(line_df, aes(x = year, y = avg_life, color = continent)) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 1.8) +
        scale_color_manual(values = c(
          preset$accent1, preset$accent2, preset$accent3, preset$accent4
        )) +
        labs(
          title    = "Life Expectancy Over Time by Continent",
          subtitle = "Trends from 1952 to 2007 (Gapminder)",
          x = "Year", y = "Avg. life expectancy", color = "Continent"
        ) +
        gg_theme
    },

    faceted = {
      facet_df <- mpg_data %>%
        filter(class %in% c("compact", "midsize", "suv", "pickup"))

      ggplot(facet_df, aes(x = displ, y = hwy)) +
        geom_point(color = preset$accent1, alpha = 0.7, size = 2) +
        geom_smooth(method = "lm", se = FALSE,
                    color = preset$accent2, linewidth = 0.9) +
        facet_wrap(~ class, nrow = 2) +
        labs(
          title    = "Engine Size vs. Highway MPG by Vehicle Class",
          subtitle = "Each panel shows one class with a linear trend line",
          x = "Displacement (litres)", y = "Highway MPG"
        ) +
        gg_theme +
        theme(strip.text = element_text(face = "bold",
                                        color = preset$header_fg,
                                        size  = preset$base_size - 1),
              strip.background = element_rect(fill = preset$header_bg, color = NA))
    }
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- page_sidebar(
  title = NULL,
  fillable = FALSE,
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),

  # ── Sidebar ──────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 310, bg = "#f1f5f9",

    # --- Instruction accordion ---
    accordion(
      id = "sidebar_instructions",
      open = FALSE,
      accordion_panel(
        title = "How to Use This Explorer",
        icon  = bsicons::bs_icon("info-circle"),
        p("1. Choose a portfolio theme to see how visual styling affects the feel of your capstone."),
        p("2. Pick a chart type to preview different visualization options."),
        p("3. Enter your project title, analysis write-up, and Ignatian reflection."),
        p("4. The preview panel updates live so you can iterate on your portfolio layout."),
        p("5. Review the rubric accordion below to self-check your capstone."),
        style = "font-size:0.82rem; color:#475569;"
      )
    ),

    tags$hr(),

    # --- Theme selector ---
    selectInput("theme_select", "Portfolio theme:",
                choices  = theme_choices,
                selected = "professional"),

    # --- Chart selector ---
    selectInput("chart_select", "Example chart:",
                choices  = chart_choices,
                selected = "scatter"),

    tags$hr(),

    # --- Project title ---
    textInput("project_title",
              "Project title:",
              value = "My Capstone Project"),

    # --- Analysis write-up ---
    textAreaInput("analysis_text",
                  "Analysis write-up (2-3 sentences):",
                  value = "",
                  rows  = 4,
                  placeholder = "This visualization reveals that... The most notable pattern is... This matters because..."),

    # --- Ignatian reflection ---
    textAreaInput("reflection_text",
                  "Ignatian reflection:",
                  value = "",
                  rows  = 4,
                  placeholder = "Reflecting on this project, I considered how data visualization can serve the common good by... An ethical concern I encountered was..."),

    tags$hr(),

    # --- Capstone rubric accordion ---
    accordion(
      id = "rubric_accord",
      open = FALSE,
      accordion_panel(
        title = "Capstone Rubric Reference",
        icon  = bsicons::bs_icon("clipboard-check"),

        # Technical Quality
        tags$div(
          style = "margin-bottom:10px;",
          tags$span(
            style = paste0("font-weight:700; color:", NAVY, "; font-size:0.88rem;"),
            "Technical Quality (30%)"
          ),
          tags$p(
            style = "font-size:0.78rem; color:#475569; margin:2px 0 0 0;",
            "Correct use of R/ggplot2, clean code, reproducibility, appropriate chart types for the data."
          )
        ),

        # Design Principles
        tags$div(
          style = "margin-bottom:10px;",
          tags$span(
            style = paste0("font-weight:700; color:", NAVY, "; font-size:0.88rem;"),
            "Design Principles (25%)"
          ),
          tags$p(
            style = "font-size:0.78rem; color:#475569; margin:2px 0 0 0;",
            "Effective use of color, layout, typography, and Tufte/Few design principles covered in class."
          )
        ),

        # Narrative & Analysis
        tags$div(
          style = "margin-bottom:10px;",
          tags$span(
            style = paste0("font-weight:700; color:", NAVY, "; font-size:0.88rem;"),
            "Narrative & Analysis (25%)"
          ),
          tags$p(
            style = "font-size:0.78rem; color:#475569; margin:2px 0 0 0;",
            "Clear written analysis that interprets the visualization, tells a data story, and draws insight."
          )
        ),

        # Reflection
        tags$div(
          style = "margin-bottom:0;",
          tags$span(
            style = paste0("font-weight:700; color:", NAVY, "; font-size:0.88rem;"),
            "Reflection (20%)"
          ),
          tags$p(
            style = "font-size:0.78rem; color:#475569; margin:2px 0 0 0;",
            "Thoughtful Ignatian reflection connecting the project to ethical data use and the common good."
          )
        )
      )
    )
  ),

  # ── Main content area ────────────────────────────────────────────────────

  # Portfolio preview card
  uiOutput("portfolio_card"),

  # Footer attribution
  tags$footer(
    style = paste0(
      "text-align:center; padding:12px 0; margin-top:16px; ",
      "font-size:0.82rem; color:#ffffff; background-color:", NAVY, ";"
    ),
    HTML("Gonzaga University &nbsp;|&nbsp; Dr.&nbsp;Vivek&nbsp;H.&nbsp;Patil")
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Reactive: current theme preset ──────────────────────────────────────
  current_preset <- reactive({
    req(input$theme_select)
    theme_presets[[input$theme_select]]
  })

  # ── Reactive: chart ─────────────────────────────────────────────────────
  chart_plot <- reactive({
    req(input$chart_select)
    preset <- current_preset()
    validate(
      need(input$chart_select %in% names(chart_choices) |
             input$chart_select %in% chart_choices,
           "Please select a valid chart type.")
    )
    build_chart(input$chart_select, preset)
  })

  # ── Render chart ────────────────────────────────────────────────────────
  output$portfolio_chart <- renderPlot({
    chart_plot()
  }, res = 110)

  # ── Reactive: formatted text sections ───────────────────────────────────
  analysis_html <- reactive({
    txt <- trimws(input$analysis_text)
    if (!nzchar(txt)) {
      return(tags$p(
        style = "color:#94a3b8; font-style:italic; font-size:0.9rem;",
        "Your analysis will appear here. Use the text box in the sidebar to write 2-3 sentences interpreting the visualization above."
      ))
    }
    tags$p(style = "font-size:0.95rem; line-height:1.6;", txt)
  })

  reflection_html <- reactive({
    txt <- trimws(input$reflection_text)
    if (!nzchar(txt)) {
      return(tags$p(
        style = "color:#94a3b8; font-style:italic; font-size:0.9rem;",
        "Your Ignatian reflection will appear here. Consider how your analysis connects to ethical data use, justice, and the common good."
      ))
    }
    tags$p(style = "font-size:0.95rem; line-height:1.6;", txt)
  })

  # ── Render the entire portfolio card (styled per theme) ─────────────────
  output$portfolio_card <- renderUI({
    preset <- current_preset()
    req(preset)

    project_title <- trimws(input$project_title)
    if (!nzchar(project_title)) project_title <- "Untitled Project"

    # Determine font family CSS
    font_css <- if (preset$font_family == "serif") {
      "font-family: Georgia, 'Times New Roman', serif;"
    } else {
      "font-family: 'Inter', -apple-system, sans-serif;"
    }

    # Build the portfolio preview
    tags$div(
      style = paste0(
        "border: 2px solid ", preset$card_border, "; ",
        "border-radius: 8px; overflow: hidden; ",
        "background-color: ", preset$body_bg, "; ",
        font_css
      ),

      # ── Header band ──────────────────────────────────────────────────
      tags$div(
        style = paste0(
          "background-color:", preset$header_bg, "; ",
          "color:", preset$header_fg, "; ",
          "padding: 18px 24px; "
        ),
        tags$h2(
          style = paste0(
            "margin:0; font-size:", preset$title_size, "px; ",
            "font-weight:700; letter-spacing:-0.02em;"
          ),
          project_title
        ),
        tags$p(
          style = "margin:4px 0 0 0; font-size:0.85rem; opacity:0.85;",
          paste0("Theme: ", preset$label, "  |  Visual Analytics & Data Storytelling with R")
        )
      ),

      # ── Body ─────────────────────────────────────────────────────────
      tags$div(
        style = paste0(
          "padding: 20px 24px; ",
          "color:", preset$text_color, ";"
        ),

        # --- Visualization section ---
        tags$h4(
          style = paste0(
            "color:", preset$title_color, "; ",
            "font-weight:600; margin-bottom:8px; ",
            "border-bottom: 2px solid ", preset$accent2, "; ",
            "padding-bottom:4px; display:inline-block;"
          ),
          "Visualization"
        ),

        tags$div(
          style = paste0(
            "background-color:", preset$panel_bg, "; ",
            "border: 1px solid ", preset$grid_color, "; ",
            "border-radius: 6px; padding: 8px; margin-bottom: 20px;"
          ),
          if (has_spinner) {
            shinycssloaders::withSpinner(
              plotOutput("portfolio_chart", height = "420px"),
              type = 6, color = NAVY
            )
          } else {
            plotOutput("portfolio_chart", height = "420px")
          }
        ),

        # --- Analysis section ---
        tags$h4(
          style = paste0(
            "color:", preset$title_color, "; ",
            "font-weight:600; margin-bottom:6px; ",
            "border-bottom: 2px solid ", preset$accent2, "; ",
            "padding-bottom:4px; display:inline-block;"
          ),
          "Analysis"
        ),
        tags$div(
          style = paste0(
            "background-color:", preset$rubric_bg, "; ",
            "border-radius: 6px; padding: 14px 16px; margin-bottom: 20px;"
          ),
          analysis_html()
        ),

        # --- Reflection section ---
        tags$h4(
          style = paste0(
            "color:", preset$title_color, "; ",
            "font-weight:600; margin-bottom:6px; ",
            "border-bottom: 2px solid ", preset$accent2, "; ",
            "padding-bottom:4px; display:inline-block;"
          ),
          "Ignatian Reflection"
        ),
        tags$div(
          style = paste0(
            "background-color:", preset$rubric_bg, "; ",
            "border-radius: 6px; padding: 14px 16px; margin-bottom: 16px;"
          ),
          reflection_html()
        ),

        # --- Rubric self-check inline ---
        tags$div(
          style = paste0(
            "border-top: 1px solid ", preset$grid_color, "; ",
            "padding-top: 14px; margin-top: 8px;"
          ),
          tags$h5(
            style = paste0("color:", preset$title_color, "; font-weight:600; margin-bottom:8px;"),
            "Quick Self-Check"
          ),
          tags$div(
            style = "display:flex; flex-wrap:wrap; gap:8px;",
            tags$span(
              style = paste0(
                "background-color:", preset$accent1, "; color:#fff; ",
                "padding:4px 10px; border-radius:12px; font-size:0.78rem; font-weight:600;"
              ),
              "Technical 30%"
            ),
            tags$span(
              style = paste0(
                "background-color:", preset$accent2, "; color:#fff; ",
                "padding:4px 10px; border-radius:12px; font-size:0.78rem; font-weight:600;"
              ),
              "Design 25%"
            ),
            tags$span(
              style = paste0(
                "background-color:", preset$accent3, "; color:#fff; ",
                "padding:4px 10px; border-radius:12px; font-size:0.78rem; font-weight:600;"
              ),
              "Narrative 25%"
            ),
            tags$span(
              style = paste0(
                "background-color:", preset$accent4, "; color:#fff; ",
                "padding:4px 10px; border-radius:12px; font-size:0.78rem; font-weight:600;"
              ),
              "Reflection 20%"
            )
          ),
          tags$p(
            style = "font-size:0.8rem; color:#64748b; margin-top:8px;",
            "Does your portfolio address all four rubric dimensions? Open the sidebar rubric for details."
          )
        )
      ),

      # ── Footer band ──────────────────────────────────────────────────
      tags$div(
        style = paste0(
          "background-color:", preset$header_bg, "; ",
          "color:", preset$header_fg, "; ",
          "padding: 10px 24px; font-size:0.78rem; ",
          "text-align:center; opacity:0.9;"
        ),
        HTML("Portfolio Showcase Template &nbsp;&bull;&nbsp; Visual Analytics &amp; Data Storytelling with R &nbsp;&bull;&nbsp; Gonzaga University")
      )
    )
  })
}

# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui, server)
