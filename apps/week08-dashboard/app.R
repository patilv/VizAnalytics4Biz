library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(patchwork)

# Attempt to load shinycssloaders; set flag for graceful fallback
has_spinner <- requireNamespace("shinycssloaders", quietly = TRUE)
if (has_spinner) library(shinycssloaders)

# ── Gonzaga brand colors ──────────────────────────────────────────────────────
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ── Synthetic business data ───────────────────────────────────────────────────
set.seed(3847)
months      <- month.abb
revenue     <- c(210, 195, 230, 245, 220, 265, 280, 255, 290, 310, 295, 340)
customers   <- c(420, 390, 460, 480, 445, 520, 540, 510, 575, 610, 590, 670)
satisfaction <- c(7.8, 7.5, 8.1, 8.0, 7.9, 8.3, 8.4, 8.2, 8.5, 8.7, 8.6, 8.9)
product_mix <- data.frame(
  product = c("Software", "Services", "Hardware", "Training"),
  revenue = c(145, 98, 62, 35),
  stringsAsFactors = FALSE
)

biz_df <- data.frame(
  month        = factor(months, levels = months),
  revenue      = revenue,
  customers    = customers,
  satisfaction = satisfaction
)

# ── UI choices ────────────────────────────────────────────────────────────────
panel_choices <- c(
  "Revenue trend"       = "revenue",
  "Customer growth"     = "customers",
  "Satisfaction scores" = "satisfaction",
  "Product mix (bar)"   = "product"
)

layout_choices <- c(
  "2 \u00d7 2 grid"            = "2x2",
  "1 wide + 2 small (row)" = "1_2",
  "Single row"              = "row"
)

theme_choices <- c(
  "Gonzaga (navy/red)" = "gonzaga",
  "Neutral gray"       = "neutral",
  "High contrast"      = "hc"
)

# ── KPI computations (static) ────────────────────────────────────────────────
kpi_total_revenue    <- paste0("$", format(sum(revenue), big.mark = ","), "k")
kpi_total_customers  <- format(sum(customers), big.mark = ",")
kpi_avg_satisfaction <- sprintf("%.1f / 10", mean(satisfaction))
kpi_top_product      <- product_mix$product[which.max(product_mix$revenue)]

# ── Color palette helper ──────────────────────────────────────────────────────
get_colors <- function(theme) {
  switch(theme,
    gonzaga = list(main = NAVY,      accent = RED,      fill = "#eff6ff",
                   card_bg = NAVY,   card_fg = "#ffffff"),
    neutral = list(main = "#374151", accent = "#6b7280", fill = "#f9fafb",
                   card_bg = "#374151", card_fg = "#ffffff"),
    hc      = list(main = "#000000", accent = "#dc2626", fill = "#fefce8",
                   card_bg = "#000000", card_fg = "#fefce8")
  )
}

# ── Panel builder ─────────────────────────────────────────────────────────────
make_panel <- function(type, colors, show_title) {
  cl <- colors

  p <- switch(type,
    revenue = ggplot(biz_df, aes(x = month, y = revenue, group = 1)) +
      geom_area(fill = cl$fill, alpha = 0.6) +
      geom_line(color = cl$main, linewidth = 1.2) +
      geom_point(color = cl$accent, size = 2) +
      labs(title = if (show_title) "Monthly Revenue ($000s)" else NULL,
           x = NULL, y = "Revenue ($000s)"),

    customers = ggplot(biz_df, aes(x = month, y = customers, group = 1)) +
      geom_col(fill = cl$main, alpha = 0.75, width = 0.7) +
      labs(title = if (show_title) "Monthly Active Customers" else NULL,
           x = NULL, y = "Customers"),

    satisfaction = ggplot(biz_df, aes(x = month, y = satisfaction, group = 1)) +
      geom_line(color = cl$accent, linewidth = 1.4) +
      geom_point(color = cl$main, size = 2.5) +
      geom_hline(yintercept = 8, linetype = "dashed", color = "#94a3b8") +
      labs(title = if (show_title) "Customer Satisfaction (0-10)" else NULL,
           x = NULL, y = "Score") +
      ylim(7, 9.5),

    product = ggplot(product_mix, aes(x = reorder(product, revenue), y = revenue)) +
      geom_col(fill = cl$main, alpha = 0.8, width = 0.6) +
      geom_text(aes(label = paste0("$", revenue, "k")),
                hjust = -0.15, size = 3.5, color = cl$main) +
      coord_flip() +
      labs(title = if (show_title) "Revenue by Product Line" else NULL,
           x = NULL, y = "Revenue ($000s)") +
      scale_y_continuous(limits = c(0, 180))
  )

  p + theme_minimal(base_size = 11) +
    theme(plot.title = element_text(color = cl$main, face = "bold", size = 11),
          axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
}

# ── Dashboard design-critique checklist items ─────────────────────────────────
critique_items <- c(
  "Does the most important metric stand out?",
  "Is there a clear visual hierarchy?",
  "Is the color palette consistent and purposeful?",
  "Are axis labels and titles informative (not redundant)?",
  "Is the layout balanced with minimal wasted space?",
  "Would a first-time viewer understand it in under 10 seconds?"
)

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),

  # ── Sidebar ───────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 290, bg = "#f1f5f9",

    # --- Instruction accordion ---
    accordion(
      id = "sidebar_accord",
      open = FALSE,
      accordion_panel(
        title = "How to Use This Explorer",
        icon  = bsicons::bs_icon("info-circle"),
        p("1. Select which panels to include in your dashboard using the checkboxes below."),
        p("2. Choose a layout to control how panels are arranged."),
        p("3. Pick a color theme to see how palette choices affect readability."),
        p("4. Customize the title and subtitle to practice clear labeling."),
        p("5. Download your dashboard as a PNG and use the critique checklist to self-evaluate."),
        style = "font-size:0.82rem; color:#475569;"
      )
    ),

    tags$hr(),

    # --- Panel selection ---
    p(strong("Select panels to include:"),
      style = "color:#002967; margin-bottom:6px;"),
    checkboxGroupInput("panels", NULL,
                       choices  = panel_choices,
                       selected = c("revenue", "customers",
                                    "satisfaction", "product")),

    tags$hr(),

    # --- Layout ---
    radioButtons("layout", "Layout:",
                 choices = layout_choices, selected = "2x2"),

    tags$hr(),

    # --- Color theme ---
    radioButtons("theme_choice", "Color theme:",
                 choices = theme_choices, selected = "gonzaga"),

    tags$hr(),

    # --- Title / subtitle text inputs ---
    textInput("dash_title",
              "Dashboard title:",
              value = "Q4 Business Dashboard"),
    textInput("dash_subtitle",
              "Dashboard subtitle:",
              value = "Monthly performance overview"),
    checkboxInput("show_panel_titles", "Show panel titles", TRUE),

    tags$hr(),

    # --- Download button ---
    downloadButton("download_png", "Save as PNG",
                   class = "btn-sm w-100",
                   style = paste0("background-color:", NAVY,
                                  "; color:#fff; border:none; margin-bottom:10px;")),

    tags$hr(),

    # --- Critique checklist accordion ---
    accordion(
      id = "critique_accord",
      open = FALSE,
      accordion_panel(
        title = "Critique This Dashboard",
        icon  = bsicons::bs_icon("clipboard-check"),
        p("Check each principle your dashboard satisfies:",
          style = "font-size:0.82rem; color:#475569; margin-bottom:6px;"),
        checkboxGroupInput("critique", NULL,
                           choices = critique_items,
                           selected = character(0)),
        uiOutput("critique_score")
      )
    )
  ),

  # ── Main content ────────────────────────────────────────────────────────────

  # KPI value boxes
  layout_columns(
    col_widths = c(3, 3, 3, 3),
    fill = FALSE,
    uiOutput("vb_revenue"),
    uiOutput("vb_customers"),
    uiOutput("vb_satisfaction"),
    uiOutput("vb_product")
  ),

  # Dashboard card
  card(
    full_screen = TRUE,
    card_header("Business Dashboard Composer", class = "bg-primary text-white"),
    card_body(
      if (has_spinner) {
        shinycssloaders::withSpinner(
          plotOutput("dashboard", height = "520px"),
          type = 6, color = NAVY
        )
      } else {
        plotOutput("dashboard", height = "520px")
      }
    ),
    card_footer(
      "Mix and match panels, layouts, and color themes. ",
      "This is patchwork in action \u2014 the same tool you\u2019ll use for your capstone.",
      style = "font-size:0.8rem; color:#64748b;"
    )
  ),

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

  # ── Reactive: current color palette ────────────────────────────────────────
  current_colors <- reactive({
    req(input$theme_choice)
    get_colors(input$theme_choice)
  })

  # ── KPI value boxes (update with theme) ────────────────────────────────────
  output$vb_revenue <- renderUI({
    cl <- current_colors()
    value_box(
      title    = "Total Revenue",
      value    = kpi_total_revenue,
      showcase = bsicons::bs_icon("currency-dollar"),
      theme    = value_box_theme(bg = cl$card_bg, fg = cl$card_fg)
    )
  })

  output$vb_customers <- renderUI({
    cl <- current_colors()
    value_box(
      title    = "Total Customers",
      value    = kpi_total_customers,
      showcase = bsicons::bs_icon("people-fill"),
      theme    = value_box_theme(bg = cl$accent, fg = cl$card_fg)
    )
  })

  output$vb_satisfaction <- renderUI({
    cl <- current_colors()
    value_box(
      title    = "Avg Satisfaction",
      value    = kpi_avg_satisfaction,
      showcase = bsicons::bs_icon("emoji-smile"),
      theme    = value_box_theme(bg = GOLD, fg = "#ffffff")
    )
  })

  output$vb_product <- renderUI({
    cl <- current_colors()
    value_box(
      title    = "Top Product",
      value    = kpi_top_product,
      showcase = bsicons::bs_icon("trophy-fill"),
      theme    = value_box_theme(bg = cl$card_bg, fg = cl$card_fg)
    )
  })

  # ── Reactive: build composite plot ─────────────────────────────────────────
  dashboard_plot <- reactive({
    sel    <- input$panels
    colors <- current_colors()
    show_t <- input$show_panel_titles

    validate(
      need(length(sel) > 0,
           "Select at least one panel from the sidebar to build your dashboard.")
    )

    panels <- lapply(sel, make_panel, colors = colors, show_title = show_t)
    n <- length(panels)

    combined <- switch(input$layout,
      `2x2` = {
        if (n == 1) panels[[1]]
        else if (n == 2) panels[[1]] + panels[[2]]
        else if (n == 3) (panels[[1]] + panels[[2]]) / panels[[3]]
        else (panels[[1]] + panels[[2]]) / (panels[[3]] + panels[[4]])
      },
      `1_2` = {
        if (n == 1) panels[[1]]
        else if (n == 2) panels[[1]] | panels[[2]]
        else if (n >= 3)
          panels[[1]] | (Reduce("/", panels[2:min(n, 3)]))
        else panels[[1]]
      },
      row = Reduce("+", panels)
    )

    # Title & subtitle
    title_text    <- trimws(input$dash_title)
    subtitle_text <- trimws(input$dash_subtitle)

    if (nzchar(title_text) || nzchar(subtitle_text)) {
      combined <- combined +
        plot_annotation(
          title    = if (nzchar(title_text)) title_text else NULL,
          subtitle = if (nzchar(subtitle_text)) subtitle_text else NULL,
          theme    = theme(
            plot.title    = element_text(face = "bold", color = colors$main,
                                         size = 16, hjust = 0),
            plot.subtitle = element_text(color = "#475569", size = 12, hjust = 0)
          )
        )
    }

    combined
  })

  # ── Render dashboard plot ──────────────────────────────────────────────────
  output$dashboard <- renderPlot({
    req(input$panels)
    req(input$theme_choice)
    req(input$layout)
    dashboard_plot()
  }, res = 110)

  # ── Download handler ───────────────────────────────────────────────────────
  output$download_png <- downloadHandler(
    filename = function() {
      paste0("dashboard_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(dashboard_plot())
      ggsave(file, plot = dashboard_plot(),
             width = 12, height = 7, dpi = 150, bg = "white")
    }
  )

  # ── Critique score display ─────────────────────────────────────────────────
  output$critique_score <- renderUI({
    req(!is.null(input$critique))
    n_checked <- length(input$critique)
    n_total   <- length(critique_items)

    color <- if (n_checked <= 2) RED
             else if (n_checked <= 4) GOLD
             else "#22863a"

    tags$p(
      style = paste0("font-weight:600; color:", color,
                      "; font-size:0.9rem; margin-top:8px;"),
      sprintf("Score: %d / %d principles met", n_checked, n_total)
    )
  })
}

shinyApp(ui, server)
