library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)

# ---------------------------------------------------------------------------
# Gonzaga palette
# ---------------------------------------------------------------------------
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ---------------------------------------------------------------------------
# Spinner helper -- wraps output in a spinner if shinycssloaders is available,
# otherwise returns the output unchanged.
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
  title = "Preattentive Processing Lab",
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),

  # -- Custom CSS -----------------------------------------------------------
  tags$head(tags$style(HTML(sprintf("
    .gonzaga-header {
      background: %s;
      color: white;
      padding: 14px 24px;
      font-size: 1.35rem;
      font-weight: 700;
      letter-spacing: 0.02em;
      border-radius: 8px 8px 0 0;
      margin-bottom: 0;
    }
    .gonzaga-footer {
      text-align: center;
      padding: 10px 0;
      font-size: 0.8rem;
      color: #64748b;
      border-top: 1px solid #e2e8f0;
      margin-top: 12px;
    }
    .timer-display {
      font-size: 2.5rem;
      font-weight: 800;
      color: %s;
      text-align: center;
      padding: 8px 0;
    }
    .timer-waiting {
      color: #94a3b8;
    }
    .status-msg {
      text-align: center;
      font-size: 1.05rem;
      padding: 4px 0;
      min-height: 32px;
    }
    .target-field {
      cursor: crosshair;
    }
    .explanation-card {
      background: #f8fafc;
      border-left: 4px solid %s;
      padding: 12px 16px;
      border-radius: 4px;
      font-size: 0.88rem;
      line-height: 1.6;
      color: #334155;
    }
    .conjunction-instructions {
      font-size: 0.9rem;
      color: #475569;
      padding: 6px 0 10px 0;
    }
  ", NAVY, NAVY, GOLD)))),

  # -- Header ---------------------------------------------------------------
  div(class = "gonzaga-header", "Preattentive Processing Lab"),

  # -- Instruction accordion ------------------------------------------------
  accordion(
    id = "help_accordion",
    open = FALSE,
    accordion_panel(
      title = "How to Use This Explorer",
      icon  = bsicons::bs_icon("info-circle"),
      p(
        "This lab has three tabs that let you experience preattentive visual ",
        "processing first-hand."
      ),
      tags$ul(
        tags$li(
          tags$strong("Spot the Target:"),
          " Click Start, then click the odd-one-out point as fast as you can. ",
          "Try each attribute and compare your reaction times."
        ),
        tags$li(
          tags$strong("Conjunction Search:"),
          " See why combining two attributes (e.g., color AND shape) forces slow, ",
          "serial search instead of instant pop-out."
        ),
        tags$li(
          tags$strong("Design Your Encoding:"),
          " Map a categorical variable to different visual channels and judge ",
          "which encoding works best."
        )
      ),
      p(
        "Preattentive features are detected in under 250 ms regardless of the ",
        "number of distractors. Conjunction targets require serial attention and ",
        "take longer as the display grows.",
        style = "color:#475569; font-size:0.88rem;"
      )
    )
  ),

  # -- Main tab panel -------------------------------------------------------
  navset_card_tab(
    id = "main_tabs",
    full_screen = TRUE,

    # =====================================================================
    # TAB 1 -- Spot the Target
    # =====================================================================
    nav_panel(
      title = "Spot the Target",
      icon  = bsicons::bs_icon("bullseye"),
      layout_sidebar(
        sidebar = sidebar(
          width = 280,
          bg    = "#f1f5f9",
          selectInput(
            "t1_attribute", "Preattentive attribute:",
            choices = c("Color" = "color", "Size" = "size",
                        "Shape" = "shape", "Orientation" = "orientation")
          ),
          sliderInput("t1_n", "Background points:", 30, 200, 80, step = 10),
          hr(),
          actionButton(
            "t1_start", "Start",
            class = "btn-primary w-100",
            icon  = icon("play")
          ),
          actionButton(
            "t1_reset", "Reset History",
            class = "btn-outline-secondary w-100 mt-2",
            icon  = icon("rotate-left")
          ),
          hr(),
          div(
            class = "timer-display",
            id = "t1_timer_box",
            textOutput("t1_timer_text", inline = TRUE)
          ),
          div(
            class = "status-msg",
            uiOutput("t1_status")
          )
        ),
        layout_columns(
          col_widths = c(7, 5),
          card(
            card_header("Find the target!", class = "bg-primary text-white"),
            card_body(
              class = "target-field",
              with_spinner(
                plotOutput("t1_plot", height = "440px", click = "t1_click")
              )
            )
          ),
          card(
            card_header("Reaction Times by Attribute", class = "bg-primary text-white"),
            card_body(
              with_spinner(plotOutput("t1_history", height = "440px"))
            )
          )
        )
      )
    ),

    # =====================================================================
    # TAB 2 -- Conjunction Search
    # =====================================================================
    nav_panel(
      title = "Conjunction Search",
      icon  = bsicons::bs_icon("grid-3x3"),
      layout_sidebar(
        sidebar = sidebar(
          width = 280,
          bg    = "#f1f5f9",
          sliderInput("t2_n", "Number of distractors:", 20, 150, 60, step = 10),
          hr(),
          actionButton(
            "t2_generate", "Generate New Display",
            class = "btn-primary w-100",
            icon  = icon("arrows-rotate")
          ),
          hr(),
          div(
            class = "explanation-card",
            tags$strong("Simple vs Conjunction"),
            tags$br(),
            "In a ", tags$em("simple"), " search the target differs by one feature ",
            "(e.g., color alone) and pops out instantly. In a ",
            tags$em("conjunction"), " search the target shares each feature with some ",
            "distractors, forcing you to check items one by one."
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(
              "Simple Search (Color only)",
              class = "bg-primary text-white"
            ),
            card_body(
              p(
                "Find the RED dot among grey dots.",
                class = "conjunction-instructions"
              ),
              with_spinner(plotOutput("t2_simple", height = "420px"))
            )
          ),
          card(
            card_header(
              "Conjunction Search (Color + Shape)",
              class = "bg-primary text-white"
            ),
            card_body(
              p(
                "Find the RED SQUARE among red circles and blue squares.",
                class = "conjunction-instructions"
              ),
              with_spinner(plotOutput("t2_conjunction", height = "420px"))
            )
          )
        )
      )
    ),

    # =====================================================================
    # TAB 3 -- Design Your Encoding
    # =====================================================================
    nav_panel(
      title = "Design Your Encoding",
      icon  = bsicons::bs_icon("palette"),
      layout_sidebar(
        sidebar = sidebar(
          width = 280,
          bg    = "#f1f5f9",
          selectInput(
            "t3_attribute", "Visual channel:",
            choices = c("Color" = "color", "Size" = "size", "Shape" = "shape")
          ),
          selectInput(
            "t3_variable", "Categorical variable to encode:",
            choices = c("class", "drv", "cyl", "fl")
          ),
          hr(),
          div(
            class = "explanation-card",
            tags$strong("Why does channel choice matter?"),
            tags$br(),
            tags$br(),
            "Not every visual attribute encodes categorical data equally well. ",
            tags$strong("Color hue"), " is excellent for nominal categories because ",
            "distinct hues are immediately discriminable. ",
            tags$strong("Shape"), " works for a small number of categories (< 6) but ",
            "becomes confusing with many levels. ",
            tags$strong("Size"), " implies order or magnitude and is poor for unordered ",
            "categories -- viewers assume bigger means more."
          )
        ),
        layout_columns(
          col_widths = c(7, 5),
          card(
            card_header("Encoded Plot", class = "bg-primary text-white"),
            card_body(
              with_spinner(plotOutput("t3_plot", height = "460px"))
            )
          ),
          card(
            card_header("Effectiveness Assessment", class = "bg-primary text-white"),
            card_body(
              uiOutput("t3_assessment")
            )
          )
        )
      )
    )
  ),

  # -- Footer ---------------------------------------------------------------
  div(class = "gonzaga-footer",
      "Gonzaga University | Dr. Vivek H. Patil")
)

# ===========================================================================
# SERVER
# ===========================================================================
server <- function(input, output, session) {

  # =========================================================================
  # TAB 1 -- Spot the Target
  # =========================================================================


  # -- Reactive values for state management ---------------------------------
  t1 <- reactiveValues(
    active      = FALSE,    # Is a trial in progress?
    start_time  = NULL,     # POSIXct when Start was clicked
    target_x    = NA,
    target_y    = NA,
    found       = FALSE,
    last_rt     = NULL,     # Last reaction time in seconds
    history     = data.frame(
      attribute = character(),
      rt        = numeric(),
      stringsAsFactors = FALSE
    ),
    seed        = 1         # Increment to force new random layout
  )

  # -- Generate trial data --------------------------------------------------
  t1_data <- reactive({
    req(input$t1_n, input$t1_attribute)
    # Use seed so data regenerates on each Start press
    t1$seed
    n <- input$t1_n

    set.seed(t1$seed * 137 + n)
    bg <- data.frame(
      x     = runif(n, 0, 10),
      y     = runif(n, 0, 10),
      group = "background",
      stringsAsFactors = FALSE
    )
    tx <- runif(1, 1.5, 8.5)
    ty <- runif(1, 1.5, 8.5)
    target <- data.frame(x = tx, y = ty, group = "target",
                         stringsAsFactors = FALSE)
    t1$target_x <- tx
    t1$target_y <- ty

    rbind(bg, target)
  })

  # -- Start button ---------------------------------------------------------
  observeEvent(input$t1_start, {
    t1$seed       <- t1$seed + 1
    t1$active     <- TRUE
    t1$found      <- FALSE
    t1$last_rt    <- NULL
    t1$start_time <- Sys.time()
  })

  # -- Reset history --------------------------------------------------------
  observeEvent(input$t1_reset, {
    t1$history <- data.frame(
      attribute = character(),
      rt        = numeric(),
      stringsAsFactors = FALSE
    )
    t1$active  <- FALSE
    t1$found   <- FALSE
    t1$last_rt <- NULL
  })

  # -- Click detection ------------------------------------------------------
  observeEvent(input$t1_click, {
    req(t1$active, !t1$found)
    click <- input$t1_click
    req(click$x, click$y)

    dist <- sqrt((click$x - t1$target_x)^2 + (click$y - t1$target_y)^2)
    hit_radius <- 0.7

    if (dist <= hit_radius) {
      elapsed      <- as.numeric(difftime(Sys.time(), t1$start_time, units = "secs"))
      t1$last_rt   <- round(elapsed, 3)
      t1$found     <- TRUE
      t1$active    <- FALSE

      attr_label <- switch(
        input$t1_attribute,
        color       = "Color",
        size        = "Size",
        shape       = "Shape",
        orientation = "Orientation"
      )
      t1$history <- rbind(
        t1$history,
        data.frame(attribute = attr_label, rt = t1$last_rt,
                   stringsAsFactors = FALSE)
      )
    }
  })

  # -- Timer display --------------------------------------------------------
  output$t1_timer_text <- renderText({
    if (!is.null(t1$last_rt) && t1$found) {
      paste0(t1$last_rt, " s")
    } else if (t1$active) {
      "Searching..."
    } else {
      "-- --"
    }
  })

  # -- Status message -------------------------------------------------------
  output$t1_status <- renderUI({
    if (t1$found) {
      tags$span(
        style = paste0("color:", NAVY, "; font-weight:600;"),
        "Target found! Try another attribute."
      )
    } else if (t1$active) {
      tags$span(
        style = paste0("color:", RED, "; font-weight:600;"),
        "Click the odd-one-out point!"
      )
    } else {
      tags$span(
        style = "color:#94a3b8;",
        "Press Start to begin a trial."
      )
    }
  })

  # -- Main stimulus plot ---------------------------------------------------
  output$t1_plot <- renderPlot({
    d <- t1_data()
    req(nrow(d) > 0)
    validate(need(input$t1_attribute, "Select an attribute."))

    tgt <- d[d$group == "target", ]
    bg  <- d[d$group == "background", ]
    attr <- input$t1_attribute

    bg_col    <- "#94a3b8"
    tgt_col   <- if (attr == "color") RED else bg_col
    bg_size   <- 5
    tgt_size  <- if (attr == "size") 10 else bg_size
    bg_shape  <- 16
    tgt_shape <- if (attr == "shape") 17 else bg_shape

    # Orientation: use short line segments
    use_orientation <- (attr == "orientation")

    p <- ggplot()

    if (use_orientation) {
      seg_len <- 0.35
      # Background segments -- horizontal
      bg_segs <- bg %>%
        mutate(
          xend = x + seg_len,
          yend = y
        )
      # Target segment -- 45-degree diagonal
      tgt_segs <- tgt %>%
        mutate(
          xend = x + seg_len * cos(pi / 4),
          yend = y + seg_len * sin(pi / 4)
        )
      p <- p +
        geom_segment(
          data = bg_segs,
          aes(x = x, y = y, xend = xend, yend = yend),
          color    = NAVY,
          linewidth = 2,
          lineend  = "round",
          alpha    = 0.7
        ) +
        geom_segment(
          data = tgt_segs,
          aes(x = x, y = y, xend = xend, yend = yend),
          color    = RED,
          linewidth = 2.5,
          lineend  = "round"
        )
    } else {
      p <- p +
        geom_point(
          data  = bg,
          aes(x = x, y = y),
          color = bg_col,
          size  = bg_size,
          shape = bg_shape,
          alpha = 0.7
        ) +
        geom_point(
          data  = tgt,
          aes(x = x, y = y),
          color = tgt_col,
          size  = tgt_size,
          shape = tgt_shape
        )
    }

    # Show a ring around found target
    if (t1$found) {
      p <- p +
        annotate(
          "point",
          x     = t1$target_x,
          y     = t1$target_y,
          shape = 1,
          size  = 14,
          color = GOLD,
          stroke = 2.5
        )
    }

    attr_label <- switch(
      attr,
      color       = "Color",
      size        = "Size",
      shape       = "Shape",
      orientation = "Orientation"
    )

    p +
      coord_cartesian(xlim = c(-0.5, 10.5), ylim = c(-0.5, 10.5)) +
      labs(
        title    = paste0("Attribute: ", attr_label),
        subtitle = paste0(input$t1_n, " background items")
      ) +
      theme_void(base_size = 14) +
      theme(
        plot.title       = element_text(color = NAVY, face = "bold",
                                        hjust = 0.5, size = 15),
        plot.subtitle    = element_text(color = "#475569", hjust = 0.5,
                                        size = 11, margin = margin(b = 8)),
        panel.background = element_rect(fill = "#f8fafc", color = NA),
        plot.margin      = margin(10, 10, 10, 10)
      )
  }, res = 110)

  # -- Reaction-time history bar chart --------------------------------------
  output$t1_history <- renderPlot({
    hist_df <- t1$history
    validate(need(nrow(hist_df) > 0, "No trials recorded yet. Press Start!"))

    summary_df <- hist_df %>%
      group_by(attribute) %>%
      summarise(
        mean_rt = mean(rt),
        n       = n(),
        se      = if (n() > 1) sd(rt) / sqrt(n()) else 0,
        .groups = "drop"
      ) %>%
      arrange(mean_rt)

    summary_df$attribute <- factor(summary_df$attribute,
                                   levels = summary_df$attribute)

    bar_colors <- c(
      "Color"       = RED,
      "Size"        = GOLD,
      "Shape"       = NAVY,
      "Orientation" = "#5B8FA8"
    )
    fill_vals <- bar_colors[as.character(summary_df$attribute)]

    ggplot(summary_df, aes(x = attribute, y = mean_rt, fill = attribute)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_errorbar(
        aes(ymin = pmax(0, mean_rt - se), ymax = mean_rt + se),
        width = 0.18, linewidth = 0.6, color = "#334155"
      ) +
      geom_text(
        aes(label = paste0(round(mean_rt, 2), "s\n(n=", n, ")")),
        vjust  = -0.4,
        size   = 3.8,
        color  = "#1e293b",
        fontface = "bold"
      ) +
      scale_fill_manual(
        values = bar_colors,
        drop   = FALSE
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
      labs(
        title = "Mean Reaction Time",
        x     = NULL,
        y     = "Seconds"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(color = NAVY, face = "bold",
                                        hjust = 0.5, size = 14),
        axis.text.x      = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank()
      )
  }, res = 110)

  # =========================================================================
  # TAB 2 -- Conjunction Search
  # =========================================================================

  t2_seed <- reactiveVal(42)

  observeEvent(input$t2_generate, {
    t2_seed(t2_seed() + 1)
  })

  # -- Simple search data ---------------------------------------------------
  t2_simple_data <- reactive({
    req(input$t2_n)
    seed <- t2_seed()
    n    <- input$t2_n

    set.seed(seed * 31)
    bg <- data.frame(
      x     = runif(n, 0, 10),
      y     = runif(n, 0, 10),
      type  = "distractor",
      stringsAsFactors = FALSE
    )
    tgt <- data.frame(
      x    = runif(1, 1, 9),
      y    = runif(1, 1, 9),
      type = "target",
      stringsAsFactors = FALSE
    )
    rbind(bg, tgt)
  })

  # -- Conjunction search data ----------------------------------------------
  t2_conj_data <- reactive({
    req(input$t2_n)
    seed <- t2_seed()
    n    <- input$t2_n

    set.seed(seed * 53)
    n_half <- floor(n / 2)

    # Distractor type 1: red circles
    red_circles <- data.frame(
      x      = runif(n_half, 0, 10),
      y      = runif(n_half, 0, 10),
      colour = "red_circle",
      stringsAsFactors = FALSE
    )
    # Distractor type 2: blue squares
    blue_squares <- data.frame(
      x      = runif(n - n_half, 0, 10),
      y      = runif(n - n_half, 0, 10),
      colour = "blue_square",
      stringsAsFactors = FALSE
    )
    # Target: red square
    target <- data.frame(
      x      = runif(1, 1, 9),
      y      = runif(1, 1, 9),
      colour = "target_red_square",
      stringsAsFactors = FALSE
    )
    rbind(red_circles, blue_squares, target)
  })

  # -- Simple search plot ---------------------------------------------------
  output$t2_simple <- renderPlot({
    d <- t2_simple_data()
    req(nrow(d) > 0)

    tgt <- d[d$type == "target", ]
    bg  <- d[d$type == "distractor", ]

    ggplot() +
      geom_point(
        data  = bg,
        aes(x = x, y = y),
        color = "#94a3b8",
        size  = 5,
        shape = 16,
        alpha = 0.7
      ) +
      geom_point(
        data  = tgt,
        aes(x = x, y = y),
        color = RED,
        size  = 5,
        shape = 16
      ) +
      coord_cartesian(xlim = c(-0.5, 10.5), ylim = c(-0.5, 10.5)) +
      labs(subtitle = paste0(input$t2_n, " grey distractors + 1 red target")) +
      theme_void(base_size = 13) +
      theme(
        plot.subtitle    = element_text(color = "#475569", hjust = 0.5,
                                        size = 11, margin = margin(b = 6)),
        panel.background = element_rect(fill = "#f8fafc", color = NA),
        plot.margin      = margin(8, 8, 8, 8)
      )
  }, res = 110)

  # -- Conjunction search plot -----------------------------------------------
  output$t2_conjunction <- renderPlot({
    d <- t2_conj_data()
    req(nrow(d) > 0)

    rc <- d[d$colour == "red_circle", ]
    bs <- d[d$colour == "blue_square", ]
    tgt <- d[d$colour == "target_red_square", ]

    ggplot() +
      # Red circles (distractor group 1)
      geom_point(
        data  = rc,
        aes(x = x, y = y),
        color = RED,
        fill  = RED,
        size  = 5,
        shape = 21,
        alpha = 0.75
      ) +
      # Blue squares (distractor group 2)
      geom_point(
        data  = bs,
        aes(x = x, y = y),
        color = NAVY,
        fill  = NAVY,
        size  = 5,
        shape = 22,
        alpha = 0.75
      ) +
      # Target: red square
      geom_point(
        data  = tgt,
        aes(x = x, y = y),
        color = RED,
        fill  = RED,
        size  = 5.5,
        shape = 22
      ) +
      coord_cartesian(xlim = c(-0.5, 10.5), ylim = c(-0.5, 10.5)) +
      labs(
        subtitle = paste0(
          input$t2_n, " distractors (",
          floor(input$t2_n / 2), " red circles + ",
          input$t2_n - floor(input$t2_n / 2), " blue squares) + 1 red square target"
        )
      ) +
      theme_void(base_size = 13) +
      theme(
        plot.subtitle    = element_text(color = "#475569", hjust = 0.5,
                                        size = 10, margin = margin(b = 6)),
        panel.background = element_rect(fill = "#f8fafc", color = NA),
        plot.margin      = margin(8, 8, 8, 8)
      )
  }, res = 110)

  # =========================================================================
  # TAB 3 -- Design Your Encoding
  # =========================================================================

  t3_plot_data <- reactive({
    req(input$t3_variable)
    mpg_data <- ggplot2::mpg
    # Ensure chosen variable is a factor for consistent encoding
    mpg_data$encode_var <- as.factor(mpg_data[[input$t3_variable]])
    mpg_data
  })

  output$t3_plot <- renderPlot({
    d <- t3_plot_data()
    req(nrow(d) > 0)
    validate(need(input$t3_attribute, "Select a visual channel."))

    attr   <- input$t3_attribute
    varname <- input$t3_variable
    n_levels <- length(levels(d$encode_var))

    base_p <- ggplot(d, aes(x = displ, y = hwy))

    gonzaga_palette <- colorRampPalette(c(NAVY, GOLD, RED))(n_levels)

    p <- switch(
      attr,
      color = {
        base_p +
          geom_point(aes(color = encode_var), size = 3.5, alpha = 0.8) +
          scale_color_manual(
            values = gonzaga_palette,
            name   = varname
          )
      },
      size = {
        size_range <- c(2, 9)
        base_p +
          geom_point(
            aes(size = encode_var),
            color = NAVY,
            alpha = 0.6
          ) +
          scale_size_manual(
            values = seq(size_range[1], size_range[2],
                         length.out = n_levels),
            name   = varname
          )
      },
      shape = {
        # ggplot2 supports up to 6 distinct shapes easily; warn if more
        available_shapes <- c(16, 17, 15, 18, 8, 3, 4, 0, 1, 2)
        shape_vals <- available_shapes[seq_len(min(n_levels, length(available_shapes)))]
        base_p +
          geom_point(
            aes(shape = encode_var),
            color = NAVY,
            size  = 3.5,
            alpha = 0.8
          ) +
          scale_shape_manual(
            values = shape_vals,
            name   = varname
          )
      }
    )

    attr_label <- switch(attr,
                         color = "Color",
                         size  = "Size",
                         shape = "Shape")

    p +
      labs(
        title    = paste0("Encoding '", varname, "' with ", attr_label),
        subtitle = paste0(n_levels, " categories mapped to ", tolower(attr_label)),
        x        = "Engine Displacement (L)",
        y        = "Highway MPG"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(color = NAVY, face = "bold", size = 15),
        plot.subtitle = element_text(color = "#64748b", size = 11,
                                     margin = margin(b = 10)),
        legend.position = "right"
      )
  }, res = 110)

  # -- Effectiveness assessment panel ---------------------------------------
  output$t3_assessment <- renderUI({
    req(input$t3_attribute, input$t3_variable)

    attr     <- input$t3_attribute
    varname  <- input$t3_variable
    n_levels <- length(unique(ggplot2::mpg[[varname]]))

    # Build assessment text
    rating <- switch(
      attr,
      color = {
        if (n_levels <= 8) {
          list(
            grade = "Excellent",
            color = "#16a34a",
            text  = paste0(
              "Color hue is one of the most effective channels for categorical data. ",
              "With ", n_levels, " categories, each group is immediately distinguishable. ",
              "Color differences are detected preattentively -- your visual system ",
              "separates the groups without conscious effort. This is why color is the ",
              "default categorical encoding in most visualization tools."
            )
          )
        } else {
          list(
            grade = "Good (many categories)",
            color = "#ca8a04",
            text  = paste0(
              "Color hue is normally excellent for categories, but with ",
              n_levels, " levels the palette becomes crowded. Consider grouping ",
              "or filtering to reduce the number of categories."
            )
          )
        }
      },
      size = {
        list(
          grade = "Poor for categories",
          color = RED,
          text  = paste0(
            "Size implies magnitude and rank -- bigger means more. Mapping an ",
            "unordered categorical variable like '", varname, "' to size misleads ",
            "viewers into thinking some categories are more important. Size is best ",
            "reserved for quantitative variables. With ", n_levels, " categories the ",
            "size differences are also hard to discriminate reliably."
          )
        )
      },
      shape = {
        if (n_levels <= 5) {
          list(
            grade = "Adequate",
            color = "#ca8a04",
            text  = paste0(
              "Shape can distinguish a small number of categories (< 6). With ",
              n_levels, " levels, this is workable but requires more attentive effort ",
              "than color. Shape differences are preattentive only when a single shape ",
              "differs from a uniform field. With multiple shapes, detection slows to ",
              "serial search."
            )
          )
        } else {
          list(
            grade = "Poor (too many levels)",
            color = RED,
            text  = paste0(
              "Shape is ineffective with ", n_levels, " categories. Humans struggle to ",
              "distinguish more than about 5-6 shapes, especially at small sizes. Many ",
              "points overlap, making individual shapes hard to identify. Consider using ",
              "color instead or reducing the number of categories."
            )
          )
        }
      }
    )

    tagList(
      div(
        style = "text-align:center; padding: 18px 0 10px 0;",
        tags$span(
          style = paste0(
            "font-size: 1.6rem; font-weight: 800; color:", rating$color, ";"
          ),
          rating$grade
        )
      ),
      hr(),
      div(
        style = "padding: 6px 8px;",
        h5("Assessment", style = paste0("color:", NAVY, "; font-weight:700;")),
        p(rating$text, style = "line-height:1.7; color:#334155; font-size:0.92rem;"),
        hr(),
        h5("Quick Reference", style = paste0("color:", NAVY, "; font-weight:700;")),
        tags$table(
          style = "width:100%; font-size:0.88rem; border-collapse:collapse;",
          tags$thead(
            tags$tr(
              style = "border-bottom: 2px solid #e2e8f0;",
              tags$th("Channel", style = "padding:6px 4px; text-align:left;"),
              tags$th("Best For", style = "padding:6px 4px; text-align:left;"),
              tags$th("Max Levels", style = "padding:6px 4px; text-align:center;")
            )
          ),
          tags$tbody(
            tags$tr(
              style = "border-bottom:1px solid #f1f5f9;",
              tags$td("Color hue", style = "padding:5px 4px;"),
              tags$td("Nominal categories", style = "padding:5px 4px;"),
              tags$td("~8-10", style = "padding:5px 4px; text-align:center;")
            ),
            tags$tr(
              style = "border-bottom:1px solid #f1f5f9;",
              tags$td("Shape", style = "padding:5px 4px;"),
              tags$td("Nominal (small n)", style = "padding:5px 4px;"),
              tags$td("~5-6", style = "padding:5px 4px; text-align:center;")
            ),
            tags$tr(
              tags$td("Size", style = "padding:5px 4px;"),
              tags$td("Quantitative / ordinal", style = "padding:5px 4px;"),
              tags$td("~4-5", style = "padding:5px 4px; text-align:center;")
            )
          )
        )
      )
    )
  })
}

# ===========================================================================
# Run
# ===========================================================================
shinyApp(ui, server)
