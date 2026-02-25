###############################################################################
#  Design Detective — Week 02 Game
#  "Spot the deception before it fools your audience."
#
#  Visual Analytics & Data Storytelling with R  |  Gonzaga University
#  8 rounds of deliberately flawed charts; identify the flaw then rate severity
###############################################################################

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(shinyjs)

has_spinners <- requireNamespace("shinycssloaders", quietly = TRUE)
if (has_spinners) library(shinycssloaders)

wrap_spinner <- function(ui_element, color = "#002967") {
  if (has_spinners) {
    shinycssloaders::withSpinner(ui_element, color = color, type = 6)
  } else {
    ui_element
  }
}

# ---------------------------------------------------------------------------
# Gonzaga branding
# ---------------------------------------------------------------------------
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

TOTAL_ROUNDS <- 8
MAX_SCORE    <- TOTAL_ROUNDS * 15   # 10 per flaw-ID + 5 bonus = 120

# ---------------------------------------------------------------------------
# Round definitions
# ---------------------------------------------------------------------------
rounds <- list(

  # ---- Round 1: Truncated Y-axis -------------------------------------------
  list(
    title   = "Quarterly Revenue ($M)",
    flaw    = "Truncated Y-axis exaggerates differences",
    expert  = 4,
    explain = "Starting the y-axis at 95 instead of 0 makes a ~4% difference look enormous. Always check where the axis begins.",
    expert_why = "A truncated axis is one of the most common tricks in business reporting — it dramatically inflates perceived change.",
    choices = c(
      "Truncated Y-axis exaggerates differences",
      "Wrong chart type for the data",
      "Missing axis labels",
      "Colours are not accessible"
    ),
    plot_fn = function() {
      df <- data.frame(
        quarter = factor(c("Q1", "Q2", "Q3", "Q4"), levels = c("Q1", "Q2", "Q3", "Q4")),
        revenue = c(98, 101, 99, 103)
      )
      ggplot(df, aes(quarter, revenue)) +
        geom_col(fill = NAVY, width = 0.6) +
        coord_cartesian(ylim = c(95, 105)) +
        labs(title = "Quarterly Revenue ($M)", x = "Quarter", y = "Revenue") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = NAVY),
          panel.grid.minor = element_blank()
        )
    }
  ),

  # ---- Round 2: Dual Y-axes ------------------------------------------------
  list(
    title   = "Ice Cream Sales vs. Shark Attacks",
    flaw    = "Dual Y-axes create false correlation",
    expert  = 5,
    explain = "Plotting two unrelated series on different scales tricks viewers into seeing a causal link. The visual overlap is an artefact of scale choice.",
    expert_why = "Dual-axis charts are rated maximum severity because they can fabricate correlations out of thin air and are frequently used to mislead.",
    choices = c(
      "Dual Y-axes create false correlation",
      "Line chart should be a bar chart",
      "Data points are too sparse",
      "Colour palette is not colourblind-safe"
    ),
    plot_fn = function() {
      df <- data.frame(
        month      = factor(month.abb, levels = month.abb),
        icecream   = c(20, 22, 35, 55, 80, 110, 130, 125, 90, 50, 30, 22),
        sharks     = c(1, 1, 2, 3, 5, 8, 10, 9, 6, 3, 1, 1)
      )
      scale_factor <- max(df$icecream) / max(df$sharks)
      ggplot(df, aes(x = month, group = 1)) +
        geom_line(aes(y = icecream), colour = NAVY, linewidth = 1.2) +
        geom_line(aes(y = sharks * scale_factor), colour = RED, linewidth = 1.2) +
        scale_y_continuous(
          name = "Ice Cream Sales ($k)",
          sec.axis = sec_axis(~ . / scale_factor, name = "Shark Attacks")
        ) +
        labs(title = "Ice Cream Sales vs. Shark Attacks", x = NULL) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title       = element_text(face = "bold", color = NAVY),
          axis.title.y.left  = element_text(colour = NAVY),
          axis.title.y.right = element_text(colour = RED),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank()
        )
    }
  ),

  # ---- Round 3: Pie chart with confusing colours / bad labels ---------------
  list(
    title   = "Market Share by Segment",
    flaw    = "Pie chart distorts area perception and percentages don't add up",
    expert  = 4,
    explain = "Pie slices encode data as angles -- one of the least accurate perceptual channels (Cleveland & McGill). Here the similar blue shades make slices nearly indistinguishable, and the percentages sum to 105%, meaning the chart is mathematically impossible. A bar chart would show these values far more accurately.",
    expert_why = "Pie charts are among the most commonly misused chart types. Area/angle distortion silently biases decisions, especially when combined with misleading labels.",
    choices = c(
      "Pie chart distorts area perception and percentages don't add up",
      "Too many categories for a single chart",
      "Chart needs a descriptive subtitle",
      "Data should be shown as a time series"
    ),
    plot_fn = function() {
      df <- data.frame(
        segment = c("Enterprise", "Mid-Market", "SMB", "Startup", "Government"),
        share   = c(32, 28, 22, 13, 10)  # sums to 105 on purpose
      )
      df$label <- paste0(df$segment, "\n", df$share, "%")
      # Similar blue-ish colours to make discrimination hard
      cols <- c("#003366", "#0055AA", "#2266BB", "#3377CC", "#4488DD")
      ggplot(df, aes(x = "", y = share, fill = segment)) +
        geom_col(width = 1, colour = "white", linewidth = 0.5) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols, name = "Segment") +
        geom_text(aes(label = label),
                  position = position_stack(vjust = 0.5),
                  colour = "white", size = 3.4, fontface = "bold") +
        labs(title = "Market Share by Segment", subtitle = "(percentages shown)") +
        theme_void(base_size = 14) +
        theme(
          plot.title    = element_text(face = "bold", color = NAVY, hjust = 0.5),
          plot.subtitle = element_text(color = "#64748b", hjust = 0.5, size = 11),
          legend.position = "right"
        )
    }
  ),

  # ---- Round 4: Cherry-picked timeframe ------------------------------------
  list(
    title   = "Stock Price — Strong Growth!",
    flaw    = "Cherry-picked time range hides the full trend",
    expert  = 5,
    explain = "The chart only shows months 8-14 of a 24-month series — the upswing after a major crash. The full data tells a very different story.",
    expert_why = "Selectively framing a time window is maximum severity because it fundamentally misrepresents the underlying reality.",
    choices = c(
      "Cherry-picked time range hides the full trend",
      "Line chart needs confidence bands",
      "X-axis labels are rotated incorrectly",
      "Grid lines are too prominent"
    ),
    plot_fn = function() {
      set.seed(42)
      full_months <- 1:24
      # Simulate: rise, crash, partial recovery
      full_price  <- c(
        50, 55, 60, 72, 78, 65, 40, 30,   # crash
        32, 38, 45, 52, 58, 63, 60, 55,   # recovery then plateau
        50, 48, 46, 44, 42, 40, 39, 38    # slow decline
      )
      df_cherry <- data.frame(month = 8:14, price = full_price[8:14])
      ggplot(df_cherry, aes(month, price)) +
        geom_line(colour = NAVY, linewidth = 1.3) +
        geom_point(colour = NAVY, size = 2.5) +
        scale_x_continuous(breaks = 8:14, labels = paste0("M", 8:14)) +
        labs(title = "Stock Price \u2014 Strong Growth!", x = "Month", y = "Price ($)") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = NAVY),
          panel.grid.minor = element_blank()
        )
    }
  ),

  # ---- Round 5: Rainbow colour map -----------------------------------------
  list(
    title   = "Regional Temperature Map",
    flaw    = "Rainbow palette misrepresents sequential data",
    expert  = 3,
    explain = "Rainbow colourmaps create artificial visual boundaries and don't have a perceptually uniform progression — viewers can't reliably judge magnitude differences.",
    expert_why = "Moderate severity: it impairs interpretation but usually doesn't create an outright false narrative.",
    choices = c(
      "Rainbow palette misrepresents sequential data",
      "Heatmap tiles are too small to read",
      "Title is not descriptive enough",
      "Data should be normalised before plotting"
    ),
    plot_fn = function() {
      df <- expand.grid(lon = seq(-120, -70, by = 5), lat = seq(25, 50, by = 5))
      set.seed(7)
      df$temp <- 40 + (df$lat - 25) * 0.8 + rnorm(nrow(df), 0, 3)
      ggplot(df, aes(lon, lat, fill = temp)) +
        geom_tile(width = 5, height = 5) +
        scale_fill_gradientn(colours = rainbow(7), name = "Temp (\u00B0F)") +
        labs(title = "Regional Temperature Map", x = "Longitude", y = "Latitude") +
        coord_fixed(ratio = 1.2) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = NAVY),
          panel.grid = element_blank()
        )
    }
  ),

  # ---- Round 6: Broken aspect ratio ----------------------------------------
  list(
    title   = "Monthly Active Users",
    flaw    = "Distorted aspect ratio misleads about rate of change",
    expert  = 3,
    explain = "Stretching the chart extremely wide and flat squashes the slope, making rapid growth look gentle. The underlying numbers are identical — only the shape lies.",
    expert_why = "Moderate severity: knowledgeable readers may notice, but casual viewers will be misled about the trend slope.",
    choices = c(
      "Distorted aspect ratio misleads about rate of change",
      "Line should have data markers",
      "Y-axis needs a currency symbol",
      "Missing legend for the line colour"
    ),
    plot_fn = function() {
      df <- data.frame(
        month = 1:12,
        users = c(100, 150, 280, 420, 600, 850, 1100, 1500, 2000, 2600, 3400, 4500)
      )
      ggplot(df, aes(month, users)) +
        geom_line(colour = NAVY, linewidth = 1.3) +
        scale_x_continuous(breaks = 1:12, labels = month.abb) +
        labs(title = "Monthly Active Users (thousands)", x = NULL, y = "Users (k)") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = NAVY),
          aspect.ratio = 0.1,
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
  ),

  # ---- Round 7: Misleading legend ------------------------------------------
  list(
    title   = "Sales by Region",
    flaw    = "Misleading legend creates wrong associations",
    expert  = 4,
    explain = "The fill colours are manually swapped so the legend says 'North' is blue but the North bar is actually red, and vice versa. Viewers trust the legend and draw wrong conclusions.",
    expert_why = "High severity: the viewer will confidently state the wrong region is leading, which can drive bad decisions.",
    choices = c(
      "Misleading legend creates wrong associations",
      "Bars should be sorted by value",
      "Chart needs percentage labels",
      "Too few data points to be meaningful"
    ),
    plot_fn = function() {
      df <- data.frame(
        region = factor(c("North", "South", "East", "West"),
                        levels = c("North", "South", "East", "West")),
        sales  = c(120, 95, 80, 110)
      )
      # Deliberately swapped colours: North gets RED, South gets NAVY, etc.
      wrong_cols <- c("North" = RED, "South" = NAVY, "East" = GOLD, "West" = "#5A9BD5")
      ggplot(df, aes(region, sales, fill = region)) +
        geom_col(width = 0.65) +
        scale_fill_manual(
          values = wrong_cols,
          # Force legend order so labels mismatch visual position perception
          breaks = c("South", "North", "West", "East"),
          name   = "Region"
        ) +
        labs(title = "Sales by Region ($M)", x = NULL, y = "Sales") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = NAVY),
          panel.grid.minor = element_blank()
        )
    }
  ),

  # ---- Round 8: Non-zero baseline stacked area -----------------------------
  list(
    title   = "Product Revenue Share Over Time",
    flaw    = "Non-zero baseline inflates apparent proportions",
    expert  = 4,
    explain = "The stacked area starts at y = 500 instead of 0, making the top product appear to dominate when in reality the proportions are more balanced.",
    expert_why = "High severity: baseline manipulation in stacked charts is subtle and systematically biases the area comparison.",
    choices = c(
      "Non-zero baseline inflates apparent proportions",
      "Area chart should be a line chart instead",
      "Too many overlapping colours",
      "Time intervals are uneven"
    ),
    plot_fn = function() {
      df <- data.frame(
        year    = rep(2018:2023, each = 3),
        product = rep(c("Alpha", "Beta", "Gamma"), times = 6),
        revenue = c(
          200, 180, 150,
          220, 190, 160,
          210, 200, 170,
          250, 210, 180,
          260, 220, 190,
          280, 230, 200
        )
      )
      df$product <- factor(df$product, levels = c("Gamma", "Beta", "Alpha"))
      ggplot(df, aes(year, revenue, fill = product)) +
        geom_area(alpha = 0.85, colour = "white", linewidth = 0.3) +
        coord_cartesian(ylim = c(500, 720)) +
        scale_fill_manual(values = c("Alpha" = NAVY, "Beta" = GOLD, "Gamma" = RED)) +
        labs(title = "Product Revenue Share Over Time ($M)", x = "Year", y = "Revenue") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = NAVY),
          panel.grid.minor = element_blank()
        )
    }
  )
)

# ---------------------------------------------------------------------------
# CSS
# ---------------------------------------------------------------------------
app_css <- tags$style(HTML(sprintf("
  /* ---- general ---- */
  body { background-color: #f8f9fa; }

  .sidebar {
    border-right: 3px solid %s;
  }

  /* ---- progress bar ---- */
  .progress {
    height: 22px;
    border-radius: 11px;
    background-color: #e2e8f0;
  }
  .progress-bar {
    background-color: %s;
    border-radius: 11px;
    transition: width 0.5s ease;
    font-weight: 600;
  }

  /* ---- score badge ---- */
  .score-badge {
    font-size: 1.35rem;
    font-weight: 700;
    color: %s;
    text-align: center;
    padding: 8px 0;
  }

  /* ---- feedback cards ---- */
  .feedback-correct {
    background-color: #d1fae5;
    border-left: 5px solid #059669;
    padding: 14px 18px;
    border-radius: 6px;
    margin-top: 10px;
  }
  .feedback-wrong {
    background-color: #fee2e2;
    border-left: 5px solid %s;
    padding: 14px 18px;
    border-radius: 6px;
    margin-top: 10px;
  }
  .feedback-severity {
    background-color: #fef9c3;
    border-left: 5px solid %s;
    padding: 14px 18px;
    border-radius: 6px;
    margin-top: 10px;
  }

  /* ---- buttons ---- */
  .btn-game {
    background-color: %s;
    color: #ffffff;
    border: none;
    font-weight: 600;
    padding: 10px 28px;
    border-radius: 6px;
    font-size: 1rem;
  }
  .btn-game:hover {
    background-color: #001a45;
    color: #ffffff;
  }
  .btn-next {
    background-color: %s;
    color: #ffffff;
    border: none;
    font-weight: 600;
    padding: 10px 28px;
    border-radius: 6px;
    font-size: 1rem;
  }
  .btn-next:hover {
    background-color: #8a7042;
    color: #ffffff;
  }

  /* ---- round header ---- */
  .round-header {
    font-size: 1.1rem;
    font-weight: 600;
    color: %s;
    margin-bottom: 6px;
  }
  .chart-title-label {
    font-size: 0.92rem;
    color: #475569;
    margin-bottom: 10px;
  }

  /* ---- completion card ---- */
  .completion-card {
    max-width: 700px;
    margin: 0 auto;
  }
  .completion-card .card-header {
    background-color: %s;
    color: #ffffff;
    font-size: 1.3rem;
    font-weight: 700;
    text-align: center;
  }
  .hash-code {
    font-family: 'Fira Code', 'Courier New', monospace;
    background-color: #f1f5f9;
    padding: 4px 10px;
    border-radius: 4px;
    font-size: 0.95rem;
  }
  .reflection-box {
    background-color: #eff6ff;
    border-left: 5px solid %s;
    padding: 16px 20px;
    border-radius: 6px;
    margin-top: 14px;
    font-style: italic;
    color: #1e3a5f;
    line-height: 1.55;
  }

  /* ---- report table ---- */
  .report-table {
    width: 100%%;
    border-collapse: collapse;
    font-size: 0.88rem;
    margin-top: 10px;
  }
  .report-table th {
    background-color: %s;
    color: #ffffff;
    padding: 8px 10px;
    text-align: left;
  }
  .report-table td {
    padding: 7px 10px;
    border-bottom: 1px solid #e2e8f0;
  }
  .report-table tr:nth-child(even) {
    background-color: #f8fafc;
  }

  /* ---- footer ---- */
  .app-footer {
    text-align: center;
    padding: 12px 0;
    font-size: 0.78rem;
    color: #64748b;
    border-top: 1px solid #e2e8f0;
    margin-top: 18px;
  }

  /* slider label fix */
  .irs--shiny .irs-bar { background: %s; border-top: 1px solid %s; border-bottom: 1px solid %s; }
  .irs--shiny .irs-single { background: %s; }
  .irs--shiny .irs-from, .irs--shiny .irs-to { background: %s; }

  /* hide elements utility */
  .hidden-ui { display: none; }

  /* tagline */
  .tagline {
    font-style: italic;
    color: #64748b;
    font-size: 0.88rem;
    margin-bottom: 4px;
  }
",
  NAVY, NAVY, NAVY, RED, GOLD, NAVY, GOLD, NAVY, NAVY, NAVY, NAVY,
  NAVY, NAVY, NAVY, NAVY, NAVY
)))


# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_sidebar(
  title    = NULL,
  fillable = FALSE,
  theme    = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),

  # Head extras
  tags$head(
    app_css,
    tags$script(HTML("
      function copyGameReport() {
        var base = document.getElementById('report_text_store');
        var ref = document.getElementById('reflection_input');
        if (!base) return;
        var refText = ref ? ref.value.trim() : '';
        var full = base.value + '\\nReflection: ' + (refText || '(No reflection provided)') + '\\n\\nGonzaga University | Dr. Vivek H. Patil';
        navigator.clipboard.writeText(full).then(function(){
          var el = document.getElementById('copy_confirm');
          if (el) { el.style.display = 'inline'; setTimeout(function(){ el.style.display = 'none'; }, 3000); }
        }).catch(function(){
          var ta = document.createElement('textarea');
          ta.value = full; ta.style.position = 'fixed'; ta.style.left = '-9999px';
          document.body.appendChild(ta); ta.select(); document.execCommand('copy'); document.body.removeChild(ta);
          var el = document.getElementById('copy_confirm');
          if (el) { el.style.display = 'inline'; setTimeout(function(){ el.style.display = 'none'; }, 3000); }
        });
      }
    "))
  ),

  useShinyjs(),

  # ---- Sidebar -----------------------------------------------------------
  sidebar = sidebar(
    width = 280,
    bg    = "#f1f5f9",

    tags$div(
      style = "text-align:center; margin-bottom:6px;",
      tags$h5("Design Detective", style = paste0("color:", NAVY, "; font-weight:700; margin-bottom:2px;")),
      tags$div(class = "tagline", "Spot the deception before it fools your audience.")
    ),

    accordion(
      id   = "help_accordion",
      open = FALSE,
      accordion_panel(
        title = "How to Play",
        icon  = bsicons::bs_icon("info-circle"),
        tags$ul(
          style = "font-size:0.82rem; padding-left:1.1rem; margin-bottom:0;",
          tags$li("Enter your name and press ", tags$strong("Start Game"), "."),
          tags$li(tags$strong("Phase 1:"), " View a deliberately flawed chart and pick the design flaw from 4 options."),
          tags$li(tags$strong("Phase 2:"), " Rate how severe you think the flaw is (1\u20135) and compare to the expert rating."),
          tags$li("Play all 8 rounds. Maximum score: 120 points."),
          tags$li("At the end, copy your completion report to submit."),
          tags$li("After all rounds, write a brief ", tags$strong("reflection"), " before copying your report.")
        )
      )
    ),

    hr(style = "margin:8px 0;"),

    textInput("player_name", "Your Name:", placeholder = "e.g. Alex Gonzaga"),

    actionButton("start_btn", "Start Game",
                 icon  = icon("play"),
                 class = "btn-game w-100",
                 style = "margin-bottom:12px;"),

    hr(style = "margin:8px 0;"),

    tags$label("Progress", style = "font-weight:600; font-size:0.85rem;"),
    uiOutput("progress_bar_ui"),

    tags$div(class = "score-badge", textOutput("score_display"))
  ),

  # ---- Main panel --------------------------------------------------------
  # Game area (hidden until start)
  div(
    id = "game_area",
    class = "hidden-ui",

    # Round header
    uiOutput("round_header_ui"),

    # Chart display
    card(
      full_screen = TRUE,
      card_body(
        wrap_spinner(plotOutput("chart_display", height = "400px"))
      )
    ),

    # Phase 1: question
    div(
      id = "phase1_area",
      card(
        card_body(
          tags$h6("What is the PRIMARY design flaw in this chart?",
                  style = paste0("color:", NAVY, "; font-weight:700; margin-bottom:10px;")),
          uiOutput("choices_ui"),
          actionButton("submit_flaw", "Submit Answer",
                       icon  = icon("check"),
                       class = "btn-game",
                       style = "margin-top:10px;")
        )
      )
    ),

    # Phase 1 feedback
    uiOutput("phase1_feedback_ui"),

    # Phase 2: severity rating (hidden until phase 1 answered)
    div(
      id = "phase2_area",
      class = "hidden-ui",
      card(
        card_body(
          tags$h6("How severe is this flaw? (1 = minor, 5 = critical)",
                  style = paste0("color:", NAVY, "; font-weight:700; margin-bottom:10px;")),
          sliderInput("severity_rating", NULL,
                      min = 1, max = 5, value = 3, step = 1,
                      ticks = TRUE, width = "100%"),
          actionButton("submit_severity", "Submit Rating",
                       icon  = icon("star"),
                       class = "btn-game",
                       style = "margin-top:6px;")
        )
      )
    ),

    # Phase 2 feedback
    uiOutput("phase2_feedback_ui"),

    # Next round button (hidden until both phases done)
    div(
      id = "next_area",
      class = "hidden-ui",
      style = "text-align:center; margin:16px 0;",
      actionButton("next_round", "Next Round \u2192",
                   class = "btn-next",
                   style = "font-size:1.1rem; padding:12px 36px;")
    )
  ),

  # ---- Welcome panel (shown before start) --------------------------------
  div(
    id = "welcome_area",
    card(
      card_header("Welcome to Design Detective",
                  class = "bg-primary text-white"),
      card_body(
        tags$p(style = "font-size:1.05rem;",
          "Misleading charts are everywhere \u2014 in news, in boardrooms, on social media. ",
          "Your mission: examine 8 deliberately flawed visualisations, identify each design flaw, ",
          "and rate its severity."
        ),
        tags$p(
          tags$strong("Scoring:"), " 10 points for correctly identifying the flaw. ",
          "Up to 5 bonus points for matching the expert severity rating (5 = exact match, 2 = within 1 point)."
        ),
        tags$p(
          tags$strong("Enter your name"), " in the sidebar and press ",
          tags$strong("Start Game"), " to begin!"
        )
      )
    )
  ),

  # ---- Completion panel (hidden until game over) -------------------------
  div(
    id = "completion_area",
    class = "hidden-ui",
    uiOutput("completion_report_ui")
  ),

  # ---- Footer ------------------------------------------------------------
  tags$footer(
    class = "app-footer",
    "Gonzaga University | Dr. Vivek H. Patil"
  )
)


# ---------------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive state
  rv <- reactiveValues(
    round       = 0L,
    phase       = 0L,      # 0 = not started, 1 = answering flaw, 2 = rating severity, 3 = reviewed
    score       = 0L,
    results     = list(),  # per-round results
    started     = FALSE,
    finished    = FALSE,
    phase1_pts  = 0L,
    phase2_pts  = 0L,
    timestamp   = NULL
  )

  # ---- Start game --------------------------------------------------------
  observeEvent(input$start_btn, {
    req(nchar(trimws(input$player_name)) > 0)

    rv$round    <- 1L
    rv$phase    <- 1L
    rv$score    <- 0L
    rv$results  <- list()
    rv$started  <- TRUE
    rv$finished <- FALSE

    shinyjs::hide("welcome_area")
    shinyjs::hide("completion_area")
    shinyjs::show("game_area")
    shinyjs::show("phase1_area")
    shinyjs::hide("phase2_area")
    shinyjs::hide("next_area")

    # Reset severity slider
    updateSliderInput(session, "severity_rating", value = 3)
  })

  # ---- Progress bar ------------------------------------------------------
  output$progress_bar_ui <- renderUI({
    pct <- if (rv$round > 0) round((rv$round - 1) / TOTAL_ROUNDS * 100) else 0
    if (rv$finished) pct <- 100
    tags$div(
      class = "progress",
      tags$div(
        class = "progress-bar",
        role = "progressbar",
        style = paste0("width:", pct, "%;"),
        `aria-valuenow` = pct, `aria-valuemin` = "0", `aria-valuemax` = "100",
        paste0(pct, "%")
      )
    )
  })

  output$score_display <- renderText({
    paste0("Score: ", rv$score, " / ", MAX_SCORE)
  })

  # ---- Round header ------------------------------------------------------
  output$round_header_ui <- renderUI({
    req(rv$round > 0, rv$round <= TOTAL_ROUNDS)
    rd <- rounds[[rv$round]]
    tags$div(
      tags$div(class = "round-header", paste0("Round ", rv$round, " of ", TOTAL_ROUNDS)),
      tags$div(class = "chart-title-label", rd$title)
    )
  })

  # ---- Chart display -----------------------------------------------------
  output$chart_display <- renderPlot({
    req(rv$round > 0, rv$round <= TOTAL_ROUNDS)
    rounds[[rv$round]]$plot_fn()
  }, res = 110)

  # ---- Multiple choice options -------------------------------------------
  output$choices_ui <- renderUI({
    req(rv$round > 0, rv$round <= TOTAL_ROUNDS)
    rd <- rounds[[rv$round]]
    # Shuffle choices with a seed based on round so it's consistent within a round
    set.seed(rv$round * 17 + 3)
    idx <- sample(seq_along(rd$choices))
    shuffled <- rd$choices[idx]
    radioButtons("flaw_choice", NULL,
                 choices  = shuffled,
                 selected = character(0))
  })

  # ---- Phase 1: submit flaw answer ---------------------------------------
  observeEvent(input$submit_flaw, {
    req(rv$phase == 1L)
    req(!is.null(input$flaw_choice))

    rd      <- rounds[[rv$round]]
    correct <- input$flaw_choice == rd$flaw

    if (correct) {
      rv$phase1_pts <- 10L
      rv$score      <- rv$score + 10L
    } else {
      rv$phase1_pts <- 0L
    }

    rv$phase <- 2L

    # Show phase 1 feedback, unlock phase 2
    shinyjs::hide("phase1_area")
    shinyjs::show("phase2_area")
  })

  # ---- Phase 1 feedback --------------------------------------------------
  output$phase1_feedback_ui <- renderUI({
    req(rv$phase >= 2L, rv$round > 0)
    rd      <- rounds[[rv$round]]
    correct <- (rv$phase1_pts == 10L)

    if (correct) {
      tags$div(
        class = "feedback-correct",
        tags$strong("\u2705 Correct!"),
        tags$span(style = "margin-left:6px;", paste0("+10 points")),
        tags$p(style = "margin-top:6px; margin-bottom:0; font-size:0.92rem;",
               tags$strong("Principle: "), rd$flaw),
        tags$p(style = "margin-bottom:0; font-size:0.9rem; color:#065f46;",
               rd$explain)
      )
    } else {
      tags$div(
        class = "feedback-wrong",
        tags$strong("\u274C Incorrect"),
        tags$p(style = "margin-top:6px; margin-bottom:0; font-size:0.92rem;",
               tags$strong("The correct answer: "), rd$flaw),
        tags$p(style = "margin-bottom:0; font-size:0.9rem; color:#991b1b;",
               rd$explain)
      )
    }
  })

  # ---- Phase 2: submit severity ------------------------------------------
  observeEvent(input$submit_severity, {
    req(rv$phase == 2L)

    rd     <- rounds[[rv$round]]
    rating <- input$severity_rating
    diff   <- abs(rating - rd$expert)

    if (diff == 0) {
      rv$phase2_pts <- 5L
    } else if (diff == 1) {
      rv$phase2_pts <- 2L
    } else {
      rv$phase2_pts <- 0L
    }
    rv$score <- rv$score + rv$phase2_pts

    # Record result
    rv$results[[rv$round]] <- list(
      round       = rv$round,
      title       = rd$title,
      flaw        = rd$flaw,
      correct     = (rv$phase1_pts == 10L),
      phase1_pts  = rv$phase1_pts,
      student_sev = rating,
      expert_sev  = rd$expert,
      phase2_pts  = rv$phase2_pts,
      total_pts   = rv$phase1_pts + rv$phase2_pts
    )

    rv$phase <- 3L

    shinyjs::hide("phase2_area")
    shinyjs::show("next_area")
  })

  # ---- Phase 2 feedback --------------------------------------------------
  output$phase2_feedback_ui <- renderUI({
    req(rv$phase >= 3L, rv$round > 0)
    rd     <- rounds[[rv$round]]
    rating <- input$severity_rating
    diff   <- abs(rating - rd$expert)
    pts    <- rv$phase2_pts

    sev_msg <- if (diff == 0) {
      paste0("Exact match! +5 bonus points.")
    } else if (diff == 1) {
      paste0("Within 1 point \u2014 good eye! +2 bonus points.")
    } else {
      paste0("The expert rates this ", rd$expert, "/5. ", rd$expert_why, " +0 bonus.")
    }

    tags$div(
      class = "feedback-severity",
      tags$strong("Severity Comparison"),
      tags$p(style = "margin:6px 0 0 0; font-size:0.92rem;",
             paste0("Your rating: ", rating, " / 5  |  Expert rating: ", rd$expert, " / 5")),
      tags$p(style = "margin:4px 0 0 0; font-size:0.9rem; color:#92400e;",
             sev_msg)
    )
  })

  # ---- Next round --------------------------------------------------------
  observeEvent(input$next_round, {
    if (rv$round >= TOTAL_ROUNDS) {
      # Game over
      rv$finished  <- TRUE
      rv$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      shinyjs::hide("game_area")
      shinyjs::show("completion_area")
    } else {
      rv$round     <- rv$round + 1L
      rv$phase     <- 1L
      rv$phase1_pts <- 0L
      rv$phase2_pts <- 0L

      shinyjs::show("phase1_area")
      shinyjs::hide("phase2_area")
      shinyjs::hide("next_area")

      updateSliderInput(session, "severity_rating", value = 3)
    }
  })

  # ---- Completion report -------------------------------------------------
  output$completion_report_ui <- renderUI({
    req(rv$finished)

    name  <- trimws(input$player_name)
    ts    <- rv$timestamp
    score <- rv$score
    hash  <- substr(digest::digest(paste(name, score, ts), algo = "md5"), 1, 8)

    # Build per-round table rows
    rows <- lapply(rv$results, function(r) {
      status <- if (r$correct) "\u2705" else "\u274C"
      tags$tr(
        tags$td(r$round),
        tags$td(r$title),
        tags$td(status),
        tags$td(paste0(r$student_sev, " / ", r$expert_sev)),
        tags$td(style = "font-weight:600;", r$total_pts)
      )
    })

    # Report plain text for clipboard
    report_lines <- c(
      "=== DESIGN DETECTIVE \u2014 COMPLETION REPORT ===",
      paste0("Student: ", name),
      paste0("Date/Time: ", ts),
      paste0("Score: ", score, " / ", MAX_SCORE),
      paste0("Verification: ", hash),
      "",
      "Round | Chart | Correct | Sev (You/Expert) | Points"
    )
    for (r in rv$results) {
      status <- if (r$correct) "Yes" else "No"
      report_lines <- c(report_lines,
        paste0(r$round, " | ", r$title, " | ", status,
               " | ", r$student_sev, "/", r$expert_sev,
               " | ", r$total_pts)
      )
    }
    report_text <- paste(report_lines, collapse = "\n")

    tags$div(
      class = "completion-card",
      tags$textarea(
        id = "report_text_store",
        style = "display:none;",
        report_text
      ),
      card(
        card_header("Game Complete!"),
        card_body(
          tags$div(
            style = "text-align:center; margin-bottom:14px;",
            tags$h4(style = paste0("color:", NAVY, ";"), name),
            tags$p(style = "color:#64748b; font-size:0.9rem; margin:2px 0;", ts),
            tags$h3(style = paste0("color:", NAVY, "; margin:10px 0;"),
                    paste0("Score: ", score, " / ", MAX_SCORE)),
            tags$span(class = "hash-code", paste0("Verification: ", hash))
          ),

          hr(),

          tags$h6("Per-Round Breakdown", style = paste0("color:", NAVY, "; font-weight:700;")),
          tags$table(
            class = "report-table",
            tags$thead(tags$tr(
              tags$th("#"),
              tags$th("Chart"),
              tags$th("Flaw ID"),
              tags$th("Severity (You/Expert)"),
              tags$th("Pts")
            )),
            tags$tbody(rows)
          ),

          hr(),

          tags$div(
            class = "reflection-box",
            tags$label(
              "for" = "reflection_input",
              style = "display:block; margin-bottom:8px; font-size:0.9rem;",
              tags$strong("Ignatian Reflection: "),
              "Think of a time you encountered a misleading visualization. How might it have influenced understanding or decisions?"
            ),
            tags$textarea(
              id = "reflection_input",
              class = "form-control",
              rows = "3",
              placeholder = "Write 1-2 sentences here...",
              style = "font-size:0.88rem; resize:vertical;"
            )
          ),

          tags$button(
            id = "copy_report_btn",
            class = "btn-game w-100",
            style = "margin-bottom:14px;",
            onclick = "copyGameReport()",
            icon("clipboard"), " Copy Report to Clipboard"
          ),

          tags$span(id = "copy_confirm",
                    style = "display:none; color:#059669; font-weight:600; text-align:center; font-size:0.9rem;",
                    "Report copied to clipboard!")
        )
      )
    )
  })

  # ---- Prevent empty-name start ------------------------------------------
  observe({
    if (nchar(trimws(input$player_name)) == 0) {
      shinyjs::disable("start_btn")
    } else {
      shinyjs::enable("start_btn")
    }
  })
}


# ---------------------------------------------------------------------------
shinyApp(ui, server)
