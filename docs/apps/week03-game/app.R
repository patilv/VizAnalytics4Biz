library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(sortable)

# ---------------------------------------------------------------------------
# Optional packages - graceful fallback
# ---------------------------------------------------------------------------
has_shinyjs <- requireNamespace("shinyjs", quietly = TRUE)
if (has_shinyjs) library(shinyjs)

has_spinners <- requireNamespace("shinycssloaders", quietly = TRUE)
if (has_spinners) library(shinycssloaders)

has_scales <- requireNamespace("scales", quietly = TRUE)

wrap_spinner <- function(ui_element, color = "#002967") {
  if (has_spinners) {
    shinycssloaders::withSpinner(ui_element, color = color, type = 6)
  } else {
    ui_element
  }
}

# ---------------------------------------------------------------------------
# Gonzaga brand colours
# ---------------------------------------------------------------------------
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ---------------------------------------------------------------------------
# Round definitions
# ---------------------------------------------------------------------------
# Each round: correct_fragments (in order), full_code (string for display),
#   dataset_name, n_layers, points_per_round = 15
rounds <- list(
  list(
    id = 1,
    title = "Round 1: Simple Scatterplot",
    dataset_name = "mpg",
    correct_fragments = c(
      "ggplot(mpg, aes(x = displ, y = hwy))",
      "geom_point()",
      'labs(title = "Engine Size vs Highway MPG")',
      "theme_minimal()"
    ),
    full_code = 'ggplot(mpg, aes(x = displ, y = hwy)) +\n  geom_point() +\n  labs(title = "Engine Size vs Highway MPG") +\n  theme_minimal()',
    build_plot = function() {
      ggplot(mpg, aes(x = displ, y = hwy)) +
        geom_point() +
        labs(title = "Engine Size vs Highway MPG") +
        theme_minimal()
    }
  ),
  list(
    id = 2,
    title = "Round 2: Bar Chart with Fill",
    dataset_name = "mpg",
    correct_fragments = c(
      "ggplot(mpg, aes(x = class, fill = class))",
      "geom_bar()",
      'scale_fill_brewer(palette = "Set2")',
      'labs(title = "Vehicle Classes", y = "Count")',
      "theme_minimal()"
    ),
    full_code = 'ggplot(mpg, aes(x = class, fill = class)) +\n  geom_bar() +\n  scale_fill_brewer(palette = "Set2") +\n  labs(title = "Vehicle Classes", y = "Count") +\n  theme_minimal()',
    build_plot = function() {
      ggplot(mpg, aes(x = class, fill = class)) +
        geom_bar() +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "Vehicle Classes", y = "Count") +
        theme_minimal()
    }
  ),
  list(
    id = 3,
    title = "Round 3: Scatter + Trend Line",
    dataset_name = "diamonds",
    correct_fragments = c(
      "ggplot(diamonds, aes(x = carat, y = price))",
      "geom_point(alpha = 0.1)",
      'geom_smooth(method = "lm", color = "red")',
      'labs(title = "Diamond Price vs Carat")',
      "theme_minimal()"
    ),
    full_code = 'ggplot(diamonds, aes(x = carat, y = price)) +\n  geom_point(alpha = 0.1) +\n  geom_smooth(method = "lm", color = "red") +\n  labs(title = "Diamond Price vs Carat") +\n  theme_minimal()',
    build_plot = function() {
      ggplot(diamonds, aes(x = carat, y = price)) +
        geom_point(alpha = 0.1) +
        geom_smooth(method = "lm", color = "red") +
        labs(title = "Diamond Price vs Carat") +
        theme_minimal()
    }
  ),
  list(
    id = 4,
    title = "Round 4: Faceted Color Scatter",
    dataset_name = "mpg",
    correct_fragments = c(
      "ggplot(mpg, aes(x = displ, y = hwy, color = class))",
      "geom_point(size = 2)",
      "facet_wrap(~drv)",
      'scale_color_brewer(palette = "Dark2")',
      'labs(title = "MPG by Drive Type")',
      "theme_bw()"
    ),
    full_code = 'ggplot(mpg, aes(x = displ, y = hwy, color = class)) +\n  geom_point(size = 2) +\n  facet_wrap(~drv) +\n  scale_color_brewer(palette = "Dark2") +\n  labs(title = "MPG by Drive Type") +\n  theme_bw()',
    build_plot = function() {
      ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
        geom_point(size = 2) +
        facet_wrap(~drv) +
        scale_color_brewer(palette = "Dark2") +
        labs(title = "MPG by Drive Type") +
        theme_bw()
    }
  ),
  list(
    id = 5,
    title = "Round 5: Box + Jitter with Custom Colors",
    dataset_name = "iris",
    correct_fragments = c(
      "ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species))",
      "geom_boxplot()",
      "geom_jitter(width = 0.2, alpha = 0.3)",
      'scale_fill_manual(values = c("#002967", "#C41E3A", "#B4975A"))',
      'labs(title = "Sepal Length by Species")',
      "theme_minimal()"
    ),
    full_code = 'ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +\n  geom_boxplot() +\n  geom_jitter(width = 0.2, alpha = 0.3) +\n  scale_fill_manual(values = c("#002967", "#C41E3A", "#B4975A")) +\n  labs(title = "Sepal Length by Species") +\n  theme_minimal()',
    build_plot = function() {
      ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
        geom_boxplot() +
        geom_jitter(width = 0.2, alpha = 0.3) +
        scale_fill_manual(values = c("#002967", "#C41E3A", "#B4975A")) +
        labs(title = "Sepal Length by Species") +
        theme_minimal()
    }
  ),
  list(
    id = 6,
    title = "Round 6: Time Series with Smoother",
    dataset_name = "economics",
    correct_fragments = c(
      "ggplot(economics, aes(x = date, y = unemploy/1000))",
      'geom_line(linewidth = 1, color = "#002967")',
      'geom_smooth(method = "loess", se = TRUE, color = "#C41E3A")',
      "scale_y_continuous(labels = scales::comma)",
      'labs(title = "US Unemployment Over Time", y = "Unemployed (thousands)")',
      "theme_minimal()"
    ),
    full_code = 'ggplot(economics, aes(x = date, y = unemploy/1000)) +\n  geom_line(linewidth = 1, color = "#002967") +\n  geom_smooth(method = "loess", se = TRUE, color = "#C41E3A") +\n  scale_y_continuous(labels = scales::comma) +\n  labs(title = "US Unemployment Over Time", y = "Unemployed (thousands)") +\n  theme_minimal()',
    build_plot = function() {
      ggplot(economics, aes(x = date, y = unemploy / 1000)) +
        geom_line(linewidth = 1, color = "#002967") +
        geom_smooth(method = "loess", se = TRUE, color = "#C41E3A") +
        scale_y_continuous(labels = if (has_scales) scales::comma else waiver()) +
        labs(title = "US Unemployment Over Time", y = "Unemployed (thousands)") +
        theme_minimal()
    }
  ),
  list(
    id = 7,
    title = "Round 7: Faceted Scatter with Trend",
    dataset_name = "mpg",
    correct_fragments = c(
      "ggplot(mpg, aes(x = cty, y = hwy))",
      "geom_point(aes(color = class), size = 2)",
      'geom_smooth(method = "lm", se = FALSE)',
      "facet_wrap(~year)",
      'scale_color_brewer(palette = "Set1")',
      'labs(title = "City vs Highway MPG by Year", x = "City MPG", y = "Highway MPG")',
      "theme_light()"
    ),
    full_code = 'ggplot(mpg, aes(x = cty, y = hwy)) +\n  geom_point(aes(color = class), size = 2) +\n  geom_smooth(method = "lm", se = FALSE) +\n  facet_wrap(~year) +\n  scale_color_brewer(palette = "Set1") +\n  labs(title = "City vs Highway MPG by Year", x = "City MPG", y = "Highway MPG") +\n  theme_light()',
    build_plot = function() {
      ggplot(mpg, aes(x = cty, y = hwy)) +
        geom_point(aes(color = class), size = 2) +
        geom_smooth(method = "lm", se = FALSE) +
        facet_wrap(~year) +
        scale_color_brewer(palette = "Set1") +
        labs(title = "City vs Highway MPG by Year", x = "City MPG", y = "Highway MPG") +
        theme_light()
    }
  ),
  list(
    id = 8,
    title = "Round 8: Violin + Boxplot with Coord Flip",
    dataset_name = "diamonds (sample of 1000)",
    correct_fragments = c(
      "ggplot(diamond_sample, aes(x = cut, y = price, fill = cut))",
      "geom_violin()",
      "geom_boxplot(width = 0.1, fill = \"white\")",
      "scale_fill_viridis_d()",
      "coord_flip()",
      'labs(title = "Diamond Price Distribution by Cut")',
      "theme_minimal()"
    ),
    full_code = 'ggplot(diamond_sample, aes(x = cut, y = price, fill = cut)) +\n  geom_violin() +\n  geom_boxplot(width = 0.1, fill = "white") +\n  scale_fill_viridis_d() +\n  coord_flip() +\n  labs(title = "Diamond Price Distribution by Cut") +\n  theme_minimal()',
    build_plot = function() {
      set.seed(42)
      ds <- diamonds[sample(nrow(diamonds), 1000), ]
      ggplot(ds, aes(x = cut, y = price, fill = cut)) +
        geom_violin() +
        geom_boxplot(width = 0.1, fill = "white") +
        scale_fill_viridis_d() +
        coord_flip() +
        labs(title = "Diamond Price Distribution by Cut") +
        theme_minimal()
    }
  )
)

TOTAL_ROUNDS <- length(rounds)
MAX_SCORE    <- TOTAL_ROUNDS * 15

# ---------------------------------------------------------------------------
# Helper: try to build a plot from student's fragment ordering
# ---------------------------------------------------------------------------
try_build_student_plot <- function(fragments, round_info) {
  # For round 8, we need the diamond_sample in scope
  if (round_info$id == 8) {
    set.seed(42)
    diamond_sample <- diamonds[sample(nrow(diamonds), 1000), ]
  }

  # Combine fragments with +
  code_str <- paste(fragments, collapse = " + \n  ")

  result <- tryCatch({
    p <- eval(parse(text = code_str))
    list(success = TRUE, plot = p, error = NULL)
  }, error = function(e) {
    list(success = FALSE, plot = NULL, error = conditionMessage(e))
  })

  result
}

# ---------------------------------------------------------------------------
# Helper: score a student ordering vs correct ordering
# Uses CATEGORY-AWARE scoring:
#   - ggplot() must be first
#   - geom_*() layers must maintain their relative order (visual layering)
#   - Everything else (labs, theme, scale, facet, coord) is order-independent
# ---------------------------------------------------------------------------
score_round <- function(student_order, correct_order) {
  n <- length(correct_order)
  position_details <- character(n)

  # Rule 1: ggplot() must be first
  ggplot_first <- length(student_order) >= 1 &&
    grepl("^ggplot\\(", student_order[1])

  # Rule 2: geoms must be in correct relative order
  correct_geoms <- correct_order[grepl("^geom_", correct_order)]
  student_geoms <- student_order[grepl("^geom_", student_order)]
  has_multiple_geoms <- length(correct_geoms) > 1
  geoms_correct <- identical(correct_geoms, student_geoms)

  # Score: 15 pts per round
  if (has_multiple_geoms) {
    # 5 pts for ggplot first, 10 pts for geom order
    score <- 0
    if (ggplot_first) score <- score + 5
    if (geoms_correct) score <- score + 10
  } else {
    # Single geom: ggplot first = full credit
    score <- if (ggplot_first) 15 else 0
  }

  # Build position details for display
  for (i in seq_along(correct_order)) {
    student_frag <- if (i <= length(student_order)) student_order[i] else ""
    correct_frag <- correct_order[i]
    if (student_frag == correct_frag) {
      position_details[i] <- "correct"
    } else if (!grepl("^ggplot\\(|^geom_", student_frag) &&
               !grepl("^ggplot\\(|^geom_", correct_frag)) {
      # Both are non-order-sensitive (scale/labs/theme/facet/coord)
      position_details[i] <- "flexible"
    } else {
      position_details[i] <- "wrong"
    }
  }

  list(
    score = score,
    ggplot_first = ggplot_first,
    geoms_correct = geoms_correct,
    has_multiple_geoms = has_multiple_geoms,
    correct_positions = sum(position_details == "correct"),
    total_positions = n,
    details = position_details
  )
}

# ---------------------------------------------------------------------------
# Helper: generate hash code for completion report
# ---------------------------------------------------------------------------
generate_hash <- function(name, score, date_str) {
  raw <- paste0(name, score, date_str, "LayerCake2026")
  if (requireNamespace("digest", quietly = TRUE)) {
    code <- paste0("LC-", toupper(substr(digest::digest(raw, algo = "md5"), 1, 8)))
  } else {
    # Fallback: simple numeric hash
    val <- sum(utf8ToInt(raw)) %% 99999
    code <- paste0("LC-", sprintf("%05d", val))
  }
  code
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_sidebar(
  title = NULL,
  fillable = FALSE,
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),

  if (has_shinyjs) shinyjs::useShinyjs() else NULL,

  tags$head(tags$style(HTML(sprintf("
    /* Sortable styling */
    .rank-list-container .rank-list-item {
      background: #f8fafc;
      border: 2px solid #cbd5e1;
      border-radius: 8px;
      padding: 10px 14px;
      margin-bottom: 6px;
      font-family: 'Fira Code', 'Consolas', monospace;
      font-size: 0.88rem;
      cursor: grab;
      transition: all 0.15s ease;
    }
    .rank-list-container .rank-list-item:hover {
      border-color: %s;
      background: #eff6ff;
      box-shadow: 0 2px 4px rgba(0,41,103,0.12);
    }
    .rank-list-container .rank-list-item.sortable-chosen {
      border-color: %s;
      background: #fef3c7;
      box-shadow: 0 4px 12px rgba(0,41,103,0.2);
    }
    .rank-list-container {
      min-height: 80px;
      background: #ffffff;
      border: 2px dashed #e2e8f0;
      border-radius: 10px;
      padding: 10px;
    }
    .rank-list-container .rank-list-title {
      font-weight: 600;
      color: %s;
      font-size: 0.9rem;
      margin-bottom: 8px;
    }
    .result-correct {
      background: #f0fdf4 !important;
      border-left: 4px solid #22c55e !important;
      padding: 14px;
      border-radius: 8px;
      margin-bottom: 10px;
    }
    .result-partial {
      background: #fffbeb !important;
      border-left: 4px solid #f59e0b !important;
      padding: 14px;
      border-radius: 8px;
      margin-bottom: 10px;
    }
    .result-wrong {
      background: #fef2f2 !important;
      border-left: 4px solid #ef4444 !important;
      padding: 14px;
      border-radius: 8px;
      margin-bottom: 10px;
    }
    .layer-position-correct {
      color: #16a34a;
      font-weight: 600;
    }
    .layer-position-wrong {
      color: #dc2626;
      font-weight: 600;
    }
    .code-display {
      background: #1e293b;
      color: #e2e8f0;
      font-family: 'Fira Code', 'Consolas', monospace;
      font-size: 0.85rem;
      padding: 14px;
      border-radius: 8px;
      overflow-x: auto;
      white-space: pre-wrap;
      margin: 8px 0;
    }
    .progress-step {
      display: inline-block;
      width: 28px;
      height: 28px;
      line-height: 28px;
      text-align: center;
      border-radius: 50%%;
      font-size: 0.75rem;
      font-weight: 700;
      margin: 0 2px;
    }
    .step-done     { background: #22c55e; color: #fff; }
    .step-current  { background: %s; color: #fff; }
    .step-upcoming { background: #e2e8f0; color: #64748b; }
    .score-display {
      font-size: 1.6rem;
      font-weight: 700;
      color: %s;
      text-align: center;
    }
    .site-footer {
      text-align: center;
      font-size: 0.78rem;
      color: #64748b;
      padding: 10px 0 4px 0;
      border-top: 1px solid #e2e8f0;
      margin-top: 16px;
    }
    .site-footer strong { color: %s; }
    .btn-check-order {
      background: %s;
      color: #fff;
      border: none;
      font-weight: 600;
      padding: 10px 28px;
      border-radius: 8px;
      font-size: 1rem;
    }
    .btn-check-order:hover {
      background: %s;
      color: #fff;
    }
    .btn-next-round {
      background: #22c55e;
      color: #fff;
      border: none;
      font-weight: 600;
      padding: 10px 28px;
      border-radius: 8px;
      font-size: 1rem;
    }
    .btn-next-round:hover {
      background: #16a34a;
      color: #fff;
    }
    .btn-start-game {
      width: 100%%;
      font-weight: 700;
      padding: 10px;
    }
    .dataset-badge {
      display: inline-block;
      background: %s;
      color: #fff;
      padding: 2px 12px;
      border-radius: 12px;
      font-size: 0.8rem;
      font-weight: 600;
    }
    .completion-card {
      border: 2px solid %s;
    }
    .reflection-box {
      background: #fffbeb;
      border-left: 4px solid %s;
      padding: 16px;
      border-radius: 0 8px 8px 0;
      font-style: italic;
      font-size: 0.9rem;
      line-height: 1.6;
      color: #374151;
      margin-top: 12px;
    }
    .hash-code {
      font-family: 'Fira Code', monospace;
      font-size: 1.2rem;
      font-weight: 700;
      color: %s;
      background: #f1f5f9;
      padding: 8px 16px;
      border-radius: 6px;
      display: inline-block;
      letter-spacing: 1px;
    }
    .btn-copy-hash {
      background: %s;
      color: #fff;
      border: none;
      padding: 6px 16px;
      border-radius: 4px;
      font-size: 0.82rem;
      cursor: pointer;
      margin-left: 8px;
    }
    .btn-copy-hash:hover { background: %s; color: #fff; }
  ", NAVY, GOLD, NAVY, NAVY, NAVY, NAVY, NAVY, RED, GOLD, NAVY, GOLD, NAVY, NAVY, RED))),
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

  # --- Sidebar ---
 sidebar = sidebar(
    width = 310, bg = "#f1f5f9",

    # Instruction accordion
    accordion(
      id = "help_accordion",
      open = FALSE,
      accordion_panel(
        title = "How to Play Layer Cake",
        icon  = bsicons::bs_icon("question-circle"),
        tags$ol(
          style = "font-size: 0.82rem; padding-left: 1.1rem; margin-bottom: 0;",
          tags$li("Enter your name and click ", tags$strong("Start Game"), "."),
          tags$li("Each round shows a ", tags$strong("target plot"), " built with ggplot2."),
          tags$li(tags$strong("Drag and drop"), " the scrambled code layers into the correct order (top = first layer)."),
          tags$li("Click ", tags$strong("Check Order"), " to see how you did."),
          tags$li("You earn points for each layer in the ", tags$strong("correct position"), "."),
          tags$li("After 8 rounds, see your ", tags$strong("completion report"), "."),
          tags$li("Write a brief ", tags$strong("reflection"), " before copying your completion report.")
        )
      )
    ),

    tags$hr(style = "margin: 6px 0;"),

    textInput("player_name", "Your Name:",
              placeholder = "Enter your name"),

    actionButton("start_game", "Start Game",
                 icon  = icon("play"),
                 class = "btn-primary btn-start-game"),

    tags$hr(style = "margin: 8px 0;"),

    # Progress tracker
    p(strong("Progress:"), style = "margin-bottom: 6px; color: #002967;"),
    uiOutput("progress_dots"),

    tags$hr(style = "margin: 8px 0;"),

    # Score display
    p(strong("Score:"), style = "margin-bottom: 4px; color: #002967;"),
    uiOutput("score_display")
  ),

  # --- Main content ---
  uiOutput("main_content"),

  # --- Footer ---
  tags$div(
    class = "site-footer",
    HTML("<strong>Gonzaga University</strong> | Dr.&nbsp;Vivek&nbsp;H.&nbsp;Patil")
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive values to track game state
  game <- reactiveValues(
    started      = FALSE,
    current_round = 0,
    scores       = rep(NA_real_, TOTAL_ROUNDS),
    round_details = vector("list", TOTAL_ROUNDS),
    checked      = FALSE,    # has current round been checked?
    finished     = FALSE,
    scrambled    = NULL       # current scrambled order
  )

  # --- Start Game ---
  observeEvent(input$start_game, {
    req(nchar(trimws(input$player_name)) > 0)
    game$started       <- TRUE
    game$current_round <- 1
    game$scores        <- rep(NA_real_, TOTAL_ROUNDS)
    game$round_details <- vector("list", TOTAL_ROUNDS)
    game$checked       <- FALSE
    game$finished      <- FALSE
    # Scramble fragments for round 1
    r <- rounds[[1]]
    set.seed(as.numeric(Sys.time()))
    game$scrambled <- sample(r$correct_fragments)
  })

  # --- Progress dots ---
  output$progress_dots <- renderUI({
    if (!game$started) {
      return(tags$span("Start the game to begin.", style = "color: #94a3b8; font-size: 0.85rem;"))
    }

    dots <- lapply(seq_len(TOTAL_ROUNDS), function(i) {
      cls <- if (!is.na(game$scores[i])) {
        "progress-step step-done"
      } else if (i == game$current_round && !game$finished) {
        "progress-step step-current"
      } else {
        "progress-step step-upcoming"
      }
      tags$span(class = cls, i)
    })

    tags$div(dots, style = "margin-bottom: 6px;")
  })

  # --- Score display ---
  output$score_display <- renderUI({
    total <- sum(game$scores, na.rm = TRUE)
    tags$div(
      class = "score-display",
      paste0(total, " / ", MAX_SCORE)
    )
  })

  # --- Main content area ---
  output$main_content <- renderUI({
    if (!game$started) {
      # Welcome screen
      card(
        card_header(
          tags$span(
            icon("layer-group", style = paste0("color: ", GOLD, "; margin-right: 8px;")),
            "Layer Cake"
          ),
          class = "bg-primary text-white"
        ),
        card_body(
          style = "text-align: center; padding: 40px 20px;",
          tags$h2("Welcome to Layer Cake!", style = paste0("color: ", NAVY, ";")),
          tags$p(
            class = "lead",
            "Stack the grammar of graphics -- in the right order."
          ),
          tags$p(
            "In ggplot2, plots are built by ", tags$strong("layering"), " components: ",
            "data, geometries, scales, facets, labels, and themes. ",
            "Each round shows you a target plot and gives you the code layers in scrambled order. ",
            "Your job: drag them into the correct sequence!",
            style = "font-size: 0.95rem; max-width: 600px; margin: 0 auto; line-height: 1.6;"
          ),
          tags$hr(),
          tags$p(
            tags$strong("8 rounds"), " | ",
            tags$strong("15 points per round"), " | ",
            tags$strong("120 points max"),
            style = "font-size: 1rem; color: #475569;"
          ),
          tags$p(
            "Enter your name in the sidebar and click ",
            tags$strong("Start Game"), " to begin!",
            style = "color: #64748b; margin-top: 10px;"
          )
        )
      )
    } else if (game$finished) {
      # Completion report
      uiOutput("completion_report")
    } else {
      # Active round
      uiOutput("round_ui")
    }
  })

  # --- Active round UI ---
  output$round_ui <- renderUI({
    req(game$current_round >= 1, game$current_round <= TOTAL_ROUNDS)
    r <- rounds[[game$current_round]]

    tagList(
      # Round header
      card(
        card_header(
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            tags$span(
              icon("layer-group", style = paste0("color: ", GOLD, "; margin-right: 8px;")),
              r$title
            ),
            tags$span(class = "dataset-badge", paste0("Dataset: ", r$dataset_name))
          ),
          class = "bg-primary text-white"
        ),
        card_body(
          # Target plot
          tags$h5("Target Plot:", style = paste0("color: ", NAVY, "; font-weight: 600; margin-bottom: 10px;")),
          tags$p("Study this plot carefully. Your goal is to order the code layers that produce it.",
                 style = "color: #64748b; font-size: 0.85rem; margin-bottom: 8px;"),
          wrap_spinner(plotOutput(paste0("target_plot_", game$current_round), height = "380px")),

          tags$hr(),

          # Correct code (hidden until checked)
          if (game$checked) {
            tags$div(
              tags$h5("Correct Code:", style = paste0("color: ", NAVY, "; font-weight: 600;")),
              tags$div(class = "code-display", r$full_code)
            )
          } else {
            NULL
          }
        )
      ),

      # Drag and drop area
      card(
        card_header("Arrange the Layers", class = "bg-light"),
        card_body(
          if (!game$checked) {
            tagList(
              tags$p(
                icon("arrows-alt-v", style = "margin-right: 6px;"),
                "Drag the code fragments below into the correct order. ",
                "The first layer (top) should be the ", tags$code("ggplot()"), " call.",
                style = "font-size: 0.88rem; color: #475569; margin-bottom: 12px;"
              ),
              rank_list(
                text = "Drag layers into the correct order (top = first):",
                labels = as.list(game$scrambled),
                input_id = "layer_order"
              ),
              tags$div(
                style = "text-align: center; margin-top: 16px;",
                actionButton("check_order", "Check Order",
                             icon = icon("check-circle"),
                             class = "btn-check-order")
              )
            )
          } else {
            # Show results
            tagList(
              uiOutput("round_feedback"),
              tags$div(
                style = "text-align: center; margin-top: 16px;",
                if (game$current_round < TOTAL_ROUNDS) {
                  actionButton("next_round", "Next Round",
                               icon = icon("arrow-right"),
                               class = "btn-next-round")
                } else {
                  actionButton("finish_game", "See Results",
                               icon = icon("trophy"),
                               class = "btn-next-round")
                }
              )
            )
          }
        )
      ),

      # Side-by-side plots after checking
      if (game$checked) {
        card(
          card_header("Side-by-Side Comparison", class = "bg-light"),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              card(
                card_header("Your Result", class = "bg-light text-center"),
                card_body(
                  wrap_spinner(plotOutput(paste0("student_plot_", game$current_round), height = "340px"))
                )
              ),
              card(
                card_header("Correct Plot", class = "bg-light text-center"),
                card_body(
                  wrap_spinner(plotOutput(paste0("correct_plot_", game$current_round), height = "340px"))
                )
              )
            )
          )
        )
      } else {
        NULL
      }
    )
  })

  # --- Render target plot for the current round ---
  observe({
    req(game$started, !game$finished)
    rnd <- game$current_round
    req(rnd >= 1, rnd <= TOTAL_ROUNDS)

    local({
      my_rnd <- rnd
      output[[paste0("target_plot_", my_rnd)]] <- renderPlot({
        rounds[[my_rnd]]$build_plot()
      }, res = 110)
    })
  })

  # --- Check Order ---
  observeEvent(input$check_order, {
    req(!game$checked)
    rnd <- game$current_round
    r <- rounds[[rnd]]
    student_order <- input$layer_order

    # Score
    result <- score_round(student_order, r$correct_fragments)
    game$scores[rnd] <- result$score
    game$round_details[[rnd]] <- list(
      student_order = student_order,
      correct_order = r$correct_fragments,
      score_result  = result,
      student_build = try_build_student_plot(student_order, r)
    )
    game$checked <- TRUE

    # Render student and correct plots
    local({
      my_rnd <- rnd
      my_r   <- r
      my_detail <- game$round_details[[rnd]]

      output[[paste0("student_plot_", my_rnd)]] <- renderPlot({
        if (my_detail$student_build$success) {
          my_detail$student_build$plot
        } else {
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = paste0("Build Error:\n", my_detail$student_build$error),
                     size = 4.5, color = "#dc2626", hjust = 0.5, vjust = 0.5) +
            theme_void() +
            labs(title = "Could not render your layer order") +
            theme(plot.title = element_text(color = "#dc2626", face = "bold", hjust = 0.5))
        }
      }, res = 110)

      output[[paste0("correct_plot_", my_rnd)]] <- renderPlot({
        my_r$build_plot()
      }, res = 110)
    })
  })

  # --- Round feedback ---
  output$round_feedback <- renderUI({
    rnd <- game$current_round
    detail <- game$round_details[[rnd]]
    req(detail)

    result <- detail$score_result
    perfect <- result$ggplot_first && result$geoms_correct
    failed  <- !result$ggplot_first && !result$geoms_correct

    # Build position-by-position feedback
    position_items <- lapply(seq_along(detail$correct_order), function(i) {
      status <- result$details[i]
      student_frag <- if (i <= length(detail$student_order)) detail$student_order[i] else "(missing)"
      correct_frag <- detail$correct_order[i]

      status_badge <- if (status == "correct") {
        tags$span(class = "layer-position-correct", icon("check"), " Correct")
      } else if (status == "flexible") {
        tags$span(style = "color: #0284c7; font-weight: 600;", icon("arrows-alt"), " Flexible")
      } else {
        tags$span(class = "layer-position-wrong", icon("times"), " Wrong")
      }

      tags$tr(
        tags$td(
          style = "padding: 6px 10px; font-weight: 600; text-align: center;",
          paste0("Layer ", i)
        ),
        tags$td(
          style = "padding: 6px 10px; font-family: 'Fira Code', monospace; font-size: 0.82rem;",
          student_frag
        ),
        tags$td(
          style = "padding: 6px 10px; font-family: 'Fira Code', monospace; font-size: 0.82rem;",
          correct_frag
        ),
        tags$td(
          style = "padding: 6px 10px; text-align: center;",
          status_badge
        )
      )
    })

    feedback_class <- if (perfect) {
      "result-correct"
    } else if (failed) {
      "result-wrong"
    } else {
      "result-partial"
    }

    # Build feedback message based on what matters
    ggplot_msg <- if (!result$ggplot_first) {
      tags$p(
        tags$strong("ggplot() must come first."),
        " It initializes the plot object that all other layers add to.",
        style = "color: #dc2626; margin-bottom: 4px; font-size: 0.88rem;"
      )
    }
    geom_msg <- if (result$has_multiple_geoms && !result$geoms_correct) {
      tags$p(
        tags$strong("Geom order matters!"),
        " When you have multiple geom_*() layers, later geoms are drawn on top. ",
        "For example, geom_jitter() should come after geom_boxplot() so the points are visible above the boxes.",
        style = "color: #dc2626; margin-bottom: 4px; font-size: 0.88rem;"
      )
    }

    feedback_msg <- if (perfect) {
      tags$div(
        tags$h5(icon("star", style = "color: #22c55e;"), " Perfect! All key ordering rules satisfied.",
                style = "color: #16a34a; font-weight: 700;"),
        tags$p("You nailed the grammar of graphics for this plot!",
               style = "color: #475569; margin-bottom: 0;")
      )
    } else {
      tags$div(
        tags$h5(
          icon("info-circle", style = paste0("color: ", if (failed) "#dc2626" else "#f59e0b", ";")),
          if (result$has_multiple_geoms) {
            paste0(" Score: ", result$score, " / 15")
          } else {
            " ggplot() must come first!"
          },
          style = paste0("color: ", if (failed) "#dc2626" else "#92400e", "; font-weight: 700;")
        ),
        ggplot_msg,
        geom_msg,
        tags$p(
          tags$strong("Key rules:"), " (1) ggplot() always comes first. ",
          "(2) When you have multiple geoms, their order determines visual layering (later = on top). ",
          "(3) scale_*(), labs(), theme_*(), facet_*(), and coord_*() can go in any order ",
          HTML("&mdash;"), " positions marked ", tags$span(style = "color: #0284c7;", "Flexible"),
          " are fine wherever you placed them.",
          style = "color: #475569; margin-bottom: 0; font-size: 0.88rem;"
        )
      )
    }

    tagList(
      tags$div(
        class = feedback_class,
        feedback_msg,
        tags$p(
          style = "margin-top: 8px; font-weight: 600;",
          paste0("Score: ", result$score, " / 15")
        )
      ),

      tags$div(
        style = "overflow-x: auto;",
        tags$table(
          style = "width: 100%; border-collapse: collapse; font-size: 0.85rem; margin-top: 10px;",
          tags$thead(
            tags$tr(
              style = paste0("background: ", NAVY, "; color: #fff;"),
              tags$th(style = "padding: 8px 10px; text-align: center;", "Position"),
              tags$th(style = "padding: 8px 10px;", "Your Order"),
              tags$th(style = "padding: 8px 10px;", "Correct Order"),
              tags$th(style = "padding: 8px 10px; text-align: center;", "Result")
            )
          ),
          tags$tbody(position_items)
        )
      ),

      if (!detail$student_build$success) {
        tags$div(
          style = "margin-top: 10px; padding: 10px; background: #fef2f2; border-radius: 6px;",
          tags$p(
            icon("exclamation-triangle", style = "color: #dc2626;"),
            tags$strong(" Build Error: "),
            "Your layer order produced an error: ",
            tags$code(detail$student_build$error),
            style = "font-size: 0.85rem; color: #991b1b; margin-bottom: 0;"
          )
        )
      }
    )
  })

  # --- Next Round ---
  observeEvent(input$next_round, {
    next_rnd <- game$current_round + 1
    if (next_rnd <= TOTAL_ROUNDS) {
      game$current_round <- next_rnd
      game$checked <- FALSE
      # Scramble fragments for next round
      r <- rounds[[next_rnd]]
      set.seed(as.numeric(Sys.time()) + next_rnd)
      game$scrambled <- sample(r$correct_fragments)
    }
  })

  # --- Finish Game ---
  observeEvent(input$finish_game, {
    game$finished <- TRUE
  })

  # --- Completion Report ---
  output$completion_report <- renderUI({
    total_score <- sum(game$scores, na.rm = TRUE)
    player <- trimws(input$player_name)
    date_str <- format(Sys.Date(), "%B %d, %Y")
    hash_code <- generate_hash(player, total_score, date_str)

    # Performance level
    pct <- total_score / MAX_SCORE * 100
    perf_level <- if (pct >= 90) {
      "Master Layer Architect"
    } else if (pct >= 75) {
      "Skilled Builder"
    } else if (pct >= 50) {
      "Emerging Designer"
    } else {
      "Keep Practicing"
    }

    # Per-round table rows
    round_rows <- lapply(seq_len(TOTAL_ROUNDS), function(i) {
      detail <- game$round_details[[i]]
      score <- game$scores[i]
      result <- if (!is.null(detail)) detail$score_result else NULL
      n_correct <- if (!is.null(result)) result$correct_positions else 0
      n_total   <- if (!is.null(result)) result$total_positions else length(rounds[[i]]$correct_fragments)

      tags$tr(
        tags$td(style = "padding: 6px 10px; text-align: center; font-weight: 600;", i),
        tags$td(style = "padding: 6px 10px;", rounds[[i]]$title),
        tags$td(style = "padding: 6px 10px;", rounds[[i]]$dataset_name),
        tags$td(style = "padding: 6px 10px; text-align: center;", n_total),
        tags$td(style = "padding: 6px 10px; text-align: center;",
                paste0(n_correct, "/", n_total)),
        tags$td(style = "padding: 6px 10px; text-align: center; font-weight: 600;",
                paste0(score, "/15"))
      )
    })

    # Build plain-text report for clipboard
    report_lines <- c(
      "===== LAYER CAKE -- COMPLETION REPORT =====",
      paste0("Player:    ", player),
      paste0("Date:      ", date_str),
      paste0("Score:     ", total_score, " / ", MAX_SCORE, "  (", round(pct), "%)"),
      paste0("Level:     ", perf_level),
      paste0("Hash:      ", hash_code),
      ""
    )
    for (i in seq_len(TOTAL_ROUNDS)) {
      detail <- game$round_details[[i]]
      score_val <- game$scores[i]
      result <- if (!is.null(detail)) detail$score_result else NULL
      n_correct <- if (!is.null(result)) result$correct_positions else 0
      n_total   <- if (!is.null(result)) result$total_positions else length(rounds[[i]]$correct_fragments)
      report_lines <- c(report_lines,
        sprintf("Round %d: %s (%s) | %d/%d correct | %d/15 pts",
                i, rounds[[i]]$title, rounds[[i]]$dataset_name, n_correct, n_total, score_val))
    }
    report_text <- paste(report_lines, collapse = "\n")

    card(
      class = "completion-card",
      card_header(
        tags$div(
          style = "text-align: center;",
          icon("trophy", style = paste0("color: ", GOLD, "; font-size: 1.4rem; margin-right: 8px;")),
          "Layer Cake - Completion Report"
        ),
        style = paste0("background: ", NAVY, "; color: #fff; font-size: 1.1rem; font-weight: 700;")
      ),
      card_body(
        # Player info
        layout_columns(
          col_widths = c(4, 4, 4),
          tags$div(
            style = "text-align: center;",
            tags$p(style = "color: #64748b; font-size: 0.8rem; margin-bottom: 2px;", "Player"),
            tags$p(style = paste0("color: ", NAVY, "; font-weight: 700; font-size: 1.1rem;"), player)
          ),
          tags$div(
            style = "text-align: center;",
            tags$p(style = "color: #64748b; font-size: 0.8rem; margin-bottom: 2px;", "Date"),
            tags$p(style = paste0("color: ", NAVY, "; font-weight: 700; font-size: 1.1rem;"), date_str)
          ),
          tags$div(
            style = "text-align: center;",
            tags$p(style = "color: #64748b; font-size: 0.8rem; margin-bottom: 2px;", "Performance"),
            tags$p(style = paste0("color: ", NAVY, "; font-weight: 700; font-size: 1.1rem;"), perf_level)
          )
        ),

        tags$hr(),

        # Score
        tags$div(
          style = "text-align: center; margin: 16px 0;",
          tags$span(
            style = paste0("font-size: 2.4rem; font-weight: 800; color: ", NAVY, ";"),
            total_score
          ),
          tags$span(
            style = "font-size: 1.4rem; color: #64748b; font-weight: 400;",
            paste0(" / ", MAX_SCORE)
          ),
          tags$div(
            style = "margin-top: 4px;",
            tags$div(
              style = paste0(
                "height: 10px; background: #e2e8f0; border-radius: 5px; ",
                "max-width: 400px; margin: 0 auto; overflow: hidden;"
              ),
              tags$div(style = paste0(
                "height: 100%; background: ", NAVY, "; border-radius: 5px; ",
                "width: ", pct, "%;"
              ))
            )
          )
        ),

        tags$hr(),

        # Per-round table
        tags$h5("Round-by-Round Results:", style = paste0("color: ", NAVY, "; font-weight: 700;")),
        tags$div(
          style = "overflow-x: auto;",
          tags$table(
            style = "width: 100%; border-collapse: collapse; font-size: 0.85rem; margin-top: 8px;",
            tags$thead(
              tags$tr(
                style = paste0("background: ", NAVY, "; color: #fff;"),
                tags$th(style = "padding: 8px 10px; text-align: center;", "#"),
                tags$th(style = "padding: 8px 10px;", "Round"),
                tags$th(style = "padding: 8px 10px;", "Dataset"),
                tags$th(style = "padding: 8px 10px; text-align: center;", "Layers"),
                tags$th(style = "padding: 8px 10px; text-align: center;", "Correct"),
                tags$th(style = "padding: 8px 10px; text-align: center;", "Score")
              )
            ),
            tags$tbody(round_rows)
          )
        ),

        tags$hr(),

        # Hash code
        tags$div(
          style = "text-align: center; margin: 16px 0;",
          tags$p(style = "color: #64748b; font-size: 0.85rem; margin-bottom: 6px;",
                 "Verification Code:"),
          tags$span(class = "hash-code", hash_code),
          if (has_shinyjs) {
            actionButton("copy_hash", "Copy",
                         icon = icon("clipboard"),
                         class = "btn-copy-hash")
          } else {
            NULL
          }
        ),

        tags$hr(),

        # Reflection input
        tags$div(
          class = "reflection-box",
          tags$label(
            "for" = "reflection_input",
            style = "display:block; margin-bottom:8px; font-size:0.9rem;",
            tags$strong("Ignatian Reflection: "),
            "How does understanding the grammar of graphics help you communicate more honestly and clearly with data?"
          ),
          tags$textarea(
            id = "reflection_input",
            class = "form-control",
            rows = "3",
            placeholder = "Write 1-2 sentences here...",
            style = "font-size:0.88rem; resize:vertical;"
          )
        ),

        # Copy report button
        tags$div(
          style = "margin-top:12px;",
          tags$button(
            id = "copy_report_btn",
            class = "btn btn-outline-primary btn-sm w-100",
            onclick = "copyGameReport()",
            icon("clipboard"), " Copy Report to Clipboard"
          ),
          tags$span(id = "copy_confirm",
                    style = "margin-left:8px; color:#16a34a; font-size:0.85rem; display:none;",
                    bsicons::bs_icon("check-lg"), " Copied!")
        ),

        # Hidden textarea for clipboard
        tags$textarea(
          id = "report_text_store",
          style = "display:none;",
          report_text
        ),

        # Play again
        tags$div(
          style = "text-align: center; margin-top: 20px;",
          actionButton("play_again", "Play Again",
                       icon = icon("redo"),
                       class = "btn-primary",
                       style = "padding: 10px 30px; font-weight: 600;")
        )
      )
    )
  })

  # --- Copy hash ---
  if (has_shinyjs) {
    observeEvent(input$copy_hash, {
      total_score <- sum(game$scores, na.rm = TRUE)
      player <- trimws(input$player_name)
      date_str <- format(Sys.Date(), "%B %d, %Y")
      hash_code <- generate_hash(player, total_score, date_str)

      safe <- gsub("'", "\\\\'", hash_code)
      shinyjs::runjs(sprintf(
        "navigator.clipboard.writeText('%s').then(function(){
           Shiny.notifications.show({html:'<strong>Copied!</strong> Verification code is on your clipboard.', type:'message', duration:2500});
         }).catch(function(){
           var ta=document.createElement('textarea');ta.value='%s';document.body.appendChild(ta);ta.select();document.execCommand('copy');document.body.removeChild(ta);
           Shiny.notifications.show({html:'<strong>Copied!</strong>', type:'message', duration:2000});
         });", safe, safe
      ))
    })
  }

  # --- Play Again ---
  observeEvent(input$play_again, {
    game$started       <- FALSE
    game$current_round <- 0
    game$scores        <- rep(NA_real_, TOTAL_ROUNDS)
    game$round_details <- vector("list", TOTAL_ROUNDS)
    game$checked       <- FALSE
    game$finished      <- FALSE
    game$scrambled     <- NULL
    updateTextInput(session, "player_name", value = "")
  })
}

# ---------------------------------------------------------------------------
shinyApp(ui, server)
