# ============================================================================
# Pipe Dream  --  Week 05 Game
# "Get the verbs in order or the pipeline breaks."
#
# Players drag dplyr verb cards into the correct pipeline order, then predict
# the resulting number of rows and columns.  8 rounds using mpg and storms.
# ============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(sortable)

# ---- Gonzaga palette -------------------------------------------------------
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ---- helpers ---------------------------------------------------------------
with_spinner <- function(ui_element) {
  if (requireNamespace("shinycssloaders", quietly = TRUE)) {
    shinycssloaders::withSpinner(ui_element, color = NAVY, type = 6)
  } else {
    ui_element
  }
}

# ---- round definitions -----------------------------------------------------
# Each round: goal (plain English), correct_verbs (ordered), decoys,
#   dataset, execute_fn (function returning the correct result data.frame)

rounds <- list(
  # ---- Round 1: 3 verbs, mpg ----
  list(
    goal = "From the mpg dataset, keep only cars with more than 30 highway MPG, and show just the manufacturer and model.",
    dataset_name = "mpg",
    correct_verbs = c(
      "filter(hwy > 30)",
      "select(manufacturer, model)"
    ),
    decoys = c("arrange(desc(hwy))"),
    execute_fn = function() {
      mpg %>%
        filter(hwy > 30) %>%
        select(manufacturer, model)
    },
    verb_explanations = list(
      "filter(hwy > 30)" = "filter() keeps rows matching a condition -- here, highway MPG above 30.",
      "select(manufacturer, model)" = "select() picks specific columns to keep.",
      "arrange(desc(hwy))" = "arrange() sorts rows but doesn't filter or select -- it's a decoy here."
    )
  ),

  # ---- Round 2: 3 verbs, mpg ----
  list(
    goal = "Sort cars by city MPG from highest to lowest, then keep only the top variables: manufacturer, model, and cty.",
    dataset_name = "mpg",
    correct_verbs = c(
      "arrange(desc(cty))",
      "select(manufacturer, model, cty)"
    ),
    decoys = c("filter(cty > 20)"),
    execute_fn = function() {
      mpg %>%
        arrange(desc(cty)) %>%
        select(manufacturer, model, cty)
    },
    verb_explanations = list(
      "arrange(desc(cty))" = "arrange(desc()) sorts rows from highest to lowest.",
      "select(manufacturer, model, cty)" = "select() narrows to just these three columns.",
      "filter(cty > 20)" = "filter() would remove rows, but the goal says 'sort', not 'remove' -- decoy!"
    )
  ),

  # ---- Round 3: 4 verbs (3 correct + 1 decoy), mpg ----
  list(
    goal = "Find the average highway MPG for each manufacturer, then sort from highest to lowest.",
    dataset_name = "mpg",
    correct_verbs = c(
      "group_by(manufacturer)",
      "summarise(avg_hwy = mean(hwy))",
      "arrange(desc(avg_hwy))"
    ),
    decoys = c("filter(hwy > 25)"),
    execute_fn = function() {
      mpg %>%
        group_by(manufacturer) %>%
        summarise(avg_hwy = mean(hwy)) %>%
        arrange(desc(avg_hwy))
    },
    verb_explanations = list(
      "group_by(manufacturer)" = "group_by() sets up groups so summarise knows what to aggregate over.",
      "summarise(avg_hwy = mean(hwy))" = "summarise() collapses each group into one summary row.",
      "arrange(desc(avg_hwy))" = "arrange(desc()) sorts the result from highest to lowest.",
      "filter(hwy > 25)" = "filter() would remove rows before summarising -- the goal asks for ALL manufacturers' averages."
    )
  ),

  # ---- Round 4: 4 verbs (3 correct + 1 decoy), mpg ----
  list(
    goal = "Add a column for the difference between highway and city MPG, keep only SUVs, then sort by this new column.",
    dataset_name = "mpg",
    correct_verbs = c(
      "mutate(mpg_diff = hwy - cty)",
      "filter(class == \"suv\")",
      "arrange(desc(mpg_diff))"
    ),
    decoys = c("select(manufacturer, model, mpg_diff)"),
    execute_fn = function() {
      mpg %>%
        mutate(mpg_diff = hwy - cty) %>%
        filter(class == "suv") %>%
        arrange(desc(mpg_diff))
    },
    verb_explanations = list(
      "mutate(mpg_diff = hwy - cty)" = "mutate() adds a new column without removing existing ones.",
      "filter(class == \"suv\")" = "filter() keeps only SUV rows.",
      "arrange(desc(mpg_diff))" = "arrange(desc()) sorts by the new column, highest first.",
      "select(manufacturer, model, mpg_diff)" = "select() would drop most columns -- the goal says 'add a column', not 'keep only these columns'."
    )
  ),

  # ---- Round 5: 4 verbs (3 correct + 1 decoy), storms ----
  list(
    goal = "From the storms dataset, keep only Category 4 and 5 hurricanes, then find the average wind speed for each category.",
    dataset_name = "storms",
    correct_verbs = c(
      "filter(category >= 4)",
      "group_by(category)",
      "summarise(avg_wind = mean(wind))"
    ),
    decoys = c("arrange(wind)"),
    execute_fn = function() {
      storms %>%
        filter(category >= 4) %>%
        group_by(category) %>%
        summarise(avg_wind = mean(wind))
    },
    verb_explanations = list(
      "filter(category >= 4)" = "filter() keeps only rows where category is 4 or 5.",
      "group_by(category)" = "group_by() groups the remaining rows by category (4 vs 5).",
      "summarise(avg_wind = mean(wind))" = "summarise() computes the average wind within each group.",
      "arrange(wind)" = "arrange() would sort, but the goal doesn't mention sorting -- decoy!"
    )
  ),

  # ---- Round 6: 5 verbs (4 correct + 1 decoy), storms ----
  list(
    goal = "Count how many storm records exist for each storm status, add a percentage column, then sort by count descending.",
    dataset_name = "storms",
    correct_verbs = c(
      "group_by(status)",
      "summarise(n = n())",
      "mutate(pct = round(n/sum(n)*100, 1))",
      "arrange(desc(n))"
    ),
    decoys = c("filter(wind > 50)"),
    execute_fn = function() {
      storms %>%
        group_by(status) %>%
        summarise(n = n()) %>%
        mutate(pct = round(n / sum(n) * 100, 1)) %>%
        arrange(desc(n))
    },
    verb_explanations = list(
      "group_by(status)" = "group_by() groups records by storm status.",
      "summarise(n = n())" = "summarise(n = n()) counts the number of rows in each group.",
      "mutate(pct = round(n/sum(n)*100, 1))" = "mutate() adds a percentage column based on the counts.",
      "arrange(desc(n))" = "arrange(desc(n)) sorts from most to fewest records.",
      "filter(wind > 50)" = "filter() would remove records before counting -- the goal says count ALL statuses."
    )
  ),

  # ---- Round 7: 5 verbs (4 correct + 1 decoy + 1 extra decoy), mpg ----
  list(
    goal = "For 4-cylinder cars only, find the average city and highway MPG by manufacturer, keeping only those with average highway MPG above 25.",
    dataset_name = "mpg",
    correct_verbs = c(
      "filter(cyl == 4)",
      "group_by(manufacturer)",
      "summarise(avg_cty = mean(cty), avg_hwy = mean(hwy))",
      "filter(avg_hwy > 25)"
    ),
    decoys = c("select(manufacturer, cty, hwy)", "arrange(avg_hwy)"),
    execute_fn = function() {
      mpg %>%
        filter(cyl == 4) %>%
        group_by(manufacturer) %>%
        summarise(avg_cty = mean(cty), avg_hwy = mean(hwy)) %>%
        filter(avg_hwy > 25)
    },
    verb_explanations = list(
      "filter(cyl == 4)" = "filter() first narrows to 4-cylinder cars before grouping.",
      "group_by(manufacturer)" = "group_by() sets up manufacturer groups for summarising.",
      "summarise(avg_cty = mean(cty), avg_hwy = mean(hwy))" = "summarise() computes both averages per manufacturer.",
      "filter(avg_hwy > 25)" = "This second filter() keeps only manufacturers whose average highway MPG exceeds 25.",
      "select(manufacturer, cty, hwy)" = "select() picks columns but doesn't aggregate -- you'd lose the averages.",
      "arrange(avg_hwy)" = "arrange() sorts but the goal doesn't mention sorting -- decoy!"
    )
  ),

  # ---- Round 8: 7 verbs (6 correct + 1 decoy), storms ----
  list(
    goal = "Find the maximum wind speed for each named storm, keep only storms with max wind >= 100 knots, add a column classifying them as 'Category 4' (100-129 knots) or 'Category 5' (130+ knots), then select name, max_wind, and category, and sort alphabetically.",
    dataset_name = "storms",
    correct_verbs = c(
      "group_by(name)",
      "summarise(max_wind = max(wind))",
      "filter(max_wind >= 100)",
      "mutate(category = ifelse(max_wind >= 130, \"Category 5\", \"Category 4\"))",
      "select(name, max_wind, category)",
      "arrange(name)"
    ),
    decoys = c("filter(status == \"hurricane\")"),
    execute_fn = function() {
      storms %>%
        group_by(name) %>%
        summarise(max_wind = max(wind)) %>%
        filter(max_wind >= 100) %>%
        mutate(category = ifelse(max_wind >= 130, "Category 5", "Category 4")) %>%
        select(name, max_wind, category) %>%
        arrange(name)
    },
    verb_explanations = list(
      "group_by(name)" = "group_by(name) groups all records by storm name.",
      "summarise(max_wind = max(wind))" = "summarise() finds the peak wind speed for each storm.",
      "filter(max_wind >= 100)" = "filter() keeps only the most powerful storms (>= 100 knots).",
      "mutate(category = ifelse(max_wind >= 130, \"Category 5\", \"Category 4\"))" = "mutate() adds a classification column based on wind speed thresholds.",
      "select(name, max_wind, category)" = "select() keeps only the three requested columns.",
      "arrange(name)" = "arrange(name) sorts alphabetically by storm name.",
      "filter(status == \"hurricane\")" = "Filtering by status before summarising would miss data -- we want max wind across ALL records per storm."
    )
  )
)

TOTAL_ROUNDS <- length(rounds)
PTS_ORDER    <- 10
PTS_ROWS     <- 3
PTS_COLS     <- 2
MAX_PER_ROUND <- PTS_ORDER + PTS_ROWS + PTS_COLS  # 15
MAX_TOTAL    <- MAX_PER_ROUND * TOTAL_ROUNDS        # 120

# ---- CSS -------------------------------------------------------------------
game_css <- "
/* ---------- general ---------- */
body { background: #f8fafc; }

/* ---------- sidebar ---------- */
.score-chip {
  display: inline-block;
  background: #002967;
  color: white;
  padding: 5px 16px;
  border-radius: 20px;
  font-weight: 700;
  font-size: 1.1rem;
  margin: 2px 0 4px;
}

/* ---------- progress ---------- */
.round-progress { margin: 6px 0; }
.round-progress .progress {
  height: 10px;
  border-radius: 5px;
  background: #e2e8f0;
}
.round-progress .progress-bar {
  background: #B4975A;
  border-radius: 5px;
  transition: width 0.5s ease;
}

/* ---------- welcome card ---------- */
.welcome-card {
  max-width: 660px;
  margin: 50px auto;
  text-align: center;
  padding: 44px 36px;
  background: white;
  border-radius: 14px;
  border: 1px solid #e2e8f0;
  box-shadow: 0 4px 24px rgba(0,41,103,0.06);
}
.welcome-card h2 {
  color: #002967;
  font-weight: 800;
  margin-bottom: 4px;
}
.welcome-card .tagline {
  color: #B4975A;
  font-style: italic;
  font-size: 1.05rem;
  margin-bottom: 18px;
}
.welcome-card p { color: #475569; font-size: 0.95rem; }

/* ---------- round container ---------- */
.round-container {
  max-width: 860px;
  margin: 0 auto;
  padding: 10px 0 30px;
}

/* ---------- goal card ---------- */
.goal-card {
  background: white;
  border: 2px solid #002967;
  border-radius: 12px;
  padding: 20px 24px;
  margin-bottom: 20px;
}
.goal-card .goal-label {
  font-size: 0.78rem;
  text-transform: uppercase;
  letter-spacing: 1.2px;
  color: #B4975A;
  font-weight: 700;
  margin-bottom: 6px;
}
.goal-card .goal-text {
  font-size: 1.05rem;
  color: #1e293b;
  line-height: 1.6;
}
.goal-card .goal-dataset {
  display: inline-block;
  background: #f1f5f9;
  border: 1px solid #e2e8f0;
  border-radius: 6px;
  padding: 2px 10px;
  font-family: 'Fira Code', monospace;
  font-size: 0.85rem;
  color: #002967;
  font-weight: 600;
  margin-top: 8px;
}

/* ---------- pipeline area ---------- */
.pipeline-section {
  background: white;
  border: 1px solid #e2e8f0;
  border-radius: 12px;
  padding: 20px 24px;
  margin-bottom: 20px;
}
.pipeline-section h5 {
  color: #002967;
  font-weight: 700;
  margin-bottom: 12px;
  font-size: 0.95rem;
}

/* sortable rank_list styling */
.rank-list-container .rank-list {
  background: #f8fafc !important;
  border: 2px dashed #cbd5e1 !important;
  border-radius: 10px !important;
  padding: 8px !important;
  min-height: 60px;
}
.rank-list-container .rank-list-item {
  background: white !important;
  border: 2px solid #002967 !important;
  border-radius: 8px !important;
  padding: 10px 16px !important;
  margin: 6px 0 !important;
  font-family: 'Fira Code', monospace !important;
  font-size: 0.88rem !important;
  color: #002967 !important;
  font-weight: 600 !important;
  cursor: grab !important;
  transition: box-shadow 0.15s, transform 0.15s !important;
}
.rank-list-container .rank-list-item:hover {
  box-shadow: 0 3px 12px rgba(0,41,103,0.15) !important;
  transform: translateY(-1px) !important;
}
.rank-list-container .rank-list-item.sortable-chosen {
  box-shadow: 0 4px 16px rgba(0,41,103,0.25) !important;
  border-color: #B4975A !important;
}

/* pipe connectors */
.pipe-hint {
  text-align: center;
  font-family: 'Fira Code', monospace;
  font-size: 1.1rem;
  color: #B4975A;
  font-weight: 700;
  margin: 2px 0;
  letter-spacing: 2px;
}

/* ---------- prediction area ---------- */
.prediction-section {
  background: white;
  border: 1px solid #e2e8f0;
  border-radius: 12px;
  padding: 20px 24px;
  margin-bottom: 20px;
}
.prediction-section h5 {
  color: #002967;
  font-weight: 700;
  margin-bottom: 4px;
  font-size: 0.95rem;
}
.prediction-section .hint-text {
  font-size: 0.82rem;
  color: #94a3b8;
  margin-bottom: 12px;
}
.pred-row {
  display: flex;
  gap: 24px;
  align-items: center;
  flex-wrap: wrap;
}
.pred-row .form-group {
  margin-bottom: 0;
}

/* ---------- submit button ---------- */
.run-btn {
  background: #002967;
  border: none;
  color: white;
  font-weight: 700;
  font-size: 1rem;
  padding: 12px 32px;
  border-radius: 10px;
  letter-spacing: 0.3px;
  transition: background 0.2s, transform 0.15s;
  width: 100%;
  margin-top: 4px;
}
.run-btn:hover {
  background: #001a44;
  transform: translateY(-1px);
}
.run-btn:disabled {
  background: #94a3b8;
  cursor: not-allowed;
  transform: none;
}

/* ---------- feedback area ---------- */
.feedback-area {
  margin-top: 16px;
  animation: fadeSlideIn 0.35s ease;
}

.order-feedback {
  padding: 16px 20px;
  border-radius: 10px;
  font-size: 0.92rem;
  margin-bottom: 14px;
}
.order-feedback.correct {
  background: #f0fdf4;
  color: #166534;
  border: 1px solid #bbf7d0;
}
.order-feedback.wrong {
  background: #fef2f2;
  color: #991b1b;
  border: 1px solid #fecaca;
}
.order-feedback .feedback-title {
  font-weight: 700;
  font-size: 1rem;
  margin-bottom: 6px;
}
.order-feedback .divergence-detail {
  font-size: 0.85rem;
  margin-top: 6px;
  padding: 8px 12px;
  background: rgba(0,0,0,0.04);
  border-radius: 6px;
  font-family: 'Fira Code', monospace;
  line-height: 1.6;
}

.pred-feedback {
  display: flex;
  gap: 20px;
  flex-wrap: wrap;
  margin-bottom: 14px;
}
.pred-chip {
  padding: 8px 16px;
  border-radius: 8px;
  font-size: 0.88rem;
  font-weight: 600;
}
.pred-chip.correct-pred {
  background: #f0fdf4;
  color: #166534;
  border: 1px solid #bbf7d0;
}
.pred-chip.wrong-pred {
  background: #fef2f2;
  color: #991b1b;
  border: 1px solid #fecaca;
}

.result-table-wrapper {
  margin-top: 10px;
  border: 1px solid #e2e8f0;
  border-radius: 10px;
  overflow: hidden;
}
.result-table-wrapper .table-header {
  background: #002967;
  color: white;
  padding: 10px 16px;
  font-weight: 700;
  font-size: 0.88rem;
}

.next-btn {
  background: #B4975A;
  border: none;
  color: white;
  font-weight: 700;
  padding: 10px 28px;
  border-radius: 8px;
  font-size: 0.95rem;
  margin-top: 14px;
  transition: background 0.2s;
}
.next-btn:hover { background: #9a7d48; }

.round-score-chip {
  display: inline-block;
  background: #002967;
  color: white;
  padding: 4px 14px;
  border-radius: 16px;
  font-weight: 700;
  font-size: 0.9rem;
  margin-left: 10px;
}

/* ---------- result table ---------- */
.result-preview-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.82rem;
}
.result-preview-table th {
  background: #f1f5f9;
  color: #002967;
  padding: 8px 10px;
  text-align: left;
  font-weight: 700;
  border-bottom: 2px solid #e2e8f0;
  font-family: 'Fira Code', monospace;
  font-size: 0.78rem;
}
.result-preview-table td {
  padding: 6px 10px;
  border-bottom: 1px solid #f1f5f9;
  color: #334155;
}
.result-preview-table tr:hover td {
  background: #f8fafc;
}

@keyframes fadeSlideIn {
  from { opacity: 0; transform: translateY(-8px); }
  to   { opacity: 1; transform: translateY(0); }
}

/* ---------- completion report ---------- */
.report-card {
  max-width: 740px;
  margin: 30px auto;
  background: white;
  border-radius: 14px;
  border: 1px solid #e2e8f0;
  box-shadow: 0 4px 24px rgba(0,41,103,0.08);
  overflow: hidden;
}
.report-header {
  background: #002967;
  color: white;
  text-align: center;
  padding: 22px;
  font-size: 1.45rem;
  font-weight: 800;
  letter-spacing: 0.3px;
}
.report-body { padding: 28px 32px; }
.report-body .meta-line {
  font-size: 0.9rem;
  color: #475569;
  margin-bottom: 3px;
}
.report-body .final-score {
  font-size: 2.2rem;
  font-weight: 800;
  color: #002967;
  text-align: center;
  margin: 18px 0 6px;
}
.report-body .final-pct {
  text-align: center;
  font-size: 1rem;
  color: #64748b;
  margin-bottom: 18px;
}

.breakdown-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.82rem;
  margin-bottom: 18px;
}
.breakdown-table th {
  background: #f1f5f9;
  color: #002967;
  padding: 8px 10px;
  text-align: left;
  font-weight: 700;
  border-bottom: 2px solid #e2e8f0;
}
.breakdown-table td {
  padding: 7px 10px;
  border-bottom: 1px solid #f1f5f9;
}
.breakdown-table .result-correct { color: #16a34a; font-weight: 700; }
.breakdown-table .result-wrong   { color: #dc2626; font-weight: 700; }
.breakdown-table .result-partial { color: #d97706; font-weight: 700; }

.hash-code {
  display: inline-block;
  background: #f1f5f9;
  font-family: 'Fira Code', monospace;
  padding: 4px 14px;
  border-radius: 6px;
  font-size: 0.92rem;
  color: #002967;
  font-weight: 600;
  letter-spacing: 1px;
  margin: 4px 0 14px;
}

.reflection-box {
  background: #fffbeb;
  border-left: 4px solid #B4975A;
  padding: 14px 18px;
  border-radius: 0 8px 8px 0;
  margin: 16px 0 6px;
  font-size: 0.88rem;
  color: #78350f;
  line-height: 1.55;
}

#copy_report { margin-top: 6px; }

/* footer */
.game-footer {
  text-align: center;
  padding: 12px 0;
  font-size: 0.78rem;
  color: #94a3b8;
  border-top: 1px solid #e2e8f0;
  margin-top: 20px;
}
"

# ---- UI --------------------------------------------------------------------
ui <- page_sidebar(
  title    = NULL,
  fillable = FALSE,
  theme    = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),

  tags$head(
    tags$style(HTML(game_css)),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Fira+Code:wght@400;600&display=swap"
    ),
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

      // Update progress bar width
      Shiny.addCustomMessageHandler('updateBar', function(msg) {
        var bar = document.getElementById('round_bar');
        if (bar) bar.style.width = msg.pct + '%';
      });
    "))
  ),

  # ---- Sidebar ----
  sidebar = sidebar(
    width = 285,
    bg    = "#f1f5f9",

    # instructions accordion
    accordion(
      id   = "help_acc",
      open = FALSE,
      accordion_panel(
        title = "How to Play",
        icon  = bsicons::bs_icon("question-circle"),
        tags$ul(
          style = "font-size:0.82rem; padding-left:1.1rem; margin-bottom:0;",
          tags$li("Enter your name and press ", tags$strong("Start Game"), "."),
          tags$li("Each round shows a ", tags$strong("wrangling goal"),
                  " in plain English."),
          tags$li(tags$strong("Drag"), " the dplyr verb cards into the correct",
                  " pipeline order (top to bottom)."),
          tags$li("Not all cards belong in the pipeline -- watch for ",
                  tags$strong("decoys"), "!"),
          tags$li("Predict the number of ", tags$strong("rows"),
                  " and ", tags$strong("columns"), " in the result."),
          tags$li("Click ", tags$strong("Run Pipeline"),
                  " to check your answer."),
          tags$li("Scoring: 10 pts for correct order + 3 pts for exact row",
                  " prediction + 2 pts for exact column prediction = 15 max per round."),
          tags$li("8 rounds, 120 points possible."),
          tags$li("After all rounds, write a brief ", tags$strong("reflection"),
                  " before copying your completion report.")
        )
      )
    ),

    hr(style = "margin:8px 0;"),

    # player name
    textInput("player_name", "Your Name:", placeholder = "Enter your name"),
    actionButton("start_btn", "Start Game",
                 icon  = icon("play"),
                 class = "btn-primary w-100",
                 style = "font-weight:600;"),

    hr(style = "margin:10px 0;"),

    # progress
    tags$div(
      class = "round-progress",
      tags$small(
        style = "color:#64748b; font-weight:600;",
        textOutput("progress_text", inline = TRUE)
      ),
      tags$div(
        class = "progress",
        tags$div(
          id    = "round_bar",
          class = "progress-bar",
          role  = "progressbar",
          style = "width:0%;"
        )
      )
    ),

    tags$div(
      style = "text-align:center; margin-top:8px;",
      tags$small("Score", style = "color:#64748b; font-weight:600;"),
      tags$br(),
      tags$span(class = "score-chip",
                textOutput("score_display", inline = TRUE))
    ),

    hr(style = "margin:10px 0;"),

    # dataset info card
    tags$div(
      style = paste0(
        "background:white; border:1px solid #e2e8f0; border-radius:8px;",
        " padding:12px 14px; margin-top:4px;"
      ),
      tags$div(
        style = "font-weight:700; font-size:0.85rem; color:#002967; margin-bottom:6px;",
        bsicons::bs_icon("database"), " Active Dataset"
      ),
      uiOutput("dataset_info_sidebar")
    )
  ),

  # ==== MAIN PANEL ==========================================================
  useShinyjs(),
  uiOutput("main_ui"),

  tags$footer(
    class = "game-footer",
    "Gonzaga University | Dr. Vivek H. Patil"
  )
)

# ---- SERVER ----------------------------------------------------------------
server <- function(input, output, session) {

  # ---- state ---------------------------------------------------------------
  rv <- reactiveValues(
    round         = 0L,
    score         = 0L,
    answers       = list(),
    start_time    = NULL,
    player_name   = "",
    game_active   = FALSE,
    game_complete = FALSE,
    # per-round
    current_round_def  = NULL,
    current_all_verbs  = character(0),  # scrambled verbs for sortable
    submitted          = FALSE,
    round_pts          = 0L,
    round_result       = NULL,  # list with order_correct, row_correct, col_correct, etc.
    ui_refresh         = 0L     # counter to force UI refresh
  )

  # ---- start game ----------------------------------------------------------
  observeEvent(input$start_btn, {
    req(nchar(trimws(input$player_name)) > 0)
    rv$player_name   <- trimws(input$player_name)
    rv$round         <- 1L
    rv$score         <- 0L
    rv$answers       <- list()
    rv$start_time    <- Sys.time()
    rv$game_active   <- TRUE
    rv$game_complete <- FALSE
    setup_round()
  })

  setup_round <- function() {
    rd <- rounds[[rv$round]]
    rv$current_round_def <- rd

    # combine correct verbs + decoys, then shuffle
    all_verbs <- c(rd$correct_verbs, rd$decoys)
    rv$current_all_verbs <- sample(all_verbs)

    rv$submitted    <- FALSE
    rv$round_pts    <- 0L
    rv$round_result <- NULL
    rv$ui_refresh   <- rv$ui_refresh + 1L
  }

  # ---- progress outputs ----------------------------------------------------
  output$progress_text <- renderText({
    if (!rv$game_active && !rv$game_complete) return("Round -- / --")
    paste0("Round ", min(rv$round, TOTAL_ROUNDS), " / ", TOTAL_ROUNDS)
  })

  output$score_display <- renderText({
    paste0(rv$score, " / ", MAX_TOTAL)
  })

  # update progress bar
  observe({
    pct <- if (rv$game_active || rv$game_complete) {
      round((min(rv$round, TOTAL_ROUNDS) - ifelse(rv$game_complete, 0, 1)) /
              TOTAL_ROUNDS * 100)
    } else {
      0
    }
    if (rv$game_complete) pct <- 100
    session$sendCustomMessage("updateBar", list(pct = pct))
  })

  # ---- sidebar dataset info ------------------------------------------------
  output$dataset_info_sidebar <- renderUI({
    if (!rv$game_active) {
      return(tags$div(
        style = "font-size:0.82rem; color:#94a3b8; font-style:italic;",
        "Start a game to see dataset info."
      ))
    }
    rd <- rv$current_round_def
    req(rd)
    ds_name <- rd$dataset_name
    ds <- if (ds_name == "mpg") mpg else storms
    tags$div(
      tags$div(
        style = "font-family:'Fira Code',monospace; font-size:0.9rem; color:#002967; font-weight:700;",
        ds_name
      ),
      tags$div(
        style = "font-size:0.8rem; color:#64748b; margin-top:4px;",
        paste0(nrow(ds), " rows x ", ncol(ds), " columns")
      ),
      tags$div(
        style = "font-size:0.75rem; color:#94a3b8; margin-top:4px; font-style:italic;",
        if (ds_name == "mpg") {
          "EPA fuel economy data (1999-2008)"
        } else {
          "NOAA Atlantic hurricane data"
        }
      )
    )
  })

  # ---- main UI router ------------------------------------------------------
  output$main_ui <- renderUI({
    if (rv$game_complete) return(build_report_ui())
    if (!rv$game_active)  return(build_welcome_ui())
    # depend on refresh counter so UI rebuilds each round
    rv$ui_refresh
    build_round_ui()
  })

  # ---- welcome screen ------------------------------------------------------
  build_welcome_ui <- function() {
    tags$div(
      class = "welcome-card",
      tags$h2("Pipe Dream"),
      tags$div(class = "tagline",
               "Get the verbs in order or the pipeline breaks."),
      tags$hr(style = "border-color:#e2e8f0;"),
      tags$p("You will see a data wrangling goal described in plain English.",
             " Your job: arrange the ", tags$strong("dplyr verb cards"),
             " into the correct pipeline order by dragging them into place."),
      tags$p("But watch out -- some cards are ", tags$strong("decoys"),
             " that don't belong in the pipeline!"),
      tags$p("After ordering, predict the number of ",
             tags$strong("rows"), " and ", tags$strong("columns"),
             " in the result. Then click ",
             tags$strong("Run Pipeline"), " to see if your pipe dream holds up."),
      tags$p(style = "margin-top:18px;",
             tags$span(
               style = "background:#f1f5f9; padding:6px 14px; border-radius:8px; font-size:0.88rem; color:#002967;",
               tags$strong("8 rounds"), " | ",
               tags$strong("15 pts"), " per round | ",
               tags$strong("120 pts"), " maximum"
             )),
      tags$p(style = "color:#94a3b8; font-size:0.85rem; margin-top:16px;",
             "Enter your name in the sidebar and press ",
             tags$strong("Start Game"), ".")
    )
  }

  # ---- round UI ------------------------------------------------------------
  build_round_ui <- function() {
    rd  <- rv$current_round_def
    req(rd)
    ns  <- session$ns
    verbs <- rv$current_all_verbs

    # unique ID per round to avoid sortable conflicts
    sortable_id <- paste0("verb_order_", rv$round)

    tags$div(
      class = "round-container",

      # ---- goal card ----
      tags$div(
        class = "goal-card",
        tags$div(class = "goal-label",
                 paste0("Round ", rv$round, " of ", TOTAL_ROUNDS,
                        " -- Wrangling Goal")),
        tags$div(class = "goal-text", rd$goal),
        tags$div(class = "goal-dataset",
                 paste0("Dataset: ", rd$dataset_name,
                        " (", if (rd$dataset_name == "mpg") "234 rows, 11 cols"
                        else paste0(nrow(storms), " rows, ",
                                    ncol(storms), " cols"), ")"))
      ),

      # ---- pipeline section ----
      tags$div(
        class = "pipeline-section",
        tags$h5(
          bsicons::bs_icon("arrow-down-circle-fill"),
          paste0("  Build Your Pipeline (", length(rd$correct_verbs),
                 " verbs needed, ", length(rd$decoys),
                 " decoy", ifelse(length(rd$decoys) > 1, "s", ""), ")")
        ),
        tags$div(
          style = "font-size:0.82rem; color:#64748b; margin-bottom:10px;",
          "Drag the verb cards into the correct order from top (first step)",
          " to bottom (last step). Leave decoys at the bottom or in any position",
          " -- only the ", tags$strong("top ",
            length(rd$correct_verbs)), " cards will be evaluated as your pipeline."
        ),

        # pipe symbol at top
        tags$div(
          style = "text-align:left; margin-bottom:4px;",
          tags$span(
            style = paste0(
              "font-family:'Fira Code',monospace; font-size:0.85rem;",
              " color:#002967; font-weight:700; background:#f1f5f9;",
              " padding:4px 12px; border-radius:6px;"
            ),
            paste0(rd$dataset_name, " %>%")
          )
        ),

        # sortable rank_list
        rank_list(
          text       = NULL,
          labels     = verbs,
          input_id   = sortable_id,
          class      = "rank-list-container"
        ),

        tags$div(class = "pipe-hint", "|>  result")
      ),

      # ---- prediction section ----
      tags$div(
        class = "prediction-section",
        tags$h5(
          bsicons::bs_icon("calculator"),
          "  Predict the Result"
        ),
        tags$div(class = "hint-text",
                 "How many rows and columns will the final result have?"),
        tags$div(
          class = "pred-row",
          numericInput(
            paste0("pred_rows_", rv$round),
            "Predicted rows:",
            value = NA, min = 0, step = 1, width = "160px"
          ),
          numericInput(
            paste0("pred_cols_", rv$round),
            "Predicted columns:",
            value = NA, min = 0, step = 1, width = "160px"
          )
        )
      ),

      # ---- submit button ----
      actionButton(
        paste0("run_pipeline_", rv$round),
        label = "Run Pipeline",
        icon  = icon("play"),
        class = "run-btn"
      ),

      # ---- feedback area ----
      uiOutput("feedback_area")
    )
  }

  # ---- handle pipeline submission ------------------------------------------
  # We need an observer that watches for the run_pipeline button for the current round.
  observe({
    req(rv$game_active, rv$round > 0, !rv$submitted)

    btn_id  <- paste0("run_pipeline_", rv$round)
    sort_id <- paste0("verb_order_", rv$round)
    rows_id <- paste0("pred_rows_", rv$round)
    cols_id <- paste0("pred_cols_", rv$round)

    observeEvent(input[[btn_id]], {
      req(!rv$submitted)
      rd <- rv$current_round_def
      req(rd)

      # get student's verb ordering from sortable
      student_order <- input[[sort_id]]
      if (is.null(student_order) || length(student_order) == 0) return()

      # student's pipeline = top N verbs (where N = length of correct_verbs)
      n_correct <- length(rd$correct_verbs)
      student_pipeline <- student_order[seq_len(min(n_correct, length(student_order)))]

      # check order correctness
      order_correct <- identical(student_pipeline, rd$correct_verbs)

      # execute the correct pipeline to get actual result
      actual_result <- tryCatch(rd$execute_fn(), error = function(e) {
        data.frame(error = "Pipeline execution error")
      })
      actual_rows <- nrow(actual_result)
      actual_cols <- ncol(actual_result)

      # get predictions
      pred_rows <- input[[rows_id]]
      pred_cols <- input[[cols_id]]
      row_correct <- !is.null(pred_rows) && !is.na(pred_rows) &&
                     pred_rows == actual_rows
      col_correct <- !is.null(pred_cols) && !is.na(pred_cols) &&
                     pred_cols == actual_cols

      # score
      pts <- 0L
      pts_order <- 0L
      pts_rows  <- 0L
      pts_cols  <- 0L
      if (order_correct) {
        pts_order <- PTS_ORDER
        pts <- pts + pts_order
      }
      if (row_correct) {
        pts_rows <- PTS_ROWS
        pts <- pts + pts_rows
      }
      if (col_correct) {
        pts_cols <- PTS_COLS
        pts <- pts + pts_cols
      }

      rv$score     <- rv$score + pts
      rv$round_pts <- pts
      rv$submitted <- TRUE

      # find divergence point
      divergence_idx <- NA
      divergence_msg <- ""
      if (!order_correct) {
        for (i in seq_len(min(length(student_pipeline),
                              length(rd$correct_verbs)))) {
          if (student_pipeline[i] != rd$correct_verbs[i]) {
            divergence_idx <- i
            break
          }
        }
        if (is.na(divergence_idx)) {
          divergence_idx <- min(length(student_pipeline),
                                length(rd$correct_verbs)) + 1
        }

        # build explanation
        if (!is.na(divergence_idx) && divergence_idx <= length(rd$correct_verbs)) {
          student_verb <- if (divergence_idx <= length(student_pipeline)) {
            student_pipeline[divergence_idx]
          } else {
            "(missing)"
          }
          correct_verb <- rd$correct_verbs[divergence_idx]

          # get explanation for the correct verb
          explanation <- rd$verb_explanations[[correct_verb]]
          if (is.null(explanation)) explanation <- ""

          # get explanation for the student's (wrong) verb if it's a decoy
          student_explanation <- rd$verb_explanations[[student_verb]]
          if (is.null(student_explanation)) student_explanation <- ""

          divergence_msg <- paste0(
            "Step ", divergence_idx, ": You placed ",
            student_verb, " but the correct verb was ",
            correct_verb, ". ", explanation
          )
          if (nchar(student_explanation) > 0 && student_verb != correct_verb) {
            divergence_msg <- paste0(divergence_msg, " (Your choice: ",
                                     student_explanation, ")")
          }
        }
      }

      rv$round_result <- list(
        order_correct    = order_correct,
        row_correct      = row_correct,
        col_correct      = col_correct,
        pts_order        = pts_order,
        pts_rows         = pts_rows,
        pts_cols         = pts_cols,
        pts_total        = pts,
        student_pipeline = student_pipeline,
        correct_pipeline = rd$correct_verbs,
        actual_rows      = actual_rows,
        actual_cols      = actual_cols,
        pred_rows        = pred_rows,
        pred_cols        = pred_cols,
        actual_result    = actual_result,
        divergence_msg   = divergence_msg,
        divergence_idx   = divergence_idx
      )

      # record answer
      rv$answers[[rv$round]] <- list(
        round          = rv$round,
        goal           = rd$goal,
        dataset        = rd$dataset_name,
        order_correct  = order_correct,
        row_correct    = row_correct,
        col_correct    = col_correct,
        pts            = pts,
        student_order  = paste(student_pipeline, collapse = " %>% "),
        correct_order  = paste(rd$correct_verbs, collapse = " %>% ")
      )

      # disable the submit button
      shinyjs::disable(btn_id)
    }, ignoreInit = TRUE, once = TRUE)
  })

  # ---- next round handler --------------------------------------------------
  observe({
    req(rv$game_active, rv$round > 0)
    next_id <- paste0("next_round_", rv$round)

    observeEvent(input[[next_id]], {
      if (rv$round >= TOTAL_ROUNDS) {
        rv$game_active   <- FALSE
        rv$game_complete <- TRUE
      } else {
        rv$round <- rv$round + 1L
        setup_round()
      }
    }, ignoreInit = TRUE, once = TRUE)
  })

  # ---- feedback area rendering ---------------------------------------------
  output$feedback_area <- renderUI({
    req(rv$submitted, rv$round_result)
    res <- rv$round_result
    rd  <- rv$current_round_def

    # ---- Order feedback ----
    if (res$order_correct) {
      order_ui <- tags$div(
        class = "order-feedback correct",
        tags$div(class = "feedback-title",
                 bsicons::bs_icon("check-circle-fill"),
                 " Pipeline Order: Correct! (+", res$pts_order, " pts)"),
        tags$div(
          "Your pipeline: ",
          tags$code(
            style = "font-size:0.85rem;",
            paste0(rd$dataset_name, " %>% ",
                   paste(res$correct_pipeline, collapse = " %>% "))
          )
        )
      )
    } else {
      # show correct vs student pipeline
      correct_str <- paste0(rd$dataset_name, " %>% ",
                            paste(res$correct_pipeline, collapse = " %>% "))
      student_str <- paste0(rd$dataset_name, " %>% ",
                            paste(res$student_pipeline, collapse = " %>% "))

      order_ui <- tags$div(
        class = "order-feedback wrong",
        tags$div(class = "feedback-title",
                 bsicons::bs_icon("x-circle-fill"),
                 " Pipeline Order: Incorrect (+0 pts)"),
        tags$div(
          class = "divergence-detail",
          tags$div(
            tags$strong("Your order: "),
            tags$span(style = "color:#991b1b;", student_str)
          ),
          tags$div(
            style = "margin-top:4px;",
            tags$strong("Correct order: "),
            tags$span(style = "color:#166534;", correct_str)
          ),
          if (nchar(res$divergence_msg) > 0) {
            tags$div(
              style = "margin-top:8px; font-family:inherit; font-size:0.85rem;",
              tags$strong("Where it went wrong: "),
              res$divergence_msg
            )
          }
        )
      )
    }

    # ---- Row/Col prediction feedback ----
    row_class <- if (res$row_correct) "pred-chip correct-pred" else "pred-chip wrong-pred"
    col_class <- if (res$col_correct) "pred-chip correct-pred" else "pred-chip wrong-pred"

    row_icon <- if (res$row_correct) bsicons::bs_icon("check-circle-fill") else bsicons::bs_icon("x-circle-fill")
    col_icon <- if (res$col_correct) bsicons::bs_icon("check-circle-fill") else bsicons::bs_icon("x-circle-fill")

    pred_display_rows <- if (is.null(res$pred_rows) || is.na(res$pred_rows)) {
      "no answer"
    } else {
      res$pred_rows
    }
    pred_display_cols <- if (is.null(res$pred_cols) || is.na(res$pred_cols)) {
      "no answer"
    } else {
      res$pred_cols
    }

    pred_ui <- tags$div(
      class = "pred-feedback",
      tags$div(
        class = row_class,
        row_icon,
        paste0(" Rows: predicted ", pred_display_rows,
               ", actual ", res$actual_rows,
               " (+", res$pts_rows, " pts)")
      ),
      tags$div(
        class = col_class,
        col_icon,
        paste0(" Columns: predicted ", pred_display_cols,
               ", actual ", res$actual_cols,
               " (+", res$pts_cols, " pts)")
      )
    )

    # ---- Result table preview ----
    preview_data <- head(res$actual_result, 10)
    # build HTML table
    header_cells <- lapply(names(preview_data), function(nm) {
      tags$th(nm)
    })
    body_rows <- lapply(seq_len(nrow(preview_data)), function(i) {
      cells <- lapply(names(preview_data), function(nm) {
        val <- preview_data[[nm]][i]
        tags$td(as.character(val))
      })
      tags$tr(cells)
    })

    showing_text <- if (nrow(res$actual_result) > 10) {
      paste0("Showing first 10 of ", res$actual_rows, " rows")
    } else {
      paste0("Showing all ", res$actual_rows, " rows")
    }

    table_ui <- tags$div(
      class = "result-table-wrapper",
      tags$div(class = "table-header",
               bsicons::bs_icon("table"),
               paste0("  Result Preview -- ", showing_text,
                      ", ", res$actual_cols, " columns")),
      tags$div(
        style = "overflow-x:auto; max-height:360px; overflow-y:auto;",
        tags$table(
          class = "result-preview-table",
          tags$thead(tags$tr(header_cells)),
          tags$tbody(body_rows)
        )
      )
    )

    # ---- Round score summary + Next button ----
    next_label <- if (rv$round >= TOTAL_ROUNDS) "See Results" else "Next Round"
    next_icon  <- if (rv$round >= TOTAL_ROUNDS) icon("trophy") else icon("arrow-right")

    summary_ui <- tags$div(
      style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; margin-top:14px;",
      tags$div(
        tags$span(style = "font-weight:700; color:#002967; font-size:0.95rem;",
                  paste0("Round ", rv$round, " Score:")),
        tags$span(class = "round-score-chip",
                  paste0(res$pts_total, " / ", MAX_PER_ROUND))
      ),
      actionButton(
        paste0("next_round_", rv$round),
        label = next_label,
        icon  = next_icon,
        class = "next-btn"
      )
    )

    tags$div(
      class = "feedback-area",
      order_ui,
      pred_ui,
      table_ui,
      summary_ui
    )
  })

  # ---- completion report ---------------------------------------------------
  build_report_ui <- function() {
    ts      <- format(Sys.time(), "%B %d, %Y at %I:%M %p")
    pname   <- rv$player_name
    score   <- rv$score
    pct     <- round(score / MAX_TOTAL * 100)

    hash_input <- paste(pname, score, TOTAL_ROUNDS, ts, sep = "|")
    hash_code  <- substr(digest::digest(hash_input, algo = "md5"), 1, 8)

    # Build breakdown rows
    rows <- lapply(rv$answers, function(a) {
      pts <- a$pts
      if (pts == MAX_PER_ROUND) {
        res_class <- "result-correct"
        res_text  <- paste0(pts, "/", MAX_PER_ROUND, " -- Perfect!")
      } else if (pts > 0) {
        res_class <- "result-partial"
        res_text  <- paste0(pts, "/", MAX_PER_ROUND)
      } else {
        res_class <- "result-wrong"
        res_text  <- paste0(pts, "/", MAX_PER_ROUND)
      }

      order_mark <- if (a$order_correct) "Yes" else "No"
      row_mark   <- if (a$row_correct) "Yes" else "No"
      col_mark   <- if (a$col_correct) "Yes" else "No"

      tags$tr(
        tags$td(a$round),
        tags$td(style = "max-width:200px; font-size:0.78rem;",
                substr(a$goal, 1, 60),
                if (nchar(a$goal) > 60) "..." else ""),
        tags$td(order_mark),
        tags$td(row_mark),
        tags$td(col_mark),
        tags$td(class = res_class, res_text)
      )
    })

    # Plain-text version for clipboard
    report_lines <- c(
      "===== PIPE DREAM -- COMPLETION REPORT =====",
      paste0("Player:    ", pname),
      paste0("Date:      ", ts),
      paste0("Score:     ", score, " / ", MAX_TOTAL, "  (", pct, "%)"),
      paste0("Hash:      ", toupper(hash_code)),
      "",
      sprintf("%-5s | %-7s | %-5s | %-5s | %-5s",
              "Round", "Order?", "Rows?", "Cols?", "Score"),
      paste0(rep("-", 50), collapse = ""),
      vapply(rv$answers, function(a) {
        sprintf("%-5d | %-7s | %-5s | %-5s | %d/%d",
                a$round,
                ifelse(a$order_correct, "Yes", "No"),
                ifelse(a$row_correct, "Yes", "No"),
                ifelse(a$col_correct, "Yes", "No"),
                a$pts, MAX_PER_ROUND)
      }, character(1)),
      ""
    )
    report_text <- paste(report_lines, collapse = "\n")

    tags$div(
      class = "report-card",
      tags$div(class = "report-header",
               bsicons::bs_icon("trophy-fill"), "  Game Complete!"),
      tags$div(
        class = "report-body",
        tags$div(class = "meta-line",
                 tags$strong("Player: "), pname),
        tags$div(class = "meta-line",
                 tags$strong("Date: "), ts),

        tags$div(class = "final-score",
                 paste0(score, " / ", MAX_TOTAL)),
        tags$div(class = "final-pct", paste0(pct, "%")),

        # breakdown table
        tags$table(
          class = "breakdown-table",
          tags$thead(
            tags$tr(
              tags$th("Round"),
              tags$th("Goal"),
              tags$th("Order"),
              tags$th("Rows"),
              tags$th("Cols"),
              tags$th("Score")
            )
          ),
          tags$tbody(rows)
        ),

        # hash code
        tags$div(
          tags$strong("Verification Hash: "),
          tags$span(class = "hash-code", toupper(hash_code))
        ),

        # copy button
        tags$button(
          id = "copy_report_btn",
          class = "btn btn-outline-primary btn-sm",
          style = "margin-top:10px;",
          onclick = "copyGameReport()",
          icon("clipboard"), " Copy Report to Clipboard"
        ),
        tags$span(id = "copy_confirm",
                  style = "margin-left:8px; color:#16a34a; font-size:0.85rem; display:none;",
                  bsicons::bs_icon("check-lg"), " Copied!"),

        # reflection
        tags$div(
          class = "reflection-box",
          tags$label(
            "for" = "reflection_input",
            style = "display:block; margin-bottom:8px; font-size:0.9rem;",
            tags$strong("Ignatian Reflection: "),
            "How does careful data preparation reflect care for the people represented in that data?"
          ),
          tags$textarea(
            id = "reflection_input",
            class = "form-control",
            rows = "3",
            placeholder = "Write 1-2 sentences here...",
            style = "font-size:0.88rem; resize:vertical;"
          )
        ),

        # play again
        tags$div(
          style = "text-align:center; margin-top:18px;",
          actionButton("play_again", "Play Again",
                       icon  = icon("rotate-right"),
                       class = "btn-primary",
                       style = "font-weight:600; padding:8px 28px;")
        )
      ),

      # hidden textarea for clipboard copy
      tags$textarea(
        id    = "report_text_store",
        style = "display:none;",
        report_text
      )
    )
  }

  # ---- play again ----------------------------------------------------------
  observeEvent(input$play_again, {
    rv$round         <- 1L
    rv$score         <- 0L
    rv$answers       <- list()
    rv$start_time    <- Sys.time()
    rv$game_active   <- TRUE
    rv$game_complete <- FALSE
    setup_round()
  })
}

shinyApp(ui, server)
