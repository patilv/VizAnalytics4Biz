# ============================================================================
# Stats Trap  --  Week 01 Game
# "Can you spot the lie hidden in the numbers?"
#
# Players see a dataset NAME (e.g. "dino") and must pick the matching
# scatterplot from a 2x2 grid.  All four plots share nearly identical
# summary statistics -- proof that you should ALWAYS visualise your data.
# ============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(datasauRus)

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

ALL_DATASETS <- c("dino", "star", "away", "bullseye", "circle", "dots",
                   "h_lines", "v_lines", "x_shape", "high_lines",
                   "slant_down", "slant_up", "wide_lines")

TOTAL_ROUNDS <- 10
PTS_PER_Q    <- 10

# Compute shared summary stats once (they are virtually identical)
shared_stats <- datasauRus::datasaurus_dozen |>
  filter(dataset == "dino") |>
  summarise(
    `Mean X`   = round(mean(x), 2),
    `Mean Y`   = round(mean(y), 2),
    `SD X`     = round(sd(x), 2),
    `SD Y`     = round(sd(y), 2),
    `Cor(X,Y)` = round(cor(x, y), 2)
  )

make_game_plot <- function(df, label = NULL, border_color = NA) {
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(color = NAVY, size = 2.2, alpha = 0.72) +
    coord_cartesian(xlim = c(10, 100), ylim = c(-5, 105)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(colour = "#94a3b8", size = 8),
      plot.margin       = margin(8, 8, 8, 8)
    )
  p
}

# ---- CSS -------------------------------------------------------------------
game_css <- "
/* ---------- general ---------- */
body { background: #f8fafc; }

/* ---------- sidebar ---------- */
.sidebar-stats-card {
  background: white;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  padding: 12px 14px;
  margin-top: 8px;
}
.sidebar-stats-card table { width: 100%; font-size: 0.82rem; }
.sidebar-stats-card td { padding: 3px 4px; }
.sidebar-stats-card .stat-label { color: #64748b; font-weight: 500; }
.sidebar-stats-card .stat-value { text-align: right; font-weight: 700; color: #002967; font-family: 'Fira Code', monospace; }

/* ---------- round banner ---------- */
.round-banner {
  text-align: center;
  padding: 18px 12px 12px;
}
.round-banner .target-label {
  font-size: 1.55rem;
  font-weight: 700;
  color: #002967;
  margin-bottom: 2px;
}
.round-banner .target-sub {
  font-size: 0.88rem;
  color: #64748b;
}

/* ---------- plot grid ---------- */
.plot-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 16px;
  padding: 0 12px 12px;
}
.plot-card {
  position: relative;
  background: white;
  border: 3px solid #e2e8f0;
  border-radius: 10px;
  overflow: hidden;
  cursor: pointer;
  transition: border-color 0.2s, box-shadow 0.2s, transform 0.15s;
}
.plot-card:hover {
  border-color: #B4975A;
  box-shadow: 0 4px 16px rgba(0,41,103,0.12);
  transform: translateY(-2px);
}
.plot-card .plot-label {
  position: absolute;
  top: 8px; left: 10px;
  background: #002967;
  color: white;
  font-weight: 700;
  font-size: 0.95rem;
  padding: 2px 12px;
  border-radius: 6px;
  z-index: 5;
  letter-spacing: 0.5px;
}

/* feedback borders */
.plot-card.correct-pick {
  border-color: #16a34a !important;
  box-shadow: 0 0 0 3px rgba(22,163,74,0.25) !important;
}
.plot-card.wrong-pick {
  border-color: #dc2626 !important;
  box-shadow: 0 0 0 3px rgba(220,38,38,0.25) !important;
}
.plot-card.reveal-correct {
  border-color: #16a34a !important;
  box-shadow: 0 0 0 3px rgba(22,163,74,0.35) !important;
}
.plot-card.disabled-card {
  pointer-events: none;
  opacity: 0.85;
}

/* ---------- feedback toast ---------- */
.feedback-toast {
  text-align: center;
  padding: 14px 20px;
  border-radius: 10px;
  font-size: 0.95rem;
  font-weight: 600;
  margin: 8px 12px 4px;
  animation: fadeSlideIn 0.3s ease;
}
.feedback-toast.correct {
  background: #f0fdf4;
  color: #166534;
  border: 1px solid #bbf7d0;
}
.feedback-toast.wrong {
  background: #fef2f2;
  color: #991b1b;
  border: 1px solid #fecaca;
}
@keyframes fadeSlideIn {
  from { opacity: 0; transform: translateY(-8px); }
  to   { opacity: 1; transform: translateY(0); }
}

/* ---------- score chip ---------- */
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
.round-progress {
  margin: 6px 0;
}
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
  max-width: 620px;
  margin: 60px auto;
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

/* ---------- completion report ---------- */
.report-card {
  max-width: 700px;
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
  padding: 20px;
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
  font-size: 0.85rem;
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

#copy_report {
  margin-top: 6px;
}

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
  title = NULL,
  fillable = FALSE,
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),

  tags$head(
    tags$style(HTML(game_css)),
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

  sidebar = sidebar(
    width = 275,
    bg    = "#f1f5f9",

    # ---- instructions accordion ----
    accordion(
      id   = "help_acc",
      open = FALSE,
      accordion_panel(
        title = "How to Play",
        icon  = bsicons::bs_icon("question-circle"),
        tags$ul(
          style = "font-size:0.82rem; padding-left:1.1rem; margin-bottom:0;",
          tags$li("Enter your name and press ", tags$strong("Start Game"), "."),
          tags$li("Each round shows a dataset ", tags$strong("name"),
                  " (e.g. 'dino', 'star')."),
          tags$li("Four scatterplots appear -- ", tags$strong("click"),
                  " the one that matches the name."),
          tags$li("The trick: all four plots share ", tags$em("nearly identical"),
                  " summary statistics!"),
          tags$li("10 rounds, 10 points each. Can you score 100?"),
          tags$li("After all rounds, write a brief ", tags$strong("reflection"),
                  " before copying your completion report.")
        )
      )
    ),

    hr(style = "margin:8px 0;"),

    # ---- player name ----
    textInput("player_name", "Your Name:", placeholder = "Enter your name"),
    actionButton("start_btn", "Start Game",
                 icon  = icon("play"),
                 class = "btn-primary w-100",
                 style = "font-weight:600;"),

    hr(style = "margin:10px 0;"),

    # ---- progress ----
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

    tags$div(style = "text-align:center; margin-top:8px;",
      tags$small("Score", style = "color:#64748b; font-weight:600;"),
      tags$br(),
      tags$span(class = "score-chip", textOutput("score_display", inline = TRUE))
    ),

    hr(style = "margin:10px 0;"),

    # ---- shared stats card ----
    tags$div(
      class = "sidebar-stats-card",
      tags$div(
        style = "font-weight:700; font-size:0.85rem; color:#002967; margin-bottom:6px;",
        bsicons::bs_icon("table"), " Shared Statistics"
      ),
      tags$table(
        tags$tr(tags$td(class = "stat-label", "Mean X"),
                tags$td(class = "stat-value", shared_stats$`Mean X`)),
        tags$tr(tags$td(class = "stat-label", "Mean Y"),
                tags$td(class = "stat-value", shared_stats$`Mean Y`)),
        tags$tr(tags$td(class = "stat-label", "SD X"),
                tags$td(class = "stat-value", shared_stats$`SD X`)),
        tags$tr(tags$td(class = "stat-label", "SD Y"),
                tags$td(class = "stat-value", shared_stats$`SD Y`)),
        tags$tr(tags$td(class = "stat-label", "Cor(X,Y)"),
                tags$td(class = "stat-value", shared_stats$`Cor(X,Y)`))
      ),
      tags$div(
        style = "font-size:0.75rem; color:#94a3b8; margin-top:6px; font-style:italic;",
        "All 13 Datasaurus Dozen datasets share these stats!"
      )
    )
  ),

  # ==== MAIN PANEL ==========================================================
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
    total_rounds  = TOTAL_ROUNDS,
    answers       = list(),        # list of lists: target, picked, correct, choices, positions
    start_time    = NULL,
    player_name   = "",
    game_active   = FALSE,
    game_complete = FALSE,
    # per-round
    round_targets    = character(0),
    current_target   = NULL,
    current_choices  = character(0),  # length-4 vector (A B C D)
    current_position = NA_integer_,   # which slot (1-4) holds the target
    feedback         = NULL,          # NULL / "correct" / "wrong"
    picked_slot      = NA_integer_,
    waiting          = FALSE          # TRUE during 2-s feedback delay
  )

  # ---- helpers: generate rounds -------------------------------------------
  generate_round_targets <- function() {
    sample(ALL_DATASETS, TOTAL_ROUNDS, replace = FALSE)
  }

  setup_round <- function() {
    target   <- rv$round_targets[rv$round]
    others   <- setdiff(ALL_DATASETS, target)
    distractors <- sample(others, 3)
    four     <- sample(c(target, distractors))   # randomised positions
    pos      <- which(four == target)

    rv$current_target   <- target
    rv$current_choices  <- four
    rv$current_position <- pos
    rv$feedback         <- NULL
    rv$picked_slot      <- NA_integer_
    rv$waiting          <- FALSE
  }

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
    rv$round_targets <- generate_round_targets()
    setup_round()
  })

  # ---- progress outputs ----------------------------------------------------
  output$progress_text <- renderText({
    if (!rv$game_active && !rv$game_complete) return("Round -- / --")
    paste0("Round ", min(rv$round, TOTAL_ROUNDS), " / ", TOTAL_ROUNDS)
  })

  output$score_display <- renderText({
    paste0(rv$score)
  })

  # update progress bar
  observe({
    pct <- if (rv$game_active) round(rv$round / TOTAL_ROUNDS * 100) else 0
    if (rv$game_complete) pct <- 100
    session$sendCustomMessage("updateBar", list(pct = pct))
  })

  # JS handler for bar
  observe({
    # inject once
    session$sendCustomMessage("dummyInit", "")
  })

  # Add a JS handler for updating the bar
  observeEvent(TRUE, once = TRUE, {
    insertUI(
      selector = "head", where = "beforeEnd",
      ui = tags$script(HTML("
        Shiny.addCustomMessageHandler('updateBar', function(msg) {
          var bar = document.getElementById('round_bar');
          if (bar) bar.style.width = msg.pct + '%';
        });
      "))
    )
  })

  # ---- main UI router ------------------------------------------------------
  output$main_ui <- renderUI({
    if (rv$game_complete) {
      return(build_report_ui())
    }
    if (!rv$game_active) {
      return(build_welcome_ui())
    }
    build_round_ui()
  })

  # ---- welcome screen ------------------------------------------------------
  build_welcome_ui <- function() {
    tags$div(
      class = "welcome-card",
      tags$h2("Stats Trap"),
      tags$div(class = "tagline", "Can you spot the lie hidden in the numbers?"),
      tags$hr(style = "border-color:#e2e8f0;"),
      tags$p("All 13 Datasaurus Dozen datasets share virtually ",
             tags$strong("identical summary statistics"), " -- yet their scatterplots ",
             "look wildly different."),
      tags$p("Your mission: given a dataset ", tags$strong("name"),
             ", pick the scatterplot that matches it from a lineup of four."),
      tags$p(style = "color:#94a3b8; font-size:0.85rem; margin-top:16px;",
             "Enter your name in the sidebar and press ", tags$strong("Start Game"), ".")
    )
  }

  # ---- round UI ------------------------------------------------------------
  build_round_ui <- function() {
    target  <- rv$current_target
    choices <- rv$current_choices
    labels  <- c("A", "B", "C", "D")

    # build plot cards
    plot_cards <- lapply(seq_along(choices), function(i) {
      slot_id <- paste0("plot_", tolower(labels[i]))
      tags$div(
        class = "plot-card",
        id    = paste0("card_", tolower(labels[i])),
        tags$span(class = "plot-label", labels[i]),
        plotOutput(slot_id, height = "280px",
                   click = paste0(slot_id, "_click"))
      )
    })

    tagList(
      # round banner
      tags$div(
        class = "round-banner",
        tags$div(class = "target-label",
                 bsicons::bs_icon("bullseye"), " Find: ",
                 tags$span(style = "color:#C41E3A;",
                           paste0("\"", target, "\""))),
        tags$div(class = "target-sub",
                 "Which scatterplot matches this dataset name?")
      ),

      # feedback area
      uiOutput("feedback_ui"),

      # 2x2 grid
      tags$div(class = "plot-grid", plot_cards)
    )
  }

  # ---- render the 4 plots --------------------------------------------------
  observe({
    req(rv$game_active, rv$round > 0)
    choices <- rv$current_choices

    lapply(seq_along(choices), function(i) {
      local({
        idx <- i
        ds  <- choices[idx]
        slot_id <- paste0("plot_", c("a","b","c","d")[idx])
        output[[slot_id]] <- renderPlot({
          df <- datasauRus::datasaurus_dozen |> filter(dataset == ds)
          make_game_plot(df)
        }, res = 100, bg = "white")
      })
    })
  })

  # ---- click handlers ------------------------------------------------------
  handle_click <- function(slot) {
    if (rv$waiting || !rv$game_active) return()

    rv$waiting     <- TRUE
    rv$picked_slot <- slot
    is_correct     <- (slot == rv$current_position)

    if (is_correct) {
      rv$score    <- rv$score + PTS_PER_Q
      rv$feedback <- "correct"
    } else {
      rv$feedback <- "wrong"
    }

    # record answer
    rv$answers[[rv$round]] <- list(
      round      = rv$round,
      target     = rv$current_target,
      choices    = rv$current_choices,
      picked     = rv$current_choices[slot],
      correct_ds = rv$current_target,
      result     = ifelse(is_correct, "Correct", "Wrong")
    )

    # apply CSS classes via JS
    labels <- c("a","b","c","d")
    correct_card <- paste0("card_", labels[rv$current_position])
    picked_card  <- paste0("card_", labels[slot])

    # disable all cards
    for (lbl in labels) {
      card_id <- paste0("card_", lbl)
      session$sendCustomMessage("addCssClass",
        list(id = card_id, cls = "disabled-card"))
    }

    # highlight correct
    session$sendCustomMessage("addCssClass",
      list(id = correct_card, cls = "reveal-correct"))

    if (!is_correct) {
      session$sendCustomMessage("addCssClass",
        list(id = picked_card, cls = "wrong-pick"))
    } else {
      session$sendCustomMessage("addCssClass",
        list(id = picked_card, cls = "correct-pick"))
    }

    # advance after delay
    observe({
      invalidateLater(2200, session)
      isolate({
        if (rv$round >= TOTAL_ROUNDS) {
          rv$game_active   <- FALSE
          rv$game_complete <- TRUE
        } else {
          rv$round <- rv$round + 1L
          setup_round()
        }
      })
    }) |> bindEvent(rv$feedback, once = TRUE)
  }

  observeEvent(input$plot_a_click, handle_click(1))
  observeEvent(input$plot_b_click, handle_click(2))
  observeEvent(input$plot_c_click, handle_click(3))
  observeEvent(input$plot_d_click, handle_click(4))

  # CSS-class helper JS (injected once)
  observeEvent(TRUE, once = TRUE, {
    insertUI(
      selector = "head", where = "beforeEnd",
      ui = tags$script(HTML("
        Shiny.addCustomMessageHandler('addCssClass', function(msg) {
          var el = document.getElementById(msg.id);
          if (el) el.classList.add(msg.cls);
        });
        Shiny.addCustomMessageHandler('removeCssClass', function(msg) {
          var el = document.getElementById(msg.id);
          if (el) el.classList.remove(msg.cls);
        });
      "))
    )
  })

  # ---- feedback UI ---------------------------------------------------------
  output$feedback_ui <- renderUI({
    req(rv$feedback)
    if (rv$feedback == "correct") {
      tags$div(
        class = "feedback-toast correct",
        bsicons::bs_icon("check-circle-fill"),
        " Correct!  Despite identical statistics (mean, SD, correlation), ",
        "these datasets look completely different. Always visualize your data!"
      )
    } else {
      target  <- rv$current_target
      picked  <- rv$current_choices[rv$picked_slot]
      tags$div(
        class = "feedback-toast wrong",
        bsicons::bs_icon("x-circle-fill"),
        paste0(" Not quite. You picked \"", picked,
               "\" -- the correct answer was \"", target,
               "\". Same stats, wildly different shapes!")
      )
    }
  })

  # ---- completion report ----------------------------------------------------
  build_report_ui <- function() {
    ts      <- format(Sys.time(), "%B %d, %Y at %I:%M %p")
    pname   <- rv$player_name
    score   <- rv$score
    pct     <- round(score / (TOTAL_ROUNDS * PTS_PER_Q) * 100)

    hash_input <- paste(pname, score, ts, sep = "|")
    hash_code  <- substr(
      digest::digest(hash_input, algo = "md5"), 1, 8
    )

    # Build breakdown rows
    rows <- lapply(rv$answers, function(a) {
      res_class <- if (a$result == "Correct") "result-correct" else "result-wrong"
      tags$tr(
        tags$td(a$round),
        tags$td(paste0("\"", a$target, "\"")),
        tags$td(paste0("\"", a$picked, "\"")),
        tags$td(class = res_class, a$result)
      )
    })

    # Plain-text version for clipboard
    report_lines <- c(
      "===== STATS TRAP -- COMPLETION REPORT =====",
      paste0("Player:    ", pname),
      paste0("Date:      ", ts),
      paste0("Score:     ", score, " / ", TOTAL_ROUNDS * PTS_PER_Q,
             "  (", pct, "%)"),
      paste0("Hash:      ", hash_code),
      "",
      "Round | Target         | Your Pick      | Result",
      paste0(rep("-", 56), collapse = ""),
      vapply(rv$answers, function(a) {
        sprintf("%-5d | %-14s | %-14s | %s",
                a$round, a$target, a$picked, a$result)
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
                 paste0(score, " / ", TOTAL_ROUNDS * PTS_PER_Q)),
        tags$div(class = "final-pct", paste0(pct, "% correct")),

        # breakdown table
        tags$table(
          class = "breakdown-table",
          tags$thead(
            tags$tr(
              tags$th("Round"),
              tags$th("Target"),
              tags$th("Your Pick"),
              tags$th("Result")
            )
          ),
          tags$tbody(rows)
        ),

        # hash code
        tags$div(
          tags$strong("Verification Hash: "),
          tags$span(class = "hash-code", toupper(hash_code))
        ),

        # reflection input
        tags$div(
          class = "reflection-box",
          tags$label(
            "for" = "reflection_input",
            style = "display:block; margin-bottom:8px; font-size:0.9rem;",
            tags$strong("Ignatian Reflection: "),
            "How might hidden patterns in data affect real decisions about people and communities?"
          ),
          tags$textarea(
            id = "reflection_input",
            class = "form-control",
            rows = "3",
            placeholder = "Write 1-2 sentences here...",
            style = "font-size:0.88rem; resize:vertical;"
          )
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

        # play again
        tags$div(
          style = "text-align:center; margin-top:18px;",
          actionButton("play_again", "Play Again",
                       icon  = icon("rotate-right"),
                       class = "btn-primary",
                       style = "font-weight:600; padding:8px 28px;")
        )
      ),

      # store report text for copying
      tags$textarea(
        id    = "report_text_store",
        style = "display:none;",
        report_text
      )
    )
  }

  # play again
  observeEvent(input$play_again, {
    rv$round         <- 1L
    rv$score         <- 0L
    rv$answers       <- list()
    rv$start_time    <- Sys.time()
    rv$game_active   <- TRUE
    rv$game_complete <- FALSE
    rv$round_targets <- generate_round_targets()
    setup_round()
  })
}

shinyApp(ui, server)
