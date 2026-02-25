# ============================================================================
# Static or Interactive?  --  Week 06 Game
# "Not everything needs a tooltip."
#
# Players read real-world scenarios (audience + medium + purpose + data
# complexity) and decide whether a STATIC or INTERACTIVE visualisation
# is the better fit, then justify their reasoning.
# ============================================================================

library(shiny)
library(bslib)
library(shinyjs)

# ---- Gonzaga palette --------------------------------------------------------
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ---- helpers ----------------------------------------------------------------
with_spinner <- function(ui_element) {
  if (requireNamespace("shinycssloaders", quietly = TRUE)) {
    shinycssloaders::withSpinner(ui_element, color = NAVY, type = 6)
  } else {
    ui_element
  }
}

TOTAL_ROUNDS   <- 10
PTS_CHOICE     <- 10
PTS_REASONING  <- 5
PTS_PER_ROUND  <- PTS_CHOICE + PTS_REASONING
MAX_SCORE      <- TOTAL_ROUNDS * PTS_PER_ROUND   # 150

# ---- Scenario data ----------------------------------------------------------
scenarios <- list(

  list(
    id          = 1,
    title       = "CEO Board Deck (Printed PDF)",
    audience    = "C-suite executives, 10 people in a boardroom",
    medium      = "Printed PDF handout",
    purpose     = "Quarterly performance summary",
    data        = "4 KPIs across 4 quarters",
    answer      = "Static",
    ambiguous   = FALSE,
    reasons     = c(
      "Print medium cannot support interactivity",
      "Small, focused dataset needs no exploration",
      "Executives prefer interactive tools",
      "More data requires more interaction"
    ),
    correct_reasons = c(1L, 2L),
    explanation = "A printed PDF physically cannot support hover, filter, or click. With only 4 KPIs over 4 quarters, the dataset is small enough to absorb at a glance. Static is the clear winner here.",
    wrong_explanation = "Interactive dashboards require a digital medium. A printed handout cannot support tooltips, filters, or drill-down. Furthermore, 16 data points (4 KPIs x 4 quarters) are easily displayed in a single, well-designed static chart."
  ),

  list(
    id          = 2,
    title       = "Customer Support Dashboard (Daily Use)",
    audience    = "Support team managers, used daily",
    medium      = "Web browser, desktop",
    purpose     = "Monitor real-time ticket volume, filter by agent/category",
    data        = "50K+ tickets, 15 categories, 20 agents",
    answer      = "Interactive",
    ambiguous   = FALSE,
    reasons     = c(
      "Users need to filter and drill down into large datasets",
      "Repeated daily use justifies interaction investment",
      "Web browsers always need interactivity",
      "Managers prefer colorful dashboards"
    ),
    correct_reasons = c(1L, 2L),
    explanation = "With 50K+ tickets across 15 categories and 20 agents, no single static view can serve every manager's needs. The daily use pattern means users will learn the interface, making the interaction investment worthwhile.",
    wrong_explanation = "This scenario has three strong signals for interactivity: large multi-dimensional data (50K+ tickets, 15 categories, 20 agents), daily repeated use (justifying the learning curve), and a web medium that supports full interaction."
  ),

  list(
    id          = 3,
    title       = "Academic Journal Figure",
    audience    = "Peer reviewers and researchers",
    medium      = "PDF article",
    purpose     = "Show regression results",
    data        = "Scatterplot with 200 points and trend line",
    answer      = "Static",
    ambiguous   = FALSE,
    reasons     = c(
      "Academic publishing requires reproducible static figures",
      "Simple relationship needs no exploration",
      "Researchers need to zoom into individual points",
      "PDF format supports embedded interactive charts"
    ),
    correct_reasons = c(1L, 2L),
    explanation = "Academic journals are published as PDFs -- a static medium. The figure shows a single clear relationship (scatterplot + trend line). Reproducibility and archival stability are paramount in research publishing.",
    wrong_explanation = "Academic journals are published as static PDFs. A scatterplot with 200 points showing a single regression relationship communicates the finding clearly without any need for filtering or drill-down."
  ),

  list(
    id          = 4,
    title       = "Sales Territory Explorer",
    audience    = "Regional sales directors, 8 people",
    medium      = "Shared web link, laptop/tablet",
    purpose     = "Explore performance across 200 accounts by region, product, time",
    data        = "200 accounts x 12 months x 5 products",
    answer      = "Interactive",
    ambiguous   = FALSE,
    reasons     = c(
      "Multi-dimensional data benefits from filtering and selection",
      "Each director needs to focus on their own region",
      "Web links always require interactive charts",
      "Small groups always need interactive tools"
    ),
    correct_reasons = c(1L, 2L),
    explanation = "The data is multi-dimensional (200 accounts x 12 months x 5 products = 12,000 data points) and each director has a different 'slice' they care about. Interactivity lets each user filter to their region without needing 8 separate static reports.",
    wrong_explanation = "With 200 accounts across 12 months and 5 products, the data has too many dimensions for a single static view to serve 8 directors with different regional focuses. Each director needs to filter to their own territory."
  ),

  list(
    id          = 5,
    title       = "Newspaper Infographic",
    audience    = "General public, newspaper readers",
    medium      = "Print newspaper",
    purpose     = "Show election results by state",
    data        = "50 states, vote counts",
    answer      = "Static",
    ambiguous   = FALSE,
    reasons     = c(
      "Print medium cannot support interactivity",
      "Broad audience expects quick visual consumption",
      "Election data is too complex for static displays",
      "Newspapers always use interactive graphics now"
    ),
    correct_reasons = c(1L, 2L),
    explanation = "A print newspaper is a static medium -- no clicks, no hovers. The broad general audience expects to absorb election results at a glance. A well-designed static map or chart communicates the story immediately.",
    wrong_explanation = "Print newspapers are a static medium that cannot support any form of interactivity. Additionally, a general newspaper audience expects to absorb the key story at a glance, not explore data at their own pace."
  ),

  list(
    id          = 6,
    title       = "Student Grade Explorer",
    audience    = "Individual students checking their own grades",
    medium      = "University web portal",
    purpose     = "View grade breakdown across assignments",
    data        = "~20 assignments per student",
    answer      = "Both",
    ambiguous   = TRUE,
    reasons     = c(
      "Small dataset is easily shown in a static table",
      "Students benefit from filtering by assignment type",
      "Web medium supports either approach",
      "Personal data exploration benefits from interactivity"
    ),
    correct_reasons = c(1L, 2L, 3L, 4L),
    explanation = "This one is deliberately debatable! Static works because ~20 assignments is a small dataset easily displayed in a simple table or bar chart. Interactive works because students may want to filter by assignment type, see trends over time, or compare to class averages -- and the web medium supports it. What matters is your reasoning.",
    wrong_explanation = "This one is deliberately debatable! Both static and interactive have genuine merit here."
  ),

  list(
    id          = 7,
    title       = "Real-Time IoT Monitoring",
    audience    = "Factory floor engineers",
    medium      = "Large wall-mounted screen + mobile app",
    purpose     = "Monitor sensor readings, trigger alerts",
    data        = "500 sensors, streaming every 5 seconds",
    answer      = "Interactive",
    ambiguous   = FALSE,
    reasons     = c(
      "Streaming data requires dynamic, updating displays",
      "Users need to drill into specific sensors when alerts trigger",
      "Wall-mounted screens cannot be interactive",
      "Engineers prefer simple paper printouts"
    ),
    correct_reasons = c(1L, 2L),
    explanation = "Data streaming every 5 seconds from 500 sensors demands a dynamic, updating display. When an alert fires, engineers need to drill into the specific sensor, view its history, and compare against thresholds -- all of which require interactivity.",
    wrong_explanation = "With 500 sensors streaming data every 5 seconds, the display must update dynamically. When alerts trigger, engineers need to drill into specific sensors to diagnose issues -- a static snapshot would be immediately outdated."
  ),

  list(
    id          = 8,
    title       = "Annual Report Cover Visualization",
    audience    = "Shareholders, general public",
    medium      = "Printed annual report + PDF",
    purpose     = "One memorable visualization summarizing the year",
    data        = "3-5 key metrics",
    answer      = "Static",
    ambiguous   = FALSE,
    reasons     = c(
      "Print medium, minimal data, narrative impact over exploration",
      "A single powerful image communicates more than a dashboard",
      "Shareholders need interactive tools to understand finances",
      "Annual reports should always include embedded dashboards"
    ),
    correct_reasons = c(1L, 2L),
    explanation = "The goal is a single memorable image for a printed medium with only 3-5 metrics. This is about narrative impact, not data exploration. A beautifully crafted static visualisation will be far more powerful than a dashboard here.",
    wrong_explanation = "This is a printed medium with minimal data (3-5 metrics) where the goal is one memorable image. Narrative impact and visual storytelling trump data exploration. A static visualisation is clearly the right fit."
  ),

  list(
    id          = 9,
    title       = "Clinical Trial Results Dashboard",
    audience    = "Regulatory review board + internal scientists",
    medium      = "Web presentation during review meeting",
    purpose     = "Show drug efficacy across patient subgroups",
    data        = "2,000 patients, 10 subgroups, multiple endpoints",
    answer      = "Both",
    ambiguous   = TRUE,
    reasons     = c(
      "Regulatory bodies require fixed, reproducible figures",
      "Scientists need to explore subgroup effects",
      "Large multi-dimensional data benefits from filtering",
      "Formal presentations benefit from controlled, static visuals"
    ),
    correct_reasons = c(1L, 2L, 3L, 4L),
    explanation = "This one is deliberately debatable! Static is justified because regulatory filings demand reproducible, fixed figures and formal presentations benefit from controlled visuals. Interactive is justified because scientists need to explore subgroup effects across 2,000 patients and multiple endpoints. In practice, many teams use both: static for regulatory submission, interactive for internal scientific review.",
    wrong_explanation = "This one is deliberately debatable! Both static and interactive have genuine merit here."
  ),

  list(
    id          = 10,
    title       = "E-commerce Product Catalog Analytics",
    audience    = "Marketing team, weekly meeting",
    medium      = "Projected screen during meeting, shared as web link after",
    purpose     = "Identify underperforming products, explore trends",
    data        = "5,000 products x 12 months x 5 metrics",
    answer      = "Interactive",
    ambiguous   = FALSE,
    reasons     = c(
      "Large multi-dimensional dataset requires filtering",
      "Post-meeting exploration needs interactivity for follow-up analysis",
      "Meetings always require projected static slides",
      "Marketing teams dislike interactive tools"
    ),
    correct_reasons = c(1L, 2L),
    explanation = "With 5,000 products across 12 months and 5 metrics (300,000 data points!), no single static view can show everything. The dual use -- projected in meetings and shared as a web link after -- means interactivity serves both contexts: guided exploration during the meeting and self-serve analysis afterward.",
    wrong_explanation = "The dataset is massive (5,000 x 12 x 5 = 300,000 data points) and multi-dimensional. Additionally, sharing as a web link after the meeting means team members will want to explore on their own, which demands interactivity."
  )
)

# ---- CSS --------------------------------------------------------------------
game_css <- "
/* ---------- general ---------- */
body { background: #f8fafc; }

/* ---------- sidebar ---------- */
.instruction-list {
  font-size: 0.82rem;
  padding-left: 1.1rem;
  margin-bottom: 0;
  color: #475569;
  line-height: 1.6;
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

/* ---------- welcome card ---------- */
.welcome-card {
  max-width: 660px;
  margin: 50px auto;
  text-align: center;
  padding: 44px 40px;
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
  font-size: 1.08rem;
  margin-bottom: 18px;
}
.welcome-card p { color: #475569; font-size: 0.95rem; line-height: 1.6; }

/* ---------- scenario card ---------- */
.scenario-card {
  max-width: 800px;
  margin: 16px auto 0;
  background: white;
  border-radius: 14px;
  border: 1px solid #e2e8f0;
  box-shadow: 0 4px 20px rgba(0,41,103,0.06);
  overflow: hidden;
  animation: fadeSlideIn 0.35s ease;
}
.scenario-header {
  background: #002967;
  color: white;
  padding: 16px 24px;
  font-size: 1.2rem;
  font-weight: 700;
  letter-spacing: 0.3px;
}
.scenario-header .scenario-number {
  background: rgba(255,255,255,0.18);
  padding: 3px 12px;
  border-radius: 12px;
  font-size: 0.85rem;
  font-weight: 600;
  margin-right: 10px;
}
.scenario-body { padding: 24px 28px 20px; }

.scenario-detail {
  display: flex;
  align-items: flex-start;
  margin-bottom: 12px;
  font-size: 0.93rem;
  line-height: 1.5;
}
.scenario-detail .detail-icon {
  flex-shrink: 0;
  width: 34px;
  height: 34px;
  border-radius: 8px;
  display: flex;
  align-items: center;
  justify-content: center;
  margin-right: 14px;
  font-size: 1rem;
  color: white;
}
.detail-icon.audience { background: #002967; }
.detail-icon.medium   { background: #C41E3A; }
.detail-icon.purpose  { background: #B4975A; }
.detail-icon.data     { background: #059669; }

.detail-label {
  font-weight: 700;
  color: #002967;
  min-width: 72px;
  margin-right: 6px;
}
.detail-value { color: #475569; }

/* ---------- choice buttons ---------- */
.choice-area {
  max-width: 800px;
  margin: 20px auto 0;
  animation: fadeSlideIn 0.35s ease;
}

.choice-buttons {
  display: flex;
  gap: 20px;
  justify-content: center;
  padding: 0 20px;
}

.choice-btn {
  flex: 1;
  max-width: 360px;
  padding: 28px 20px;
  border: 3px solid #e2e8f0;
  border-radius: 14px;
  background: white;
  cursor: pointer;
  transition: all 0.25s ease;
  text-align: center;
  position: relative;
  overflow: hidden;
}
.choice-btn:hover {
  transform: translateY(-3px);
  box-shadow: 0 8px 24px rgba(0,41,103,0.12);
}
.choice-btn:active {
  transform: translateY(0px);
}

.choice-btn .choice-icon {
  font-size: 2.5rem;
  margin-bottom: 10px;
  display: block;
}
.choice-btn .choice-label {
  font-size: 1.35rem;
  font-weight: 800;
  display: block;
  margin-bottom: 4px;
}
.choice-btn .choice-hint {
  font-size: 0.82rem;
  color: #94a3b8;
}

.choice-btn.btn-static {
  border-color: #d1d5db;
}
.choice-btn.btn-static:hover {
  border-color: #002967;
  background: #f0f4f8;
}
.choice-btn.btn-static .choice-label { color: #002967; }

.choice-btn.btn-interactive {
  border-color: #d1d5db;
}
.choice-btn.btn-interactive:hover {
  border-color: #C41E3A;
  background: #fef7f7;
}
.choice-btn.btn-interactive .choice-label { color: #C41E3A; }

.choice-btn.selected-static {
  border-color: #002967 !important;
  background: #f0f4f8 !important;
  box-shadow: 0 0 0 3px rgba(0,41,103,0.2) !important;
}
.choice-btn.selected-interactive {
  border-color: #C41E3A !important;
  background: #fef7f7 !important;
  box-shadow: 0 0 0 3px rgba(196,30,58,0.2) !important;
}

.choice-btn.disabled-choice {
  pointer-events: none;
  opacity: 0.7;
}

/* ---------- reasoning section ---------- */
.reasoning-area {
  max-width: 800px;
  margin: 20px auto 0;
  background: white;
  border-radius: 14px;
  border: 1px solid #e2e8f0;
  padding: 24px 28px;
  box-shadow: 0 2px 12px rgba(0,41,103,0.04);
  animation: fadeSlideIn 0.35s ease;
}
.reasoning-area h5 {
  color: #002967;
  font-weight: 700;
  margin-bottom: 14px;
  font-size: 1rem;
}
.reasoning-area .form-check {
  padding: 10px 14px 10px 38px;
  margin-bottom: 6px;
  border-radius: 8px;
  border: 2px solid #f1f5f9;
  transition: all 0.2s ease;
}
.reasoning-area .form-check:hover {
  border-color: #B4975A;
  background: #fffbf0;
}
.reasoning-area .form-check-input:checked ~ .form-check-label {
  font-weight: 600;
  color: #002967;
}
.reasoning-area .form-check label {
  font-size: 0.9rem;
  color: #475569;
  cursor: pointer;
}

#submit_reasoning {
  margin-top: 14px;
  font-weight: 600;
  padding: 10px 32px;
}

/* ---------- feedback card ---------- */
.feedback-card {
  max-width: 800px;
  margin: 20px auto 0;
  border-radius: 14px;
  overflow: hidden;
  animation: fadeSlideIn 0.35s ease;
  box-shadow: 0 4px 20px rgba(0,0,0,0.06);
}
.feedback-card.feedback-green  { border: 2px solid #16a34a; }
.feedback-card.feedback-yellow { border: 2px solid #eab308; }
.feedback-card.feedback-red    { border: 2px solid #dc2626; }

.feedback-card .feedback-header {
  padding: 14px 22px;
  font-weight: 700;
  font-size: 1.05rem;
  display: flex;
  align-items: center;
  gap: 10px;
}
.feedback-green .feedback-header  { background: #f0fdf4; color: #166534; }
.feedback-yellow .feedback-header { background: #fefce8; color: #854d0e; }
.feedback-red .feedback-header    { background: #fef2f2; color: #991b1b; }

.feedback-card .feedback-body {
  padding: 18px 22px;
  background: white;
  font-size: 0.92rem;
  color: #475569;
  line-height: 1.6;
}

.feedback-card .points-badge {
  display: inline-block;
  padding: 3px 14px;
  border-radius: 12px;
  font-weight: 700;
  font-size: 0.88rem;
  margin-left: auto;
}
.feedback-green .points-badge  { background: #dcfce7; color: #166534; }
.feedback-yellow .points-badge { background: #fef9c3; color: #854d0e; }
.feedback-red .points-badge    { background: #fee2e2; color: #991b1b; }

.next-round-area {
  max-width: 800px;
  margin: 18px auto 0;
  text-align: center;
}
#next_round {
  font-weight: 600;
  padding: 10px 36px;
  font-size: 1rem;
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
.breakdown-table .result-partial { color: #eab308; font-weight: 700; }
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

#copy_report { margin-top: 6px; }

/* ---------- animation ---------- */
@keyframes fadeSlideIn {
  from { opacity: 0; transform: translateY(-10px); }
  to   { opacity: 1; transform: translateY(0); }
}

/* ---------- footer ---------- */
.game-footer {
  text-align: center;
  padding: 14px 0;
  font-size: 0.78rem;
  color: #94a3b8;
  border-top: 1px solid #e2e8f0;
  margin-top: 24px;
}
"

# ---- UI ---------------------------------------------------------------------
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
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"
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

      // Update progress bar
      Shiny.addCustomMessageHandler('updateBar', function(msg) {
        var bar = document.getElementById('round_bar');
        if (bar) bar.style.width = msg.pct + '%';
      });
    "))
  ),

  sidebar = sidebar(
    width = 280,
    bg    = "#f1f5f9",

    # ---- instructions accordion ----
    accordion(
      id   = "help_acc",
      open = FALSE,
      accordion_panel(
        title = "How to Play",
        icon  = bsicons::bs_icon("question-circle"),
        tags$ol(
          class = "instruction-list",
          tags$li("Enter your name and press ", tags$strong("Start Game"), "."),
          tags$li("Read the scenario: audience, medium, purpose, and data."),
          tags$li("Choose: ", tags$strong("Static"), " or ",
                  tags$strong("Interactive"), " visualisation."),
          tags$li("Select your ", tags$strong("reasoning"), " from the options."),
          tags$li("See feedback and learn the framework."),
          tags$li("10 scenarios, up to 15 points each. Max score: 150."),
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

    tags$div(
      style = "text-align:center; margin-top:8px;",
      tags$small("Score", style = "color:#64748b; font-weight:600;"),
      tags$br(),
      tags$span(class = "score-chip", textOutput("score_display", inline = TRUE))
    ),

    hr(style = "margin:10px 0;"),

    # ---- decision framework card ----
    tags$div(
      style = "background:white; border:1px solid #e2e8f0; border-radius:8px; padding:12px 14px; margin-top:4px;",
      tags$div(
        style = "font-weight:700; font-size:0.85rem; color:#002967; margin-bottom:8px;",
        bsicons::bs_icon("lightbulb"), " Decision Framework"
      ),
      tags$div(
        style = "font-size:0.78rem; color:#475569; line-height:1.5;",
        tags$div(style = "margin-bottom:4px;",
          tags$span(style = "font-weight:600; color:#002967;", "Ask yourself:"),
        ),
        tags$div(style = "padding-left:4px;",
          tags$div("1. Can the ", tags$strong("medium"), " support interaction?"),
          tags$div("2. Is the ", tags$strong("data"), " too complex for one view?"),
          tags$div("3. Do users need to ", tags$strong("explore"), " or just ", tags$strong("consume"), "?"),
          tags$div("4. Is ", tags$strong("repeated use"), " expected?")
        )
      )
    )
  ),

  # ==== MAIN PANEL ============================================================
  useShinyjs(),
  uiOutput("main_ui"),

  tags$footer(
    class = "game-footer",
    "Gonzaga University | Dr. Vivek H. Patil"
  )
)

# ---- SERVER ------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- reactive state --------------------------------------------------------
  rv <- reactiveValues(
    round          = 0L,
    score          = 0L,
    answers        = list(),
    start_time     = NULL,
    player_name    = "",
    game_active    = FALSE,
    game_complete  = FALSE,
    # per-round state
    scenario_order = integer(0),
    current_scenario = NULL,
    choice_made    = NULL,      # "Static" or "Interactive"
    reasoning_shown = FALSE,
    feedback_shown  = FALSE,
    round_pts      = 0L
  )

  # ---- start game ------------------------------------------------------------
  observeEvent(input$start_btn, {
    req(nchar(trimws(input$player_name)) > 0)
    rv$player_name    <- trimws(input$player_name)
    rv$round          <- 1L
    rv$score          <- 0L
    rv$answers        <- list()
    rv$start_time     <- Sys.time()
    rv$game_active    <- TRUE
    rv$game_complete  <- FALSE
    rv$scenario_order <- sample(seq_along(scenarios))
    setup_round()
  })

  setup_round <- function() {
    idx <- rv$scenario_order[rv$round]
    rv$current_scenario <- scenarios[[idx]]
    rv$choice_made      <- NULL
    rv$reasoning_shown  <- FALSE
    rv$feedback_shown   <- FALSE
    rv$round_pts        <- 0L
  }

  # ---- progress outputs ------------------------------------------------------
  output$progress_text <- renderText({
    if (!rv$game_active && !rv$game_complete) return("Scenario -- / --")
    paste0("Scenario ", min(rv$round, TOTAL_ROUNDS), " / ", TOTAL_ROUNDS)
  })

  output$score_display <- renderText({
    paste0(rv$score, " / ", MAX_SCORE)
  })

  observe({
    pct <- if (rv$game_active) round(rv$round / TOTAL_ROUNDS * 100) else 0
    if (rv$game_complete) pct <- 100
    session$sendCustomMessage("updateBar", list(pct = pct))
  })

  # ---- main UI router --------------------------------------------------------
  output$main_ui <- renderUI({
    if (rv$game_complete) return(build_report_ui())
    if (!rv$game_active)  return(build_welcome_ui())
    build_round_ui()
  })

  # ---- welcome screen --------------------------------------------------------
  build_welcome_ui <- function() {
    tags$div(
      class = "welcome-card",
      tags$h2("Static or Interactive?"),
      tags$div(class = "tagline", "Not everything needs a tooltip."),
      tags$hr(style = "border-color:#e2e8f0;"),
      tags$p(
        "Interactive dashboards impress, but sometimes a simple, clear chart ",
        "communicates more effectively. The right choice depends on ",
        tags$strong("who"), " sees it, ", tags$strong("where"), " they see it, ",
        tags$strong("why"), " they need it, and ", tags$strong("how much"),
        " data is involved."
      ),
      tags$p(
        "You will see 10 real-world scenarios. For each one, decide whether the ",
        "visualisation should be ", tags$strong("Static"), " or ",
        tags$strong("Interactive"), ", then explain your reasoning."
      ),
      tags$p(
        style = "color:#94a3b8; font-size:0.85rem; margin-top:18px;",
        "Enter your name in the sidebar and press ", tags$strong("Start Game"), "."
      )
    )
  }

  # ---- round UI build --------------------------------------------------------
  build_round_ui <- function() {
    sc <- rv$current_scenario

    # icon mapping
    icons <- list(
      audience = "bi-people-fill",
      medium   = "bi-display",
      purpose  = "bi-bullseye",
      data     = "bi-database"
    )

    scenario_card <- tags$div(
      class = "scenario-card",
      tags$div(
        class = "scenario-header",
        tags$span(class = "scenario-number",
                  paste0(rv$round, " / ", TOTAL_ROUNDS)),
        sc$title
      ),
      tags$div(
        class = "scenario-body",

        tags$div(class = "scenario-detail",
          tags$div(class = "detail-icon audience",
                   tags$i(class = icons$audience)),
          tags$span(class = "detail-label", "Audience:"),
          tags$span(class = "detail-value", sc$audience)
        ),

        tags$div(class = "scenario-detail",
          tags$div(class = "detail-icon medium",
                   tags$i(class = icons$medium)),
          tags$span(class = "detail-label", "Medium:"),
          tags$span(class = "detail-value", sc$medium)
        ),

        tags$div(class = "scenario-detail",
          tags$div(class = "detail-icon purpose",
                   tags$i(class = icons$purpose)),
          tags$span(class = "detail-label", "Purpose:"),
          tags$span(class = "detail-value", sc$purpose)
        ),

        tags$div(class = "scenario-detail",
          tags$div(class = "detail-icon data",
                   tags$i(class = icons$data)),
          tags$span(class = "detail-label", "Data:"),
          tags$span(class = "detail-value", sc$data)
        )
      )
    )

    # Choice buttons (hidden once choice is made)
    choice_ui <- NULL
    if (is.null(rv$choice_made)) {
      choice_ui <- tags$div(
        class = "choice-area",
        tags$div(
          style = "text-align:center; margin-bottom:12px; color:#64748b; font-weight:600; font-size:0.9rem;",
          "What type of visualisation fits this scenario?"
        ),
        tags$div(
          class = "choice-buttons",
          tags$div(
            id    = "btn_static",
            class = "choice-btn btn-static",
            onclick = "Shiny.setInputValue('choice_click', 'Static', {priority: 'event'});",
            tags$span(class = "choice-icon", tags$i(class = "bi bi-image")),
            tags$span(class = "choice-label", "STATIC"),
            tags$span(class = "choice-hint", "Fixed image, print-ready, no interaction")
          ),
          tags$div(
            id    = "btn_interactive",
            class = "choice-btn btn-interactive",
            onclick = "Shiny.setInputValue('choice_click', 'Interactive', {priority: 'event'});",
            tags$span(class = "choice-icon", tags$i(class = "bi bi-hand-index-thumb")),
            tags$span(class = "choice-label", "INTERACTIVE"),
            tags$span(class = "choice-hint", "Filter, hover, drill-down, explore")
          )
        )
      )
    }

    # After choice: show reasoning radio buttons
    reasoning_ui <- NULL
    if (!is.null(rv$choice_made) && !rv$feedback_shown) {
      reasoning_ui <- tags$div(
        class = "reasoning-area",
        id    = "reasoning_panel",
        tags$h5(
          bsicons::bs_icon("chat-left-quote"),
          paste0("  You chose: ", rv$choice_made, ". Why?")
        ),
        radioButtons(
          "reasoning_choice",
          label    = NULL,
          choices  = setNames(as.character(1:4), sc$reasons),
          selected = character(0),
          width    = "100%"
        ),
        tags$div(
          style = "text-align:center;",
          actionButton("submit_reasoning", "Submit Reasoning",
                       icon  = icon("check"),
                       class = "btn-primary")
        )
      )
    }

    # Feedback card
    feedback_ui <- NULL
    if (rv$feedback_shown) {
      feedback_ui <- build_feedback_ui()
    }

    # Next button
    next_ui <- NULL
    if (rv$feedback_shown) {
      btn_label <- if (rv$round >= TOTAL_ROUNDS) "See Results" else "Next Scenario"
      btn_icon  <- if (rv$round >= TOTAL_ROUNDS) icon("flag-checkered") else icon("arrow-right")
      next_ui <- tags$div(
        class = "next-round-area",
        actionButton("next_round", btn_label,
                     icon  = btn_icon,
                     class = "btn-primary")
      )
    }

    tagList(scenario_card, choice_ui, reasoning_ui, feedback_ui, next_ui)
  }

  # ---- handle choice click ---------------------------------------------------
  observeEvent(input$choice_click, {
    req(is.null(rv$choice_made), rv$game_active)
    rv$choice_made     <- input$choice_click
    rv$reasoning_shown <- TRUE
  })

  # ---- handle reasoning submit -----------------------------------------------
  observeEvent(input$submit_reasoning, {
    req(!rv$feedback_shown, !is.null(rv$choice_made))
    req(input$reasoning_choice)

    sc <- rv$current_scenario
    player_choice   <- rv$choice_made
    reasoning_idx   <- as.integer(input$reasoning_choice)

    # Evaluate choice correctness
    is_ambiguous <- sc$ambiguous
    if (is_ambiguous) {
      choice_correct <- TRUE   # either choice is correct for ambiguous
    } else {
      choice_correct <- (player_choice == sc$answer)
    }

    # Evaluate reasoning correctness
    reasoning_correct <- (reasoning_idx %in% sc$correct_reasons)

    # Calculate points
    pts <- 0L
    if (choice_correct) pts <- pts + PTS_CHOICE
    if (reasoning_correct) pts <- pts + PTS_REASONING

    rv$round_pts <- pts
    rv$score     <- rv$score + pts

    # Determine feedback type
    if (choice_correct && reasoning_correct) {
      feedback_type <- "green"
    } else if (choice_correct && !reasoning_correct) {
      feedback_type <- "yellow"
    } else {
      feedback_type <- "red"
    }

    # Record answer
    rv$answers[[rv$round]] <- list(
      round           = rv$round,
      scenario_title  = sc$title,
      correct_answer  = sc$answer,
      player_choice   = player_choice,
      ambiguous       = is_ambiguous,
      choice_correct  = choice_correct,
      reasoning_idx   = reasoning_idx,
      reasoning_text  = sc$reasons[reasoning_idx],
      reasoning_correct = reasoning_correct,
      points          = pts,
      feedback_type   = feedback_type
    )

    rv$feedback_shown <- TRUE
  })

  # ---- build feedback UI -----------------------------------------------------
  build_feedback_ui <- function() {
    ans <- rv$answers[[rv$round]]
    sc  <- rv$current_scenario

    fb_class <- paste0("feedback-card feedback-", ans$feedback_type)

    if (ans$feedback_type == "green") {
      header_text <- "Correct choice + correct reasoning!"
      header_icon <- bsicons::bs_icon("check-circle-fill")
      body_html   <- tags$div(
        tags$p(sc$explanation),
        tags$p(style = "margin-top:8px; font-weight:600; color:#166534;",
               paste0("Your reasoning: \"", ans$reasoning_text, "\""))
      )
    } else if (ans$feedback_type == "yellow") {
      header_text <- "Right choice, but consider your reasoning."
      header_icon <- bsicons::bs_icon("exclamation-triangle-fill")
      # Find a correct reason to suggest
      better_reason <- sc$reasons[sc$correct_reasons[1]]
      body_html <- tags$div(
        tags$p(
          tags$strong("Your reasoning: "), paste0("\"", ans$reasoning_text, "\"")
        ),
        tags$p(style = "margin-top:8px;",
          tags$strong("A stronger reason: "), paste0("\"", better_reason, "\"")
        ),
        tags$p(style = "margin-top:8px;", sc$explanation)
      )
    } else {
      # Red - wrong choice
      header_text <- paste0("The better choice here is: ", sc$answer)
      header_icon <- bsicons::bs_icon("x-circle-fill")

      if (sc$ambiguous) {
        body_html <- tags$div(
          tags$p(sc$explanation)
        )
      } else {
        # Side-by-side comparison
        static_gains <- if (sc$answer == "Static") {
          "Simpler, faster to consume, works in the given medium, clear narrative focus."
        } else {
          "Easier to produce, no technical overhead, but the dataset and use-case demand more."
        }
        interactive_gains <- if (sc$answer == "Interactive") {
          "Supports filtering, drill-down, and exploration for this complex, multi-dimensional data."
        } else {
          "Would add unnecessary complexity; the medium/audience/data do not require it."
        }

        body_html <- tags$div(
          tags$p(sc$wrong_explanation),
          tags$div(
            style = "display:flex; gap:16px; margin-top:14px;",
            tags$div(
              style = "flex:1; background:#f0f4f8; border-radius:8px; padding:12px; border-left:3px solid #002967;",
              tags$div(style = "font-weight:700; color:#002967; margin-bottom:4px;", "Static"),
              tags$div(style = "font-size:0.85rem; color:#475569;", static_gains)
            ),
            tags$div(
              style = "flex:1; background:#fef7f7; border-radius:8px; padding:12px; border-left:3px solid #C41E3A;",
              tags$div(style = "font-weight:700; color:#C41E3A; margin-bottom:4px;", "Interactive"),
              tags$div(style = "font-size:0.85rem; color:#475569;", interactive_gains)
            )
          )
        )
      }
    }

    tags$div(
      class = fb_class,
      tags$div(
        class = "feedback-header",
        header_icon,
        tags$span(header_text),
        tags$span(class = "points-badge",
                  paste0("+", ans$points, " pts"))
      ),
      tags$div(class = "feedback-body", body_html)
    )
  }

  # ---- next round ------------------------------------------------------------
  observeEvent(input$next_round, {
    if (rv$round >= TOTAL_ROUNDS) {
      rv$game_active   <- FALSE
      rv$game_complete <- TRUE
    } else {
      rv$round <- rv$round + 1L
      setup_round()
    }
  })

  # ---- completion report -----------------------------------------------------
  build_report_ui <- function() {
    ts      <- format(Sys.time(), "%B %d, %Y at %I:%M %p")
    pname   <- rv$player_name
    score   <- rv$score
    pct     <- round(score / MAX_SCORE * 100)

    hash_input <- paste(pname, score, ts, sep = "|")
    hash_code  <- substr(digest::digest(hash_input, algo = "md5"), 1, 8)

    # Build breakdown rows
    rows <- lapply(rv$answers, function(a) {
      # Determine result label and class
      if (a$choice_correct && a$reasoning_correct) {
        res_label <- "Full Credit"
        res_class <- "result-correct"
      } else if (a$choice_correct && !a$reasoning_correct) {
        res_label <- "Partial"
        res_class <- "result-partial"
      } else {
        res_label <- "Incorrect"
        res_class <- "result-wrong"
      }
      tags$tr(
        tags$td(a$round),
        tags$td(a$scenario_title),
        tags$td(a$player_choice),
        tags$td(
          if (a$ambiguous) paste0(a$correct_answer, "*") else a$correct_answer
        ),
        tags$td(paste0(a$points, " / ", PTS_PER_ROUND)),
        tags$td(class = res_class, res_label)
      )
    })

    # Plain-text version for clipboard
    report_lines <- c(
      "===== STATIC OR INTERACTIVE? -- COMPLETION REPORT =====",
      paste0("Player:    ", pname),
      paste0("Date:      ", ts),
      paste0("Score:     ", score, " / ", MAX_SCORE, "  (", pct, "%)"),
      paste0("Hash:      ", toupper(hash_code)),
      "",
      sprintf("%-3s | %-42s | %-12s | %-12s | %-5s | %s",
              "#", "Scenario", "Your Choice", "Answer", "Pts", "Result"),
      paste0(rep("-", 100), collapse = ""),
      vapply(rv$answers, function(a) {
        if (a$choice_correct && a$reasoning_correct) {
          res <- "Full Credit"
        } else if (a$choice_correct) {
          res <- "Partial"
        } else {
          res <- "Incorrect"
        }
        ans_display <- if (a$ambiguous) paste0(a$correct_answer, "*") else a$correct_answer
        sprintf("%-3d | %-42s | %-12s | %-12s | %2d/15 | %s",
                a$round, a$scenario_title, a$player_choice,
                ans_display, a$points, res)
      }, character(1)),
      "",
      "* = Ambiguous scenario (both Static and Interactive are acceptable)",
      "",
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
                 paste0(score, " / ", MAX_SCORE)),
        tags$div(class = "final-pct", paste0(pct, "%")),

        # breakdown table
        tags$table(
          class = "breakdown-table",
          tags$thead(
            tags$tr(
              tags$th("#"),
              tags$th("Scenario"),
              tags$th("Your Choice"),
              tags$th("Answer"),
              tags$th("Points"),
              tags$th("Result")
            )
          ),
          tags$tbody(rows)
        ),

        tags$div(
          style = "font-size:0.82rem; color:#94a3b8; margin-bottom:12px;",
          "* = Ambiguous scenario (both Static and Interactive are acceptable)"
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
            "When has simplicity been more powerful than complexity in your own experience with communication?"
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

      # store report text for copying
      tags$textarea(
        id    = "report_text_store",
        style = "display:none;",
        report_text
      )
    )
  }

  # ---- play again ------------------------------------------------------------
  observeEvent(input$play_again, {
    rv$round          <- 1L
    rv$score          <- 0L
    rv$answers        <- list()
    rv$start_time     <- Sys.time()
    rv$game_active    <- TRUE
    rv$game_complete  <- FALSE
    rv$scenario_order <- sample(seq_along(scenarios))
    setup_round()
  })
}

shinyApp(ui, server)
