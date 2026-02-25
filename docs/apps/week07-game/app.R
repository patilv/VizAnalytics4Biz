# ============================================================================
# Map ER  --  Week 07 Game
# "Fix the broken map before it misleads the public."
#
# Players see a US choropleth map with one deliberate cartographic error,
# then identify the error from 4 multiple-choice options.  After answering,
# the corrected map is revealed alongside the broken one.
# ============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(maps)

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

# ---- Synthetic state data ---------------------------------------------------
set.seed(42)
state_data <- data.frame(
  region       = tolower(state.name),
  population   = runif(50, 500000, 40000000),
  income       = runif(50, 35000, 85000),
  temp         = runif(50, 30, 85),
  unemployment = runif(50, 2, 12),
  education    = runif(50, 20, 50),
  crime_rate   = runif(50, 100, 800),
  vaccination  = runif(50, 40, 95),
  broadband    = runif(50, 60, 98),
  stringsAsFactors = FALSE
)

us_map      <- map_data("state")
map_merged  <- left_join(us_map, state_data, by = "region")

# State centroids for proportional symbol overlay (Round 8)
state_centroids <- us_map %>%
  group_by(region) %>%
  summarise(
    cent_long  = mean(range(long)),
    cent_lat   = mean(range(lat)),
    .groups    = "drop"
  ) %>%
  left_join(state_data, by = "region")

# ---- Game constants ---------------------------------------------------------
TOTAL_ROUNDS <- 8L
PTS_PER_Q    <- 12L

# ---- Base map theme ---------------------------------------------------------
map_theme <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      axis.text        = element_blank(),
      axis.title       = element_blank(),
      axis.ticks       = element_blank(),
      panel.grid       = element_blank(),
      plot.title       = element_text(face = "bold", size = 14, color = NAVY,
                                      hjust = 0.5, margin = margin(b = 4)),
      plot.subtitle    = element_text(size = 10, color = "#64748b",
                                      hjust = 0.5, margin = margin(b = 8)),
      plot.caption     = element_text(size = 8, color = "#94a3b8",
                                      hjust = 1, margin = margin(t = 6)),
      plot.background  = element_rect(fill = "white", colour = NA),
      legend.position  = "right",
      legend.title     = element_text(size = 9, face = "bold"),
      legend.text      = element_text(size = 8),
      plot.margin      = margin(10, 10, 10, 10)
    )
}

# ---- Round definitions ------------------------------------------------------
# Each round is a list with:
#   title       - short label for the error type
#   question    - question displayed to the player
#   correct     - the correct answer text
#   correct_idx - position of the correct answer (1-4)
#   options     - character vector of length 4
#   explanation - explanation shown after answer
#   broken_plot - function() returning the broken ggplot
#   fixed_plot  - function() returning the fixed ggplot

rounds <- list(

  # ==== Round 1: Wrong palette type (sequential vs. diverging) ===============
  list(
    title       = "Wrong Palette Type",
    question    = "What cartographic error is shown in this map of state population?",
    correct_idx = 2L,
    options     = c(
      "Missing legend",
      "Using a diverging palette for sequential data implies a meaningful midpoint that doesn't exist",
      "Wrong projection",
      "Too few color bins"
    ),
    explanation = "Population is a sequential variable (low to high) with no natural midpoint. A diverging palette (blue-white-red) falsely suggests that the midpoint is meaningful, misleading readers into thinking states near white are 'neutral' when they're just mid-range.",
    broken_plot = function() {
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = population), color = "white", linewidth = 0.2) +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red",
          midpoint = median(state_data$population),
          labels = scales::comma,
          name = "Population"
        ) +
        coord_map("polyconic") +
        labs(title = "U.S. State Population",
             subtitle = "Examine this map carefully") +
        map_theme()
    },
    fixed_plot = function() {
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = population), color = "white", linewidth = 0.2) +
        scale_fill_gradient(
          low = "#deebf7", high = "#08306b",
          labels = scales::comma,
          name = "Population"
        ) +
        coord_map("polyconic") +
        labs(title = "U.S. State Population",
             subtitle = "Sequential palette (light to dark blue)") +
        map_theme()
    }
  ),

  # ==== Round 2: Missing legend ===============================================
  list(
    title       = "Missing Legend",
    question    = "This map shows household income by state. What is wrong?",
    correct_idx = 1L,
    options     = c(
      "Map has no legend \u2014 readers can't decode what the colors mean",
      "Wrong color palette",
      "Missing state borders",
      "Reversed color direction"
    ),
    explanation = "Without a legend, readers have no way to decode the color encoding. Even a beautiful map becomes useless if viewers cannot translate colors back to data values. Always include a legend with clear labels and units.",
    broken_plot = function() {
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = income), color = "white", linewidth = 0.2) +
        scale_fill_viridis_c(labels = scales::dollar, name = "Income") +
        coord_map("polyconic") +
        labs(title = "Median Household Income by State",
             subtitle = "Examine this map carefully") +
        map_theme() +
        theme(legend.position = "none")
    },
    fixed_plot = function() {
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = income), color = "white", linewidth = 0.2) +
        scale_fill_viridis_c(labels = scales::dollar, name = "Median\nIncome") +
        coord_map("polyconic") +
        labs(title = "Median Household Income by State",
             subtitle = "Legend restored with clear dollar labels") +
        map_theme()
    }
  ),

  # ==== Round 3: Rainbow palette on sequential data ===========================
  list(
    title       = "Rainbow Palette",
    question    = "This map shows average temperature by state. Spot the error.",
    correct_idx = 3L,
    options     = c(
      "Too many categories",
      "Missing title",
      "Rainbow colors create false boundaries and aren't perceptually uniform",
      "Aspect ratio distortion"
    ),
    explanation = "Rainbow palettes create artificial visual boundaries where hue changes sharply (e.g., green to yellow), misleading viewers into seeing clusters that don't exist. They also fail for colorblind readers. Perceptually uniform palettes like viridis preserve the data's true gradient.",
    broken_plot = function() {
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = temp), color = "white", linewidth = 0.2) +
        scale_fill_gradientn(
          colours = rainbow(7),
          name = "Avg Temp (\u00B0F)"
        ) +
        coord_map("polyconic") +
        labs(title = "Average Temperature by State",
             subtitle = "Examine this map carefully") +
        map_theme()
    },
    fixed_plot = function() {
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = temp), color = "white", linewidth = 0.2) +
        scale_fill_viridis_c(
          option = "C",
          name = "Avg Temp (\u00B0F)"
        ) +
        coord_map("polyconic") +
        labs(title = "Average Temperature by State",
             subtitle = "Perceptually uniform viridis palette") +
        map_theme()
    }
  ),

  # ==== Round 4: Reversed color direction =====================================
  list(
    title       = "Reversed Color Direction",
    question    = "This choropleth displays unemployment rate. What's misleading?",
    correct_idx = 4L,
    options     = c(
      "Wrong variable mapped",
      "Missing state labels",
      "Inappropriate projection",
      "Color direction is reversed \u2014 darker shades should represent higher values"
    ),
    explanation = "Readers naturally expect darker = more / higher. When the direction is reversed, states with the worst unemployment look lightest and vice versa, completely inverting the intended message. Always follow the convention: darker = higher for sequential data.",
    broken_plot = function() {
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = unemployment), color = "white", linewidth = 0.2) +
        scale_fill_gradient(
          low  = "#67000d",  # dark red for LOW unemployment (misleading!)
          high = "#fee0d2",  # light for HIGH unemployment (misleading!)
          name = "Unemp. Rate (%)"
        ) +
        coord_map("polyconic") +
        labs(title = "Unemployment Rate by State",
             subtitle = "Examine this map carefully") +
        map_theme()
    },
    fixed_plot = function() {
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = unemployment), color = "white", linewidth = 0.2) +
        scale_fill_gradient(
          low  = "#fee0d2",  # light for low unemployment
          high = "#67000d",  # dark for high unemployment
          name = "Unemp. Rate (%)"
        ) +
        coord_map("polyconic") +
        labs(title = "Unemployment Rate by State",
             subtitle = "Darker shades = higher unemployment") +
        map_theme()
    }
  ),

  # ==== Round 5: Missing states (join error) ==================================
  list(
    title       = "Missing States (Join Error)",
    question    = "This map of crime rate has a visible data problem. What is it?",
    correct_idx = 1L,
    options     = c(
      "Several states show no data due to a data join error",
      "Wrong palette choice",
      "Legend is unclear",
      "Color scale is too narrow"
    ),
    explanation = "When state names don't match between the map data and the data table, the join produces NAs and those states appear gray. This is one of the most common mapping errors. Always verify your join and check for unmatched records before mapping.",
    broken_plot = function() {
      # Remove some states to simulate a bad join
      dropped <- c("texas", "california", "new york", "florida",
                    "ohio", "michigan", "georgia", "illinois")
      partial_data <- state_data[!state_data$region %in% dropped, ]
      broken_merged <- left_join(us_map, partial_data, by = "region")
      ggplot(broken_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = crime_rate), color = "white", linewidth = 0.2) +
        scale_fill_gradient(
          low  = "#fef0d9",
          high = "#b30000",
          na.value = "#d4d4d4",
          name = "Crime Rate\n(per 100k)"
        ) +
        coord_map("polyconic") +
        labs(title = "Crime Rate by State",
             subtitle = "Examine this map carefully") +
        map_theme()
    },
    fixed_plot = function() {
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = crime_rate), color = "white", linewidth = 0.2) +
        scale_fill_gradient(
          low  = "#fef0d9",
          high = "#b30000",
          name = "Crime Rate\n(per 100k)"
        ) +
        coord_map("polyconic") +
        labs(title = "Crime Rate by State",
             subtitle = "All states included after correcting the data join") +
        map_theme()
    }
  ),

  # ==== Round 6: Misleading classification breaks =============================
  list(
    title       = "Misleading Classification Breaks",
    question    = "This map shows education level (% with bachelor's degree). What's wrong?",
    correct_idx = 3L,
    options     = c(
      "Too many colors",
      "Missing data labels",
      "Classification breaks hide variation \u2014 most states fall into a single category",
      "Wrong map projection"
    ),
    explanation = "When break points are chosen poorly, nearly all states land in one color bin, making the map appear uniform when real variation exists. Quantile or equal-interval breaks reveal the actual distribution across states.",
    broken_plot = function() {
      # Almost all states in one bin
      map_merged_edu <- map_merged %>%
        mutate(edu_bin = cut(education,
                             breaks = c(0, 48, 50),
                             labels = c("20-48%", "48-50%"),
                             include.lowest = TRUE))
      ggplot(map_merged_edu, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = edu_bin), color = "white", linewidth = 0.2) +
        scale_fill_manual(
          values = c("20-48%" = "#c6dbef", "48-50%" = "#08519c"),
          name = "Bachelor's\nDegree %",
          na.value = "#d4d4d4"
        ) +
        coord_map("polyconic") +
        labs(title = "Education Level by State",
             subtitle = "Examine this map carefully") +
        map_theme()
    },
    fixed_plot = function() {
      # Quantile breaks
      brks <- quantile(state_data$education, probs = seq(0, 1, 0.2))
      map_merged_edu <- map_merged %>%
        mutate(edu_bin = cut(education,
                             breaks = brks,
                             include.lowest = TRUE,
                             dig.lab = 3))
      ggplot(map_merged_edu, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = edu_bin), color = "white", linewidth = 0.2) +
        scale_fill_brewer(
          palette = "Blues",
          name = "Bachelor's\nDegree %"
        ) +
        coord_map("polyconic") +
        labs(title = "Education Level by State",
             subtitle = "Quantile breaks reveal the true variation") +
        map_theme()
    }
  ),

  # ==== Round 7: No title or context ==========================================
  list(
    title       = "No Title or Context",
    question    = "This choropleth has a good palette, but something critical is missing. What?",
    correct_idx = 2L,
    options     = c(
      "Wrong color palette",
      "Map lacks title and context \u2014 viewers can't understand what data is being shown",
      "Too many categories",
      "Data is outdated"
    ),
    explanation = "A map without a title, subtitle, or source attribution is a 'naked map.' Viewers have no way to know what variable is shown, what year the data is from, or how to interpret the visualization. Context is as important as the data itself.",
    broken_plot = function() {
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = vaccination), color = "white", linewidth = 0.2) +
        scale_fill_viridis_c(
          option = "D",
          name = NULL
        ) +
        coord_map("polyconic") +
        map_theme() +
        theme(
          plot.title    = element_blank(),
          plot.subtitle = element_blank(),
          plot.caption  = element_blank()
        )
    },
    fixed_plot = function() {
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = vaccination), color = "white", linewidth = 0.2) +
        scale_fill_viridis_c(
          option = "D",
          name = "Vaccination\nRate (%)"
        ) +
        coord_map("polyconic") +
        labs(
          title    = "COVID-19 Vaccination Rate by State",
          subtitle = "Percentage of population fully vaccinated (synthetic data, 2024)",
          caption  = "Source: Synthetic data for illustration | VizAnalytics4Biz"
        ) +
        map_theme()
    }
  ),

  # ==== Round 8: Area distortion (large states dominate) ======================
  list(
    title       = "Area Distortion",
    question    = "This broadband access map looks fine technically. What's the deeper issue?",
    correct_idx = 4L,
    options     = c(
      "Missing state borders",
      "Color palette is confusing",
      "Legend has wrong units",
      "Equal-area choropleth gives visual weight to large, sparse states over small, dense ones"
    ),
    explanation = "Montana, Wyoming, and other western states have huge geographic area but small populations. On a standard choropleth, these states dominate visually even though they represent far fewer people than tiny northeastern states. Cartograms or supplementary population data can correct this misperception.",
    broken_plot = function() {
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = broadband), color = "white", linewidth = 0.2) +
        scale_fill_gradient(
          low  = "#fee5d9",
          high = "#a50f15",
          name = "Broadband\nAccess (%)"
        ) +
        coord_map("polyconic") +
        labs(
          title    = "Broadband Access by State",
          subtitle = "Examine this map carefully"
        ) +
        map_theme()
    },
    fixed_plot = function() {
      # Overlay population-proportional circles to show the area-vs-population problem
      ggplot(map_merged, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = broadband), color = "white", linewidth = 0.2) +
        scale_fill_gradient(
          low  = "#fee5d9",
          high = "#a50f15",
          name = "Broadband\nAccess (%)"
        ) +
        geom_point(
          data = state_centroids,
          aes(x = cent_long, y = cent_lat, size = population),
          inherit.aes = FALSE,
          color = NAVY, alpha = 0.45, shape = 16
        ) +
        scale_size_area(
          max_size = 14,
          labels   = scales::comma,
          name     = "Population",
          breaks   = c(5e6, 15e6, 30e6)
        ) +
        coord_map("polyconic") +
        labs(
          title    = "Broadband Access by State",
          subtitle = "Population circles reveal which states actually have the most people",
          caption  = "Circle size = population. Geographic area \u2260 population importance."
        ) +
        map_theme() +
        theme(plot.caption = element_text(size = 8.5, color = "#475569",
                                          hjust = 0, margin = margin(t = 6)))
    }
  )
)

# ---- CSS --------------------------------------------------------------------
game_css <- "
/* ---------- general ---------- */
body { background: #f8fafc; }
.bslib-page-sidebar { min-height: 100vh; }

/* ---------- sidebar ---------- */
.sidebar-info-card {
  background: white;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  padding: 12px 14px;
  margin-top: 8px;
}
.sidebar-info-card .info-label {
  color: #64748b; font-weight: 500; font-size: 0.82rem;
}

/* ---------- round banner ---------- */
.round-banner {
  text-align: center;
  padding: 18px 12px 6px;
}
.round-banner .round-title {
  font-size: 1.5rem;
  font-weight: 800;
  color: #002967;
  margin-bottom: 2px;
}
.round-banner .round-subtitle {
  font-size: 0.92rem;
  color: #B4975A;
  font-style: italic;
}

/* ---------- map container ---------- */
.map-panel {
  max-width: 1200px;
  margin: 0 auto;
  padding: 0 16px;
}
.map-display {
  display: grid;
  grid-template-columns: 1fr;
  gap: 16px;
  margin-bottom: 16px;
}
.map-display.side-by-side {
  grid-template-columns: 1fr 1fr;
}
.map-card {
  background: white;
  border: 2px solid #e2e8f0;
  border-radius: 12px;
  overflow: hidden;
  box-shadow: 0 2px 12px rgba(0,41,103,0.05);
}
.map-card.broken-card {
  border-color: #fca5a5;
}
.map-card.fixed-card {
  border-color: #86efac;
}
.map-card-header {
  padding: 8px 14px;
  font-weight: 700;
  font-size: 0.85rem;
  letter-spacing: 0.3px;
}
.map-card-header.broken-header {
  background: #fef2f2;
  color: #991b1b;
  border-bottom: 2px solid #fca5a5;
}
.map-card-header.fixed-header {
  background: #f0fdf4;
  color: #166534;
  border-bottom: 2px solid #86efac;
}

/* ---------- question area ---------- */
.question-card {
  background: white;
  border: 1px solid #e2e8f0;
  border-radius: 12px;
  padding: 20px 24px;
  margin: 12px 0;
  box-shadow: 0 2px 12px rgba(0,41,103,0.04);
}
.question-text {
  font-size: 1.05rem;
  font-weight: 600;
  color: #002967;
  margin-bottom: 14px;
}

/* radio options styling */
.question-card .shiny-options-group {
  margin-bottom: 12px;
}
.question-card .radio label {
  display: block;
  padding: 10px 14px;
  margin: 4px 0;
  border: 2px solid #e2e8f0;
  border-radius: 8px;
  cursor: pointer;
  transition: all 0.2s ease;
  font-size: 0.92rem;
  line-height: 1.4;
}
.question-card .radio label:hover {
  border-color: #B4975A;
  background: #fffbeb;
}

/* Feedback-highlighted options */
.option-correct label {
  border-color: #16a34a !important;
  background: #f0fdf4 !important;
  font-weight: 700;
}
.option-wrong label {
  border-color: #dc2626 !important;
  background: #fef2f2 !important;
}

/* ---------- feedback toast ---------- */
.feedback-toast {
  text-align: center;
  padding: 14px 20px;
  border-radius: 10px;
  font-size: 0.95rem;
  font-weight: 600;
  margin: 10px 0;
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

/* ---------- explanation card ---------- */
.explanation-card {
  padding: 14px 18px;
  border-radius: 10px;
  font-size: 0.9rem;
  line-height: 1.55;
  margin: 10px 0;
  animation: fadeSlideIn 0.3s ease;
}
.explanation-card.correct-expl {
  background: #f0fdf4;
  border-left: 4px solid #16a34a;
  color: #14532d;
}
.explanation-card.wrong-expl {
  background: #fef2f2;
  border-left: 4px solid #dc2626;
  color: #7f1d1d;
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
  font-size: 2rem;
}
.welcome-card .tagline {
  color: #B4975A;
  font-style: italic;
  font-size: 1.08rem;
  margin-bottom: 18px;
}
.welcome-card p { color: #475569; font-size: 0.95rem; line-height: 1.6; }
.welcome-card .map-emoji {
  font-size: 3rem;
  margin-bottom: 10px;
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

#copy_report { margin-top: 6px; }

/* ---------- submit button ---------- */
#submit_answer {
  font-weight: 700;
  padding: 10px 32px;
  font-size: 0.95rem;
}

/* ---------- next round button ---------- */
#next_round {
  font-weight: 700;
  padding: 10px 32px;
  font-size: 0.95rem;
}

/* ---------- footer ---------- */
.game-footer {
  text-align: center;
  padding: 12px 0;
  font-size: 0.78rem;
  color: #94a3b8;
  border-top: 1px solid #e2e8f0;
  margin-top: 20px;
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
    ")),
    tags$script(HTML("
      // Copy text to clipboard
      Shiny.addCustomMessageHandler('copyToClipboard', function(txt) {
        navigator.clipboard.writeText(txt).then(function() {
          Shiny.setInputValue('clipboard_ok', Math.random());
        });
      });
      // Update progress bar width
      Shiny.addCustomMessageHandler('updateBar', function(msg) {
        var bar = document.getElementById('round_bar');
        if (bar) bar.style.width = msg.pct + '%';
      });
      // Add CSS class to element
      Shiny.addCustomMessageHandler('addCssClass', function(msg) {
        var el = document.getElementById(msg.id);
        if (el) el.classList.add(msg.cls);
      });
      // Remove CSS class from element
      Shiny.addCustomMessageHandler('removeCssClass', function(msg) {
        var el = document.getElementById(msg.id);
        if (el) el.classList.remove(msg.cls);
      });
    "))
  ),

  # ---- sidebar ----
  sidebar = sidebar(
    width = 280,
    bg    = "#f1f5f9",

    accordion(
      id   = "help_acc",
      open = FALSE,
      accordion_panel(
        title = "How to Play",
        icon  = bsicons::bs_icon("question-circle"),
        tags$ul(
          style = "font-size:0.82rem; padding-left:1.1rem; margin-bottom:0;",
          tags$li("Enter your name and press ", tags$strong("Start Game"), "."),
          tags$li("Each round shows a US choropleth map with ",
                  tags$strong("one deliberate cartographic error"), "."),
          tags$li("Read the question and select the option that identifies the error."),
          tags$li("Press ", tags$strong("Submit Answer"),
                  " to see the corrected map and explanation."),
          tags$li("8 rounds, 12 points each. Max score: 96."),
          tags$li("After all rounds, write a brief ", tags$strong("reflection"),
                  " before copying your completion report.")
        )
      )
    ),

    hr(style = "margin:8px 0;"),

    textInput("player_name", "Your Name:", placeholder = "Enter your name"),
    actionButton("start_btn", "Start Game",
                 icon  = icon("play"),
                 class = "btn-primary w-100",
                 style = "font-weight:600;"),

    hr(style = "margin:10px 0;"),

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

    tags$div(
      class = "sidebar-info-card",
      tags$div(
        style = "font-weight:700; font-size:0.85rem; color:#002967; margin-bottom:6px;",
        bsicons::bs_icon("map"), " Map Error Types"
      ),
      tags$ul(
        style = "font-size:0.78rem; color:#475569; padding-left:1.1rem; margin:0; line-height:1.6;",
        tags$li("Wrong palette type"),
        tags$li("Missing legend"),
        tags$li("Rainbow palette"),
        tags$li("Reversed color direction"),
        tags$li("Join errors (missing states)"),
        tags$li("Misleading classification"),
        tags$li("Missing context"),
        tags$li("Area distortion")
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

# ---- SERVER -----------------------------------------------------------------
server <- function(input, output, session) {

  # ---- reactive state -------------------------------------------------------
  rv <- reactiveValues(
    round         = 0L,
    score         = 0L,
    answers       = list(),
    start_time    = NULL,
    player_name   = "",
    game_active   = FALSE,
    game_complete = FALSE,
    round_order   = seq_len(TOTAL_ROUNDS),
    feedback      = NULL,     # NULL / "correct" / "wrong"
    answered      = FALSE     # has current round been answered?
  )

  # ---- start game -----------------------------------------------------------
  observeEvent(input$start_btn, {
    req(nchar(trimws(input$player_name)) > 0)
    rv$player_name   <- trimws(input$player_name)
    rv$round         <- 1L
    rv$score         <- 0L
    rv$answers       <- list()
    rv$start_time    <- Sys.time()
    rv$game_active   <- TRUE
    rv$game_complete <- FALSE
    rv$round_order   <- sample(TOTAL_ROUNDS)
    rv$feedback      <- NULL
    rv$answered      <- FALSE
  })

  # ---- current round data ---------------------------------------------------
  current_round <- reactive({
    req(rv$game_active, rv$round > 0)
    rounds[[rv$round_order[rv$round]]]
  })

  # ---- progress outputs -----------------------------------------------------
  output$progress_text <- renderText({
    if (!rv$game_active && !rv$game_complete) return("Round -- / --")
    paste0("Round ", min(rv$round, TOTAL_ROUNDS), " / ", TOTAL_ROUNDS)
  })

  output$score_display <- renderText({
    paste0(rv$score)
  })

  observe({
    pct <- if (rv$game_active) round(rv$round / TOTAL_ROUNDS * 100) else 0
    if (rv$game_complete) pct <- 100
    session$sendCustomMessage("updateBar", list(pct = pct))
  })

  # ---- main UI router -------------------------------------------------------
  output$main_ui <- renderUI({
    if (rv$game_complete) return(build_report_ui())
    if (!rv$game_active)  return(build_welcome_ui())
    build_round_ui()
  })

  # ---- welcome screen -------------------------------------------------------
  build_welcome_ui <- function() {
    tags$div(
      class = "welcome-card",
      tags$div(class = "map-emoji", "\U0001F5FA"),
      tags$h2("Map ER"),
      tags$div(class = "tagline",
               "Fix the broken map before it misleads the public."),
      tags$hr(style = "border-color:#e2e8f0;"),
      tags$p("Every round presents a US choropleth map with ",
             tags$strong("one deliberate cartographic error"),
             " -- a bad palette, missing legend, reversed colors, ",
             "or something more subtle."),
      tags$p("Your mission: ",
             tags$strong("diagnose the error"),
             " from four options, then study the corrected map ",
             "to learn the mapping principle behind it."),
      tags$p(tags$strong("8 rounds"), " \u00D7 ",
             tags$strong("12 points"), " = ",
             tags$strong("96 max"), " points."),
      tags$p(style = "color:#94a3b8; font-size:0.85rem; margin-top:16px;",
             "Enter your name in the sidebar and press ",
             tags$strong("Start Game"), ".")
    )
  }

  # ---- round UI -------------------------------------------------------------
  build_round_ui <- function() {
    rd <- current_round()
    answered <- rv$answered

    tagList(
      tags$div(
        class = "map-panel",

        # Round banner
        tags$div(
          class = "round-banner",
          tags$div(class = "round-title",
                   bsicons::bs_icon("map-fill"),
                   paste0("  Round ", rv$round, ": ", rd$title)),
          tags$div(class = "round-subtitle",
                   "Examine the map below and identify the cartographic error")
        ),

        # Map display area
        tags$div(
          id = "map_display",
          class = if (answered) "map-display side-by-side" else "map-display",

          # Broken map (always shown)
          tags$div(
            class = "map-card broken-card",
            tags$div(class = "map-card-header broken-header",
                     bsicons::bs_icon("exclamation-triangle-fill"),
                     if (answered) " Broken Map" else " Examine This Map"),
            with_spinner(plotOutput("broken_map", height = "380px"))
          ),

          # Fixed map (shown only after answering)
          if (answered) {
            tags$div(
              class = "map-card fixed-card",
              tags$div(class = "map-card-header fixed-header",
                       bsicons::bs_icon("check-circle-fill"),
                       " Corrected Map"),
              with_spinner(plotOutput("fixed_map", height = "380px"))
            )
          }
        ),

        # Feedback toast
        if (answered) uiOutput("feedback_toast"),

        # Explanation card
        if (answered) uiOutput("explanation_card"),

        # Question area
        tags$div(
          class = "question-card",
          tags$div(class = "question-text",
                   bsicons::bs_icon("patch-question-fill"),
                   paste0("  ", rd$question)),

          radioButtons(
            "answer_choice",
            label    = NULL,
            choices  = setNames(seq_along(rd$options), rd$options),
            selected = character(0)
          ),

          if (!answered) {
            actionButton("submit_answer", "Submit Answer",
                         icon  = icon("check"),
                         class = "btn-primary",
                         style = "margin-top:6px;")
          } else {
            actionButton("next_round",
                         if (rv$round < TOTAL_ROUNDS) "Next Round" else "See Results",
                         icon  = icon("arrow-right"),
                         class = "btn-primary",
                         style = "margin-top:6px;")
          }
        )
      )
    )
  }

  # ---- render broken map ----------------------------------------------------
  output$broken_map <- renderPlot({
    req(rv$game_active, rv$round > 0)
    rd <- current_round()
    rd$broken_plot()
  }, res = 100, bg = "white")

  # ---- render fixed map -----------------------------------------------------
  output$fixed_map <- renderPlot({
    req(rv$answered)
    rd <- current_round()
    rd$fixed_plot()
  }, res = 100, bg = "white")

  # ---- submit answer --------------------------------------------------------
  observeEvent(input$submit_answer, {
    req(!rv$answered)
    chosen <- as.integer(input$answer_choice)
    req(length(chosen) == 1, !is.na(chosen))

    rd         <- current_round()
    is_correct <- (chosen == rd$correct_idx)

    if (is_correct) {
      rv$score    <- rv$score + PTS_PER_Q
      rv$feedback <- "correct"
    } else {
      rv$feedback <- "wrong"
    }

    rv$answered <- TRUE

    # Record answer
    rv$answers[[rv$round]] <- list(
      round       = rv$round,
      error_type  = rd$title,
      picked      = rd$options[chosen],
      correct_ans = rd$options[rd$correct_idx],
      result      = ifelse(is_correct, "Correct", "Wrong"),
      points      = ifelse(is_correct, PTS_PER_Q, 0L)
    )

    # Highlight correct/wrong radio options via JS
    # The radio buttons have wrapper divs with class "radio"
    # We add classes to highlight them
    for (i in seq_along(rd$options)) {
      opt_selector <- paste0(
        "#answer_choice .radio:nth-child(", i, ")"
      )
      if (i == rd$correct_idx) {
        shinyjs::runjs(paste0(
          "document.querySelector('", opt_selector,
          "').classList.add('option-correct');"
        ))
      } else if (i == chosen && !is_correct) {
        shinyjs::runjs(paste0(
          "document.querySelector('", opt_selector,
          "').classList.add('option-wrong');"
        ))
      }
    }

    # Disable radio buttons after answering
    shinyjs::disable("answer_choice")
  })

  # ---- feedback toast -------------------------------------------------------
  output$feedback_toast <- renderUI({
    req(rv$feedback)
    rd <- current_round()
    if (rv$feedback == "correct") {
      tags$div(
        class = "feedback-toast correct",
        bsicons::bs_icon("check-circle-fill"),
        paste0(" Correct!  +", PTS_PER_Q, " points")
      )
    } else {
      tags$div(
        class = "feedback-toast wrong",
        bsicons::bs_icon("x-circle-fill"),
        " Not quite. The correct answer is highlighted in green."
      )
    }
  })

  # ---- explanation card -----------------------------------------------------
  output$explanation_card <- renderUI({
    req(rv$answered)
    rd <- current_round()
    expl_class <- if (rv$feedback == "correct") "explanation-card correct-expl"
                  else "explanation-card wrong-expl"
    tags$div(
      class = expl_class,
      tags$strong("Why this matters: "),
      rd$explanation
    )
  })

  # ---- next round -----------------------------------------------------------
  observeEvent(input$next_round, {
    if (rv$round >= TOTAL_ROUNDS) {
      rv$game_active   <- FALSE
      rv$game_complete <- TRUE
    } else {
      rv$round    <- rv$round + 1L
      rv$feedback <- NULL
      rv$answered <- FALSE
      # Reset radio selection
      updateRadioButtons(session, "answer_choice", selected = character(0))
      shinyjs::enable("answer_choice")
    }
  })

  # ---- completion report ----------------------------------------------------
  build_report_ui <- function() {
    ts      <- format(Sys.time(), "%B %d, %Y at %I:%M %p")
    pname   <- rv$player_name
    score   <- rv$score
    max_pts <- TOTAL_ROUNDS * PTS_PER_Q
    pct     <- round(score / max_pts * 100)

    hash_input <- paste(pname, score, ts, sep = "|")
    hash_code  <- substr(digest::digest(hash_input, algo = "md5"), 1, 8)

    # Build breakdown rows
    rows <- lapply(rv$answers, function(a) {
      res_class <- if (a$result == "Correct") "result-correct" else "result-wrong"
      tags$tr(
        tags$td(a$round),
        tags$td(a$error_type),
        tags$td(class = res_class, a$result),
        tags$td(paste0(a$points))
      )
    })

    # Plain-text version for clipboard
    report_lines <- c(
      "===== MAP ER -- COMPLETION REPORT =====",
      paste0("Player:    ", pname),
      paste0("Date:      ", ts),
      paste0("Score:     ", score, " / ", max_pts, "  (", pct, "%)"),
      paste0("Hash:      ", hash_code),
      "",
      "Round | Error Type                    | Result  | Points",
      paste0(rep("-", 62), collapse = ""),
      vapply(rv$answers, function(a) {
        sprintf("%-5d | %-29s | %-7s | %d",
                a$round, a$error_type, a$result, a$points)
      }, character(1)),
      ""
    )
    report_text <- paste(report_lines, collapse = "\n")

    tags$div(
      class = "report-card",
      tags$div(class = "report-header",
               bsicons::bs_icon("trophy-fill"), "  Map ER -- Game Complete!"),
      tags$div(
        class = "report-body",
        tags$div(class = "meta-line",
                 tags$strong("Player: "), pname),
        tags$div(class = "meta-line",
                 tags$strong("Date: "), ts),

        tags$div(class = "final-score",
                 paste0(score, " / ", max_pts)),
        tags$div(class = "final-pct", paste0(pct, "% correct")),

        # Breakdown table
        tags$table(
          class = "breakdown-table",
          tags$thead(
            tags$tr(
              tags$th("Round"),
              tags$th("Error Type"),
              tags$th("Result"),
              tags$th("Points")
            )
          ),
          tags$tbody(rows)
        ),

        # Hash code
        tags$div(
          tags$strong("Verification Hash: "),
          tags$span(class = "hash-code", toupper(hash_code))
        ),

        # Copy button
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

        # Ignatian reflection
        tags$div(
          class = "reflection-box",
          tags$label(
            "for" = "reflection_input",
            style = "display:block; margin-bottom:8px; font-size:0.9rem;",
            tags$strong("Ignatian Reflection: "),
            "Whose perspective might be missing or marginalized in the geographic visualizations you encounter?"
          ),
          tags$textarea(
            id = "reflection_input",
            class = "form-control",
            rows = "3",
            placeholder = "Write 1-2 sentences here...",
            style = "font-size:0.88rem; resize:vertical;"
          )
        ),

        # Play again
        tags$div(
          style = "text-align:center; margin-top:18px;",
          actionButton("play_again", "Play Again",
                       icon  = icon("rotate-right"),
                       class = "btn-primary",
                       style = "font-weight:600; padding:8px 28px;")
        )
      ),

      # Hidden textarea for clipboard
      tags$textarea(
        id    = "report_text_store",
        style = "display:none;",
        report_text
      )
    )
  }

  # ---- play again -----------------------------------------------------------
  observeEvent(input$play_again, {
    rv$round         <- 1L
    rv$score         <- 0L
    rv$answers       <- list()
    rv$start_time    <- Sys.time()
    rv$game_active   <- TRUE
    rv$game_complete <- FALSE
    rv$round_order   <- sample(TOTAL_ROUNDS)
    rv$feedback      <- NULL
    rv$answered      <- FALSE
  })
}

shinyApp(ui, server)
