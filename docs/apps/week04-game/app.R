library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(forcats)
library(shinyjs)

# Optional packages: graceful fallback
has_spinner  <- requireNamespace("shinycssloaders", quietly = TRUE)
has_ggridges <- requireNamespace("ggridges", quietly = TRUE)

# ---------------------------------------------------------------------------
# Gonzaga branding
# ---------------------------------------------------------------------------
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ---------------------------------------------------------------------------
# Spinner helper
# ---------------------------------------------------------------------------
with_spinner <- function(ui_element) {
  if (has_spinner) {
    shinycssloaders::withSpinner(ui_element, color = NAVY, type = 6)
  } else {
    ui_element
  }
}

# ---------------------------------------------------------------------------
# Chart type definitions (the 7 options)
# ---------------------------------------------------------------------------
CHART_TYPES <- c(
  "Bar Chart"      = "bar",
  "Lollipop Chart" = "lollipop",
  "Violin Plot"    = "violin",
  "Ridgeline Plot" = "ridgeline",
  "Heatmap"        = "heatmap",
  "Bubble Chart"   = "bubble",
  "Boxplot"        = "boxplot"
)

CHART_IDS <- unname(CHART_TYPES)
CHART_NAMES <- names(CHART_TYPES)
names(CHART_NAMES) <- CHART_IDS

# ---------------------------------------------------------------------------
# Thumbnail generator: small ggplot for each chart type
# ---------------------------------------------------------------------------
make_thumbnail <- function(chart_type) {
  base <- theme_minimal(base_size = 9) +
    theme(
      plot.title       = element_text(color = NAVY, face = "bold", size = 10,
                                       hjust = 0.5, margin = margin(b = 3)),
      axis.title       = element_blank(),
      axis.text        = element_text(size = 7, color = "#64748b"),
      panel.grid.minor = element_blank(),
      plot.margin      = margin(6, 6, 6, 6)
    )

  set.seed(42)

  switch(chart_type,

    bar = {
      df <- data.frame(
        cat = factor(LETTERS[1:5], levels = LETTERS[5:1]),
        val = c(45, 72, 58, 33, 61)
      )
      ggplot(df, aes(x = cat, y = val)) +
        geom_col(fill = NAVY, alpha = 0.85, width = 0.65) +
        coord_flip() +
        labs(title = "Bar Chart") + base
    },

    lollipop = {
      df <- data.frame(
        cat = factor(LETTERS[1:6], levels = LETTERS[6:1]),
        val = c(38, 55, 42, 67, 48, 60)
      )
      ggplot(df, aes(x = cat, y = val)) +
        geom_segment(aes(xend = cat, y = 0, yend = val),
                     color = "#94a3b8", linewidth = 0.7) +
        geom_point(color = NAVY, size = 2.5) +
        coord_flip() +
        labs(title = "Lollipop Chart") + base
    },

    violin = {
      df <- data.frame(
        grp = rep(c("A", "B", "C"), each = 80),
        val = c(rnorm(80, 50, 10), rnorm(80, 60, 8), rnorm(80, 45, 12))
      )
      ggplot(df, aes(x = grp, y = val, fill = grp)) +
        geom_violin(alpha = 0.7, color = NA) +
        scale_fill_manual(values = c(NAVY, GOLD, RED)) +
        labs(title = "Violin Plot") + base +
        theme(legend.position = "none")
    },

    ridgeline = {
      df <- data.frame(
        grp = factor(rep(c("D", "C", "B", "A"), each = 100),
                     levels = c("A", "B", "C", "D")),
        val = c(rnorm(100, 40, 8), rnorm(100, 50, 6),
                rnorm(100, 55, 10), rnorm(100, 65, 7))
      )
      if (has_ggridges) {
        ggplot(df, aes(x = val, y = grp, fill = grp)) +
          ggridges::geom_density_ridges(alpha = 0.7, scale = 1.2,
                                         color = "white", linewidth = 0.3) +
          scale_fill_manual(values = c(NAVY, GOLD, RED, "#5B8FA8")) +
          labs(title = "Ridgeline Plot") + base +
          theme(legend.position = "none")
      } else {
        ggplot(df, aes(x = val, color = grp)) +
          geom_density(linewidth = 0.8) +
          scale_color_manual(values = c(NAVY, GOLD, RED, "#5B8FA8")) +
          labs(title = "Ridgeline Plot") + base +
          theme(legend.position = "none")
      }
    },

    heatmap = {
      df <- expand.grid(row = LETTERS[1:4], col = paste0("Q", 1:4))
      df$val <- c(12, 45, 28, 60, 33, 18, 55, 42, 50, 38, 22, 48,
                  25, 62, 40, 15)
      ggplot(df, aes(x = col, y = row, fill = val)) +
        geom_tile(color = "white", linewidth = 0.6) +
        scale_fill_viridis_c(option = "mako", direction = -1, guide = "none") +
        labs(title = "Heatmap") + base
    },

    bubble = {
      df <- data.frame(
        x = c(2, 5, 3, 7, 4, 6),
        y = c(40, 65, 50, 80, 55, 70),
        sz = c(10, 25, 15, 30, 20, 18)
      )
      ggplot(df, aes(x = x, y = y, size = sz)) +
        geom_point(color = NAVY, alpha = 0.6) +
        scale_size_area(max_size = 8, guide = "none") +
        labs(title = "Bubble Chart") + base
    },

    boxplot = {
      df <- data.frame(
        grp = rep(c("A", "B", "C", "D"), each = 50),
        val = c(rnorm(50, 50, 10), rnorm(50, 60, 8),
                rnorm(50, 45, 12), rnorm(50, 55, 9))
      )
      ggplot(df, aes(x = grp, y = val, fill = grp)) +
        geom_boxplot(alpha = 0.7, outlier.size = 1) +
        scale_fill_manual(values = c(NAVY, GOLD, RED, "#5B8FA8")) +
        labs(title = "Boxplot") + base +
        theme(legend.position = "none")
    }
  )
}

# ---------------------------------------------------------------------------
# Full-size correct chart generator (renders scenario-appropriate data)
# ---------------------------------------------------------------------------
make_full_chart <- function(scenario_num) {
  base <- theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(color = NAVY, face = "bold", size = 14),
      plot.subtitle    = element_text(color = "#64748b", size = 11),
      axis.title       = element_text(size = 11),
      panel.grid.minor = element_blank()
    )

  set.seed(scenario_num + 100)

  switch(as.character(scenario_num),

    # 1: Bar Chart - revenue across 8 offices
    "1" = {
      df <- data.frame(
        office = paste("Region", LETTERS[1:8]),
        revenue = c(4.2, 3.8, 5.1, 2.9, 6.3, 3.5, 4.7, 5.5)
      ) |> mutate(office = forcats::fct_reorder(office, revenue))
      ggplot(df, aes(x = office, y = revenue)) +
        geom_col(fill = NAVY, alpha = 0.85, width = 0.65) +
        coord_flip() +
        labs(title = "Total Revenue by Regional Office",
             subtitle = "Bar chart: position on a common scale",
             x = NULL, y = "Revenue ($M)") +
        base
    },

    # 2: Violin - customer response times
    "2" = {
      df <- data.frame(
        group = "Customers",
        time = c(rlnorm(500, meanlog = 3, sdlog = 0.8))
      )
      ggplot(df, aes(x = group, y = time)) +
        geom_violin(fill = NAVY, alpha = 0.7, color = NA) +
        geom_boxplot(width = 0.1, fill = "white", outlier.size = 1) +
        labs(title = "Distribution of Customer Response Times",
             subtitle = "Violin plot: shows full distribution shape",
             x = NULL, y = "Response Time (seconds)") +
        base
    },

    # 3: Boxplot - salary distributions across 5 departments
    "3" = {
      depts <- c("Sales", "Engineering", "Marketing", "HR", "Finance")
      df <- data.frame(
        dept = rep(depts, each = 60),
        salary = c(rnorm(60, 65, 12), rnorm(60, 95, 15), rnorm(60, 70, 10),
                   rnorm(60, 60, 8), rnorm(60, 80, 14))
      ) |> mutate(dept = factor(dept))
      ggplot(df, aes(x = dept, y = salary, fill = dept)) +
        geom_boxplot(alpha = 0.7, outlier.color = RED, outlier.size = 2) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "Salary Distributions Across Departments",
             subtitle = "Boxplot: median, IQR, and outliers at a glance",
             x = NULL, y = "Salary ($K)") +
        base + theme(legend.position = "none")
    },

    # 4: Ridgeline - satisfaction across 6 product lines
    "4" = {
      products <- paste("Product", LETTERS[1:6])
      df <- data.frame(
        product = factor(rep(products, each = 80), levels = rev(products)),
        score = c(rnorm(80, 7.2, 1.1), rnorm(80, 6.5, 1.5),
                  rnorm(80, 8.0, 0.8), rnorm(80, 5.8, 1.3),
                  rnorm(80, 7.0, 1.0), rnorm(80, 6.2, 1.4))
      )
      if (has_ggridges) {
        ggplot(df, aes(x = score, y = product, fill = after_stat(x))) +
          ggridges::geom_density_ridges_gradient(scale = 1.5,
                                                  rel_min_height = 0.01) +
          scale_fill_viridis_c(option = "plasma", guide = "none") +
          labs(title = "Satisfaction Score Distributions by Product Line",
               subtitle = "Ridgeline plot: shape comparison, compact display",
               x = "Satisfaction Score", y = NULL) +
          base
      } else {
        ggplot(df, aes(x = score, color = product)) +
          geom_density(linewidth = 0.9) +
          scale_color_brewer(palette = "Set2") +
          labs(title = "Satisfaction Score Distributions by Product Line",
               subtitle = "Density overlay (install ggridges for ridgeline display)",
               x = "Satisfaction Score", y = "Density") +
          base
      }
    },

    # 5: Bubble - ad spend vs sales vs market size
    "5" = {
      df <- data.frame(
        market = paste("Market", 1:12),
        ad_spend = c(120, 340, 200, 450, 180, 280, 390, 150, 310, 500, 260, 410),
        sales = c(45, 120, 75, 160, 55, 95, 140, 50, 110, 185, 85, 150),
        mkt_size = c(8, 22, 14, 30, 10, 18, 25, 9, 20, 35, 15, 28)
      )
      ggplot(df, aes(x = ad_spend, y = sales, size = mkt_size)) +
        geom_point(color = NAVY, alpha = 0.6) +
        scale_size_area(max_size = 14, name = "Market Size") +
        labs(title = "Ad Spend vs. Sales (bubble = market size)",
             subtitle = "Bubble chart: two axes + size for third variable",
             x = "Advertising Spend ($K)", y = "Sales ($K)") +
        base
    },

    # 6: Heatmap - complaints by day x hour
    "6" = {
      days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      hours <- paste0(seq(8, 20, 2), ":00")
      df <- expand.grid(
        day = factor(days, levels = rev(days)),
        hour = factor(hours, levels = hours)
      )
      set.seed(106)
      df$complaints <- sample(5:55, nrow(df), replace = TRUE)
      # Make patterns: weekday lunch rush, weekend evening spike
      df$complaints[df$day %in% c("Mon","Tue","Wed","Thu","Fri") &
                      df$hour %in% c("12:00","14:00")] <-
        df$complaints[df$day %in% c("Mon","Tue","Wed","Thu","Fri") &
                        df$hour %in% c("12:00","14:00")] + 25
      df$complaints[df$day %in% c("Sat","Sun") &
                      df$hour %in% c("18:00","20:00")] <-
        df$complaints[df$day %in% c("Sat","Sun") &
                        df$hour %in% c("18:00","20:00")] + 20
      ggplot(df, aes(x = hour, y = day, fill = complaints)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(aes(label = complaints), size = 3, color = "white") +
        scale_fill_viridis_c(option = "mako", direction = -1,
                             name = "Complaints") +
        labs(title = "Customer Complaints by Day and Hour",
             subtitle = "Heatmap: two categorical axes, color = intensity",
             x = "Hour of Day", y = NULL) +
        base +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    },

    # 7: Lollipop - 15 product categories, profit margins
    "7" = {
      set.seed(107)
      df <- data.frame(
        category = paste("Category", sprintf("%02d", 1:15)),
        margin = round(runif(15, 5, 35), 1)
      ) |> mutate(category = forcats::fct_reorder(category, margin))
      ggplot(df, aes(x = category, y = margin)) +
        geom_segment(aes(xend = category, y = 0, yend = margin),
                     color = "#94a3b8", linewidth = 0.7) +
        geom_point(color = NAVY, size = 3) +
        coord_flip() +
        labs(title = "Profit Margins Across 15 Product Categories",
             subtitle = "Lollipop chart: clean display for many categories",
             x = NULL, y = "Profit Margin (%)") +
        base
    },

    # 8: Violin - test scores across 4 school types
    "8" = {
      types <- c("Public", "Charter", "Private", "Magnet")
      df <- data.frame(
        school_type = rep(types, each = 100),
        score = c(rnorm(100, 72, 12), rnorm(100, 78, 10),
                  rnorm(100, 82, 8), rnorm(100, 85, 9))
      ) |> mutate(school_type = factor(school_type, levels = types))
      ggplot(df, aes(x = school_type, y = score, fill = school_type)) +
        geom_violin(alpha = 0.7, color = NA) +
        geom_boxplot(width = 0.1, fill = "white", outlier.size = 1) +
        scale_fill_manual(values = c(NAVY, GOLD, RED, "#5B8FA8")) +
        labs(title = "Test Score Distributions by School Type",
             subtitle = "Violin plot: full shape comparison for 4 groups",
             x = NULL, y = "Test Score") +
        base + theme(legend.position = "none")
    },

    # 9: Heatmap - website traffic by month x day
    "9" = {
      months <- month.abb
      days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      df <- expand.grid(
        month = factor(months, levels = months),
        day = factor(days, levels = rev(days))
      )
      set.seed(109)
      df$traffic <- sample(800:5000, nrow(df), replace = TRUE)
      # Pattern: higher on weekdays, summer peak
      df$traffic[df$day %in% c("Sat","Sun")] <-
        df$traffic[df$day %in% c("Sat","Sun")] * 0.6
      df$traffic[df$month %in% c("Jun","Jul","Aug")] <-
        df$traffic[df$month %in% c("Jun","Jul","Aug")] * 1.4
      df$traffic <- round(df$traffic)
      ggplot(df, aes(x = month, y = day, fill = traffic)) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_viridis_c(option = "mako", direction = -1,
                             name = "Visits") +
        labs(title = "Website Traffic: Month x Day of Week",
             subtitle = "Heatmap: two ordinal dimensions, color = traffic",
             x = "Month", y = NULL) +
        base +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    },

    # 10: Bubble - median home prices x listings across 20 neighborhoods
    "10" = {
      set.seed(110)
      df <- data.frame(
        neighborhood = paste("Nbhd", 1:20),
        median_price = runif(20, 200, 800),
        listings = sample(10:120, 20, replace = TRUE),
        sq_ft = runif(20, 1200, 3500)
      )
      ggplot(df, aes(x = median_price, y = listings, size = sq_ft)) +
        geom_point(color = NAVY, alpha = 0.55) +
        scale_size_area(max_size = 12, name = "Avg Sq Ft") +
        labs(title = "Median Home Price vs. Number of Listings",
             subtitle = "Bubble chart: price (x), listings (y), size = sq footage",
             x = "Median Price ($K)", y = "Number of Listings") +
        base
    }
  )
}

# ---------------------------------------------------------------------------
# Scenario bank: 10 questions
# ---------------------------------------------------------------------------
SCENARIOS <- list(
  list(
    id       = 1,
    question = "Compare total revenue across 8 regional offices.",
    data_desc = "8 categories (offices), 1 continuous measure (total revenue).",
    answer   = "bar",
    correct_explanation = paste(
      "Bar charts use position along a common scale -- the most accurate",
      "perceptual channel per Cleveland & McGill. With 8 categories and one",
      "measure, a sorted bar chart makes ranking effortless."
    ),
    wrong_explanations = list(
      lollipop  = "Lollipop charts are similar, but with only 8 categories the bar chart's filled bars provide a stronger visual anchor. Both are acceptable, but bar charts are the standard for simple category comparisons.",
      violin    = "Violin plots show distributions, but you only have one summary value per office -- there is no distribution to display.",
      ridgeline = "Ridgeline plots show overlapping density curves -- they require raw observations within each group, not a single total per office.",
      heatmap   = "Heatmaps require two categorical dimensions. You have one category (office) and one value -- not a matrix structure.",
      bubble    = "Bubble charts encode three continuous variables. You only have one category and one measure -- the extra encodings would be wasted.",
      boxplot   = "Boxplots summarize distributions (median, IQR, outliers). Your data is already aggregated to one total per office."
    )
  ),
  list(
    id       = 2,
    question = "Show the distribution of customer response times (single group, ~500 observations).",
    data_desc = "1 group, 1 continuous variable, ~500 raw observations.",
    answer   = "violin",
    correct_explanation = paste(
      "A violin plot reveals the full shape of a distribution -- skewness,",
      "multiple modes, spread -- all at once. For a single group with many",
      "observations, it is the ideal choice to show distributional shape."
    ),
    wrong_explanations = list(
      bar       = "Bar charts compare quantities across categories. You have a single continuous variable -- there are no categories to compare.",
      lollipop  = "Lollipop charts, like bar charts, compare summary values across categories. You need to show a distribution, not a single summary.",
      ridgeline = "Ridgeline plots compare distributions across multiple groups. With only one group, the overlapping layout adds no benefit over a violin.",
      heatmap   = "Heatmaps encode values in a two-dimensional matrix of color tiles. A single continuous variable does not fit this structure.",
      bubble    = "Bubble charts require at least two continuous axes plus a size variable. You have one variable and one group.",
      boxplot   = "Boxplots show five-number summaries but hide distributional shape (bimodality, skewness). The violin reveals the full density."
    )
  ),
  list(
    id       = 3,
    question = "Compare salary distributions across 5 departments.",
    data_desc = "5 groups (departments), 1 continuous variable (salary), ~50-100 observations per group.",
    answer   = "boxplot",
    correct_explanation = paste(
      "Boxplots provide a compact summary of each distribution -- median,",
      "IQR, and outliers -- enabling quick side-by-side comparison.",
      "With 5 groups, the clear structural summary is more interpretable",
      "than 5 overlapping density shapes for a non-technical audience."
    ),
    wrong_explanations = list(
      bar       = "Bar charts show one value per category. You want to show the entire salary distribution, not just the mean or total.",
      lollipop  = "Lollipop charts show single summary values per category. You need to display distributional information (spread, outliers).",
      violin    = "Violin plots also show distributions and would be acceptable here, but boxplots provide a more familiar summary (median, quartiles, outliers) for comparing across groups.",
      ridgeline = "Ridgeline plots work for many groups, but with only 5 departments, the compact boxplot layout is clearer and shows key summary statistics directly.",
      heatmap   = "Heatmaps require two categorical axes. You have one categorical axis (department) and one continuous variable -- not a matrix.",
      bubble    = "Bubble charts encode three continuous variables on two axes plus size. Salary distributions do not map to this structure."
    )
  ),
  list(
    id       = 4,
    question = "Show how satisfaction scores are distributed across 6 product lines, emphasizing distribution shape.",
    data_desc = "6 groups (products), 1 continuous variable (satisfaction score), ~80 observations per group.",
    answer   = "ridgeline",
    correct_explanation = paste(
      "Ridgeline plots stack density curves vertically, making it easy to compare",
      "distributional shapes across many groups in a compact layout. When the emphasis",
      "is on shape (skewness, modes), ridgelines are ideal."
    ),
    wrong_explanations = list(
      bar       = "Bar charts show a single summary per category and completely hide distributional shape.",
      lollipop  = "Lollipop charts, like bar charts, reduce each group to one number. You cannot see spread, skewness, or modes.",
      violin    = "Violin plots also show distribution shape, but with 6 groups they can become wide and hard to compare. Ridgeline's stacked layout is more space-efficient and emphasizes shape differences.",
      heatmap   = "Heatmaps encode values in a color matrix. A single continuous distribution does not map naturally to a grid of tiles.",
      bubble    = "Bubble charts encode three continuous variables. You have one continuous measure per group -- no third variable for size encoding.",
      boxplot   = "Boxplots show summary statistics (median, IQR) but hide the actual distribution shape. The question specifically asks to emphasize shape."
    )
  ),
  list(
    id       = 5,
    question = "Display correlation between advertising spend and sales, with market size as a third variable.",
    data_desc = "12 markets, 3 continuous variables: ad spend, sales, and market size.",
    answer   = "bubble",
    correct_explanation = paste(
      "Bubble charts encode two continuous variables on the x- and y-axes (highest",
      "perceptual accuracy) and use bubble area for a third variable. This is the",
      "standard approach for showing relationships among three continuous measures."
    ),
    wrong_explanations = list(
      bar       = "Bar charts compare one value across categories. You have three continuous variables and want to show their relationships.",
      lollipop  = "Lollipop charts show one summary per category. They cannot encode the three-way relationship among spend, sales, and market size.",
      violin    = "Violin plots show distributions of a single variable across groups. Your question is about the relationship between three continuous variables.",
      ridgeline = "Ridgeline plots compare distributions. You want to show a scatter relationship with a third variable, not distributions.",
      heatmap   = "Heatmaps need two categorical dimensions. Your variables are all continuous -- a scatter-based approach is needed.",
      boxplot   = "Boxplots summarize distributions. You need to show the correlation between three continuous variables."
    )
  ),
  list(
    id       = 6,
    question = "Show which day of the week and hour of day have the most customer complaints.",
    data_desc = "2 categorical/ordinal dimensions (day, hour), 1 continuous measure (complaint count).",
    answer   = "heatmap",
    correct_explanation = paste(
      "Heatmaps encode a continuous value as color intensity across two categorical",
      "dimensions arranged as a matrix. Day-of-week x hour-of-day is a classic",
      "heatmap use case -- readers instantly spot 'hot spots' of high complaint volume."
    ),
    wrong_explanations = list(
      bar       = "Bar charts work for one categorical dimension. With two dimensions (day AND hour), you would need grouped or faceted bars, which becomes cluttered. A heatmap is far more compact.",
      lollipop  = "Lollipop charts handle one category axis. Two dimensions (day x hour) create too many combinations for a clean lollipop display.",
      violin    = "Violin plots show distributions. You want to display a single count for each day-hour combination, not a distribution.",
      ridgeline = "Ridgeline plots compare distributions across one grouping variable. You have two grouping variables forming a matrix.",
      bubble    = "Bubble charts require continuous x and y axes. Day and hour are categorical/ordinal -- a matrix layout (heatmap) is more natural.",
      boxplot   = "Boxplots show distributions. Each day-hour combination has a single complaint count, not a distribution."
    )
  ),
  list(
    id       = 7,
    question = "Compare profit margins for 15 product categories (simple comparison, exact values matter).",
    data_desc = "15 categories, 1 continuous measure (profit margin %).",
    answer   = "lollipop",
    correct_explanation = paste(
      "With 15 categories, a lollipop chart reduces visual clutter compared to",
      "thick bars while still using position along a common scale. The dot endpoint",
      "emphasizes the exact value, making it easier to read precise margins."
    ),
    wrong_explanations = list(
      bar       = "Bar charts also use position on a common scale and would work, but with 15 categories the heavy bars create visual clutter. The lollipop's minimal ink is cleaner for many categories.",
      violin    = "Violin plots show distributions. You have one summary value per category, not a distribution to display.",
      ridgeline = "Ridgeline plots show overlapping density curves. Your data is one number per category -- there is nothing to show as a density.",
      heatmap   = "Heatmaps need two dimensions. You have one category and one value -- not a matrix structure.",
      bubble    = "Bubble charts need three continuous variables. You have one categorical and one continuous variable.",
      boxplot   = "Boxplots show distributions with summary statistics. You have a single value per category, not raw data to summarize."
    )
  ),
  list(
    id       = 8,
    question = "Show how test score distributions differ between 4 school types, emphasizing the full shape.",
    data_desc = "4 groups (school types), 1 continuous variable (test score), ~100 students per group.",
    answer   = "violin",
    correct_explanation = paste(
      "Violin plots reveal the full density shape of each distribution. With 4",
      "groups, the side-by-side violins are easy to compare, and the mirrored",
      "density curves highlight differences in shape, spread, and central tendency."
    ),
    wrong_explanations = list(
      bar       = "Bar charts reduce each group to a single summary value, completely hiding the distributional shape you want to emphasize.",
      lollipop  = "Lollipop charts show one number per category. You need to display the full distribution, not a single summary.",
      ridgeline = "Ridgeline plots also show distributional shape and would be acceptable, but with only 4 groups, side-by-side violins provide a more direct and familiar comparison.",
      heatmap   = "Heatmaps encode a matrix of values. You have one variable per group -- not two categorical dimensions.",
      bubble    = "Bubble charts show relationships among three continuous variables. You want to compare distributions across groups.",
      boxplot   = "Boxplots show summary statistics but hide the full distributional shape. The question specifically asks to emphasize the full shape."
    )
  ),
  list(
    id       = 9,
    question = "Visualize website traffic patterns across 12 months and 7 days of the week.",
    data_desc = "2 ordinal dimensions (month, day-of-week), 1 continuous measure (traffic count). 84 cells total.",
    answer   = "heatmap",
    correct_explanation = paste(
      "Two ordinal dimensions forming a 12x7 matrix is a textbook heatmap scenario.",
      "Color intensity reveals seasonal and weekly patterns simultaneously -- readers",
      "can spot summer peaks and weekday vs. weekend differences at a glance."
    ),
    wrong_explanations = list(
      bar       = "With 84 month-day combinations, a bar chart would be impossibly cluttered. A heatmap compactly displays the full matrix.",
      lollipop  = "Lollipop charts cannot efficiently display 84 combinations. The two-dimensional matrix structure calls for a heatmap.",
      violin    = "Violin plots show distributions. Each month-day combination has a single traffic value, not a distribution.",
      ridgeline = "Ridgeline plots compare distributions across one grouping variable. You have two dimensions forming a grid.",
      bubble    = "Bubble charts need continuous axes. Months and days are ordinal -- a grid layout (heatmap) maps more naturally.",
      boxplot   = "Boxplots summarize distributions. Each cell in the month-day matrix has one value, not multiple observations."
    )
  ),
  list(
    id       = 10,
    question = "Compare the median home prices and number of listings across 20 neighborhoods.",
    data_desc = "20 neighborhoods, 2 continuous measures (median price, listing count), plus neighborhood size.",
    answer   = "bubble",
    correct_explanation = paste(
      "A bubble chart encodes median price on one axis, listing count on the other,",
      "and can use size for a third variable (e.g., neighborhood size or average",
      "square footage). This reveals the relationship between price and inventory",
      "across neighborhoods."
    ),
    wrong_explanations = list(
      bar       = "Bar charts show one value per category. You have two measures per neighborhood -- a bar chart would require two separate charts or grouped bars, losing the relationship between variables.",
      lollipop  = "Lollipop charts handle one measure per category. Two separate lollipop charts would miss the relationship between price and listings.",
      violin    = "Violin plots show distributions of a single variable. You want to compare two measures per neighborhood, not distributions.",
      ridgeline = "Ridgeline plots compare distributions. You have summary values per neighborhood, not raw data distributions.",
      heatmap   = "Heatmaps need two categorical axes. You have one category (neighborhood) and two continuous measures -- a scatter/bubble approach is better.",
      boxplot   = "Boxplots summarize distributions. You have two aggregate measures per neighborhood, not raw data to boxplot."
    )
  )
)

# ---------------------------------------------------------------------------
# CSS styles
# ---------------------------------------------------------------------------
app_css <- paste0("
  /* Overall page */
  .main-content { max-width: 1100px; margin: 0 auto; }

  /* Header banner */
  .game-header {
    background: linear-gradient(135deg, ", NAVY, " 0%, #003d8f 100%);
    color: white;
    padding: 18px 22px;
    border-radius: 10px;
    margin-bottom: 16px;
    text-align: center;
  }
  .game-header h2 { margin: 0 0 4px 0; font-weight: 700; font-size: 1.55rem; }
  .game-header .tagline {
    font-style: italic; color: ", GOLD, "; font-size: 1.0rem; margin: 0;
  }

  /* Scenario card */
  .scenario-card {
    background: #f0f4ff;
    border-left: 5px solid ", NAVY, ";
    padding: 16px 20px;
    border-radius: 6px;
    margin-bottom: 14px;
  }
  .scenario-card .q-label {
    color: ", NAVY, "; font-weight: 700; font-size: 0.85rem;
    text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 6px;
  }
  .scenario-card .q-text {
    font-size: 1.08rem; color: #1e293b; font-weight: 600;
    line-height: 1.5; margin-bottom: 6px;
  }
  .scenario-card .q-data {
    font-size: 0.88rem; color: #475569; margin: 0;
  }

  /* Chart button grid */
  .chart-btn-wrapper {
    border: 2px solid #e2e8f0;
    border-radius: 8px;
    padding: 8px;
    text-align: center;
    cursor: pointer;
    transition: all 0.2s ease;
    background: white;
    height: 100%;
  }
  .chart-btn-wrapper:hover {
    border-color: ", NAVY, ";
    box-shadow: 0 2px 8px rgba(0,41,103,0.15);
    transform: translateY(-2px);
  }
  .chart-btn-wrapper .chart-label {
    font-weight: 600; font-size: 0.85rem; color: ", NAVY, ";
    margin-top: 6px; margin-bottom: 2px;
  }
  .chart-btn-wrapper .btn { width: 100%; border: none; background: transparent; padding: 0; }
  .chart-btn-wrapper .btn:focus { outline: none; box-shadow: none; }

  /* Selected states */
  .chart-btn-correct {
    border-color: #16a34a !important;
    background: #f0fdf4 !important;
    box-shadow: 0 0 0 3px rgba(22,163,74,0.25) !important;
  }
  .chart-btn-wrong {
    border-color: ", RED, " !important;
    background: #fef2f2 !important;
    box-shadow: 0 0 0 3px rgba(196,30,58,0.25) !important;
  }
  .chart-btn-disabled {
    pointer-events: none;
    opacity: 0.6;
  }

  /* Feedback card */
  .feedback-correct {
    background: #f0fdf4;
    border-left: 5px solid #16a34a;
    padding: 14px 18px;
    border-radius: 6px;
    margin-top: 12px;
  }
  .feedback-correct .fb-label {
    color: #16a34a; font-weight: 700; font-size: 1.0rem; margin-bottom: 6px;
  }
  .feedback-wrong {
    background: #fef2f2;
    border-left: 5px solid ", RED, ";
    padding: 14px 18px;
    border-radius: 6px;
    margin-top: 12px;
  }
  .feedback-wrong .fb-label {
    color: ", RED, "; font-weight: 700; font-size: 1.0rem; margin-bottom: 6px;
  }
  .fb-explanation {
    font-size: 0.9rem; color: #334155; line-height: 1.6;
  }

  /* Progress bar custom */
  .progress { height: 22px; border-radius: 11px; background: #e2e8f0; }
  .progress-bar {
    background: linear-gradient(90deg, ", NAVY, ", ", GOLD, ") !important;
    border-radius: 11px;
    font-weight: 600;
    font-size: 0.8rem;
  }

  /* Score display */
  .score-box {
    background: ", NAVY, ";
    color: white;
    border-radius: 8px;
    padding: 10px 14px;
    text-align: center;
    font-size: 1.1rem;
    font-weight: 700;
  }
  .score-box .score-label {
    font-size: 0.75rem; font-weight: 400; color: ", GOLD, ";
    text-transform: uppercase; letter-spacing: 1px;
  }

  /* Next button */
  .btn-next {
    background: ", NAVY, " !important;
    color: white !important;
    border: none !important;
    font-weight: 600 !important;
    padding: 10px 28px !important;
    border-radius: 6px !important;
    margin-top: 12px !important;
  }
  .btn-next:hover {
    background: #003d8f !important;
  }

  /* Completion report */
  .completion-card {
    border: 2px solid ", NAVY, ";
    border-radius: 10px;
    overflow: hidden;
    max-width: 750px;
    margin: 0 auto;
  }
  .completion-header {
    background: ", NAVY, ";
    color: white;
    padding: 16px 22px;
    text-align: center;
  }
  .completion-header h3 { margin: 0 0 4px 0; font-weight: 700; }
  .completion-header p { margin: 0; color: ", GOLD, "; font-size: 0.9rem; }
  .completion-body { padding: 20px 24px; }
  .completion-body table {
    width: 100%; border-collapse: collapse; font-size: 0.88rem; margin: 14px 0;
  }
  .completion-body th {
    background: #f1f5f9; color: ", NAVY, "; padding: 8px 10px;
    text-align: left; border-bottom: 2px solid ", NAVY, ";
    font-size: 0.82rem; text-transform: uppercase; letter-spacing: 0.3px;
  }
  .completion-body td {
    padding: 7px 10px; border-bottom: 1px solid #e2e8f0;
  }
  .result-correct { color: #16a34a; font-weight: 600; }
  .result-wrong { color: ", RED, "; font-weight: 600; }

  /* Reflection */
  .reflection-box {
    background: #fdf8f0;
    border-left: 4px solid ", GOLD, ";
    padding: 14px 18px;
    border-radius: 4px;
    margin-top: 16px;
  }
  .reflection-box .ref-title {
    color: ", GOLD, "; font-weight: 700; font-size: 0.92rem; margin-bottom: 6px;
  }
  .reflection-box p {
    font-size: 0.88rem; color: #4a3c28; line-height: 1.6; margin: 0;
    font-style: italic;
  }

  /* Hash code */
  .hash-box {
    background: #f1f5f9; border: 1px dashed ", NAVY, ";
    padding: 10px 14px; border-radius: 6px; margin-top: 14px;
    text-align: center;
  }
  .hash-code {
    font-family: 'Fira Code', 'Consolas', monospace;
    font-size: 1.0rem; font-weight: 700; color: ", NAVY, ";
    letter-spacing: 1px;
  }

  /* Sidebar helper */
  .sidebar-section-label {
    font-size: 0.78rem; color: #64748b; text-transform: uppercase;
    letter-spacing: 0.5px; font-weight: 600; margin-bottom: 6px;
  }

  /* Footer */
  .app-footer {
    text-align: center; padding: 10px 0 6px 0; margin-top: 12px;
    font-size: 0.78rem; color: #64748b; border-top: 1px solid #e2e8f0;
  }
")

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

  # Head: shinyjs + custom CSS + clipboard JS
  tags$head(
    tags$style(HTML(app_css)),
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

  # --- Sidebar ---
  sidebar = sidebar(
    width = 290, bg = "#f1f5f9",

    # Instruction accordion
    accordion(
      id = "help_acc",
      open = FALSE,
      accordion_panel(
        title = "How to Play",
        icon  = bsicons::bs_icon("info-circle"),
        tags$ol(
          style = "font-size: 0.82rem; color: #374151; line-height: 1.6; padding-left: 18px;",
          tags$li("Enter your name and click ", tags$strong("Start Game"), "."),
          tags$li("Read the business question and data description."),
          tags$li("Select the ", tags$strong("best chart type"), " from the 7 options."),
          tags$li("Review the feedback and see the correct chart."),
          tags$li("Click ", tags$strong("Next Question"), " to continue."),
          tags$li("After 10 questions, view your ", tags$strong("Completion Report"), "."),
          tags$li("Write a brief ", tags$strong("reflection"), " before copying your report.")
        ),
        p("The right chart for the right question!",
          style = "font-size: 0.82rem; color: #475569; font-style: italic; margin-top: 6px;"),
        style = "font-size: 0.82rem; color: #374151; line-height: 1.55;"
      )
    ),

    tags$hr(style = "margin: 8px 0;"),

    # Player name
    tags$div(
      class = "sidebar-section-label",
      "Player"
    ),
    textInput("player_name", NULL, placeholder = "Enter your name"),

    # Start button
    actionButton("start_btn", "Start Game",
                 icon = icon("play"),
                 class = "btn-primary w-100",
                 style = "margin-bottom: 14px;"),

    tags$hr(style = "margin: 8px 0;"),

    # Progress
    tags$div(
      class = "sidebar-section-label",
      "Progress"
    ),
    uiOutput("progress_bar_ui"),

    tags$div(style = "height: 10px;"),

    # Score
    uiOutput("score_ui")
  ),

  # --- Main panel ---
  # Game header
  tags$div(
    class = "game-header",
    tags$h2("Chart Matchmaker"),
    tags$p(class = "tagline", "The right chart for the right question.")
  ),

  # Game content (scenario or completion)
  uiOutput("game_content"),

  # Footer
  tags$footer(
    class = "app-footer",
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

  # ---- Reactive values ----
  rv <- reactiveValues(
    game_started  = FALSE,
    current_q     = 0,
    score         = 0,
    answered      = FALSE,
    selected      = NULL,   # which chart the student picked
    order         = NULL,   # randomised scenario order
    results       = list(), # per-question results
    game_complete = FALSE
  )

  # ---- Pre-render thumbnail plots at session start ----
  # Store as grobs for faster rendering
  thumbnail_plots <- list()
  for (ct in CHART_IDS) {
    thumbnail_plots[[ct]] <- make_thumbnail(ct)
  }

  # ---- Render thumbnails as output plots ----
  lapply(CHART_IDS, function(ct) {
    local({
      my_ct <- ct
      output_id <- paste0("thumb_", my_ct)
      output[[output_id]] <- renderPlot({
        thumbnail_plots[[my_ct]]
      }, res = 96, height = 120, width = 160)
    })
  })

  # ---- Start game ----
  observeEvent(input$start_btn, {
    req(nchar(trimws(input$player_name)) > 0)
    rv$game_started  <- TRUE
    rv$current_q     <- 1
    rv$score         <- 0
    rv$answered      <- FALSE
    rv$selected      <- NULL
    rv$order         <- sample(length(SCENARIOS))
    rv$results       <- list()
    rv$game_complete <- FALSE
  })

  # ---- Current scenario ----
  current_scenario <- reactive({
    req(rv$game_started, rv$current_q > 0, rv$current_q <= length(SCENARIOS))
    SCENARIOS[[rv$order[rv$current_q]]]
  })

  # ---- Observe chart button clicks ----
  lapply(CHART_IDS, function(ct) {
    local({
      my_ct <- ct
      btn_id <- paste0("btn_", my_ct)
      observeEvent(input[[btn_id]], {
        req(rv$game_started, !rv$answered, rv$current_q > 0)
        rv$selected <- my_ct
        rv$answered <- TRUE

        sc <- current_scenario()
        is_correct <- (my_ct == sc$answer)

        if (is_correct) {
          rv$score <- rv$score + 10
        }

        # Store result
        rv$results[[length(rv$results) + 1]] <- list(
          q_num       = rv$current_q,
          scenario_id = sc$id,
          question    = sc$question,
          answer      = sc$answer,
          selected    = my_ct,
          correct     = is_correct
        )

        # Add CSS classes to buttons
        correct_wrapper <- paste0("wrapper_", sc$answer)
        if (is_correct) {
          shinyjs::addClass(correct_wrapper, "chart-btn-correct")
        } else {
          selected_wrapper <- paste0("wrapper_", my_ct)
          shinyjs::addClass(selected_wrapper, "chart-btn-wrong")
          shinyjs::addClass(correct_wrapper, "chart-btn-correct")
        }

        # Disable all buttons
        for (cid in CHART_IDS) {
          shinyjs::addClass(paste0("wrapper_", cid), "chart-btn-disabled")
        }
      }, ignoreInit = TRUE)
    })
  })

  # ---- Next question ----
  observeEvent(input$next_btn, {
    if (rv$current_q >= length(SCENARIOS)) {
      rv$game_complete <- TRUE
    } else {
      rv$current_q <- rv$current_q + 1
      rv$answered  <- FALSE
      rv$selected  <- NULL

      # Reset button styles
      for (cid in CHART_IDS) {
        shinyjs::removeClass(paste0("wrapper_", cid), "chart-btn-correct")
        shinyjs::removeClass(paste0("wrapper_", cid), "chart-btn-wrong")
        shinyjs::removeClass(paste0("wrapper_", cid), "chart-btn-disabled")
      }
    }
  })

  # ---- Progress bar ----
  output$progress_bar_ui <- renderUI({
    if (!rv$game_started) {
      tags$div(
        class = "progress",
        tags$div(class = "progress-bar", role = "progressbar",
                 style = "width: 0%;", "0 / 10")
      )
    } else {
      answered_count <- length(rv$results)
      pct <- round(answered_count / length(SCENARIOS) * 100)
      tags$div(
        class = "progress",
        tags$div(class = "progress-bar", role = "progressbar",
                 style = paste0("width: ", pct, "%;"),
                 paste0(answered_count, " / ", length(SCENARIOS)))
      )
    }
  })

  # ---- Score display ----
  output$score_ui <- renderUI({
    tags$div(
      class = "score-box",
      tags$div(class = "score-label", "Score"),
      paste0(rv$score, " / ", length(SCENARIOS) * 10)
    )
  })

  # ---- Main game content ----
  output$game_content <- renderUI({
    if (!rv$game_started) {
      # Welcome screen
      tags$div(
        style = "text-align: center; padding: 40px 20px;",
        tags$div(
          style = paste0("background: #f0f4ff; border-radius: 12px; padding: 30px; ",
                         "max-width: 600px; margin: 0 auto;"),
          tags$h4(style = paste0("color: ", NAVY, "; font-weight: 700;"),
                  "Welcome to Chart Matchmaker!"),
          tags$p(style = "color: #475569; font-size: 1.0rem; line-height: 1.6;",
                 "You will be presented with 10 business scenarios. For each one,",
                 " your job is to choose the best chart type from 7 options."),
          tags$p(style = "color: #475569; font-size: 1.0rem; line-height: 1.6;",
                 "Each correct answer earns ", tags$strong("10 points"),
                 ". Can you get a perfect 100?"),
          tags$hr(style = "border-color: #cbd5e1;"),
          tags$p(style = paste0("color: ", GOLD, "; font-weight: 600;"),
                 icon("arrow-left"),
                 " Enter your name in the sidebar and click Start Game to begin.")
        )
      )
    } else if (rv$game_complete) {
      # Completion report
      build_completion_report()
    } else {
      # Active game question
      build_question_ui()
    }
  })

  # ---- Build question UI ----
  build_question_ui <- function() {
    sc <- current_scenario()

    # Scenario card
    scenario_card <- tags$div(
      class = "scenario-card",
      tags$div(class = "q-label",
               paste0("Question ", rv$current_q, " of ", length(SCENARIOS))),
      tags$div(class = "q-text", sc$question),
      tags$p(class = "q-data",
             icon("database"), " ", sc$data_desc)
    )

    # Chart button grid
    chart_buttons <- layout_column_wrap(
      width = 1/3,
      gap = "10px",
      !!!lapply(CHART_IDS, function(ct) {
        tags$div(
          id = paste0("wrapper_", ct),
          class = "chart-btn-wrapper",
          actionButton(
            paste0("btn_", ct),
            label = tagList(
              plotOutput(paste0("thumb_", ct), height = "120px", width = "160px"),
              tags$div(class = "chart-label", CHART_NAMES[ct])
            ),
            style = "width:100%; border:none; background:transparent; padding:4px;"
          )
        )
      })
    )

    # Feedback area
    feedback_ui <- uiOutput("feedback_area")

    # Correct chart display
    correct_chart_ui <- uiOutput("correct_chart_area")

    # Next button
    next_btn_ui <- conditionalPanel(
      condition = "true",
      uiOutput("next_btn_ui")
    )

    tagList(
      scenario_card,
      tags$h6(style = paste0("color: ", NAVY, "; font-weight: 700; margin-bottom: 8px;"),
              icon("chart-bar"), " Select the best chart type:"),
      chart_buttons,
      feedback_ui,
      correct_chart_ui,
      next_btn_ui
    )
  }

  # ---- Feedback area ----
  output$feedback_area <- renderUI({
    req(rv$answered)
    sc <- current_scenario()
    sel <- rv$selected

    if (sel == sc$answer) {
      tags$div(
        class = "feedback-correct",
        tags$div(class = "fb-label",
                 icon("check-circle"), " Correct! +10 points"),
        tags$p(class = "fb-explanation", sc$correct_explanation)
      )
    } else {
      # Build wrong explanation
      wrong_key <- sel
      wrong_text <- sc$wrong_explanations[[wrong_key]]
      if (is.null(wrong_text)) wrong_text <- "This chart type is not the best fit for this scenario."

      tagList(
        tags$div(
          class = "feedback-wrong",
          tags$div(class = "fb-label",
                   icon("times-circle"),
                   paste0(" Not quite. You chose ", CHART_NAMES[sel], ".")),
          tags$p(class = "fb-explanation", wrong_text)
        ),
        tags$div(
          class = "feedback-correct",
          style = "margin-top: 8px;",
          tags$div(class = "fb-label",
                   icon("check-circle"),
                   paste0(" The best choice: ", CHART_NAMES[sc$answer])),
          tags$p(class = "fb-explanation", sc$correct_explanation)
        )
      )
    }
  })

  # ---- Correct chart full display ----
  output$correct_chart_plot <- renderPlot({
    req(rv$answered)
    sc <- current_scenario()
    make_full_chart(sc$id)
  }, res = 110, height = 380)

  output$correct_chart_area <- renderUI({
    req(rv$answered)
    sc <- current_scenario()
    card(
      style = "margin-top: 14px;",
      card_header(
        paste0("Correct Chart: ", CHART_NAMES[sc$answer], " (with sample data)"),
        class = "bg-primary text-white"
      ),
      card_body(
        with_spinner(plotOutput("correct_chart_plot", height = "380px"))
      )
    )
  })

  # ---- Next button ----
  output$next_btn_ui <- renderUI({
    req(rv$answered)
    btn_label <- if (rv$current_q >= length(SCENARIOS)) {
      "View Results"
    } else {
      "Next Question"
    }
    tags$div(
      style = "text-align: center; margin-top: 14px; margin-bottom: 20px;",
      actionButton("next_btn", btn_label,
                   icon = icon("arrow-right"),
                   class = "btn-next")
    )
  })

  # ---- Completion report ----
  build_completion_report <- function() {
    player <- trimws(input$player_name)
    total <- length(SCENARIOS) * 10
    pct <- round(rv$score / total * 100)

    # Grade
    grade <- if (pct >= 90) "A" else if (pct >= 80) "B" else if (pct >= 70) "C" else if (pct >= 60) "D" else "F"

    # Hash code
    hash_input <- paste0(player, "-", rv$score, "-", Sys.Date(), "-chartmatch")
    if (requireNamespace("digest", quietly = TRUE)) {
      hash_code <- toupper(substring(digest::digest(hash_input, algo = "md5"), 1, 12))
    } else {
      hash_code <- toupper(format(as.hexmode(
        abs(sum(utf8ToInt(hash_input)) * 7919 + rv$score * 104729) %% .Machine$integer.max
      ), width = 12))
      hash_code <- substring(paste0(hash_code, "ABCDEF12345"), 1, 12)
    }

    # Build results table rows
    result_rows <- lapply(rv$results, function(r) {
      sc <- SCENARIOS[[r$scenario_id]]
      status_class <- if (r$correct) "result-correct" else "result-wrong"
      status_text  <- if (r$correct) "Correct" else "Wrong"
      tags$tr(
        tags$td(r$q_num),
        tags$td(style = "max-width: 300px;", sc$question),
        tags$td(CHART_NAMES[sc$answer]),
        tags$td(CHART_NAMES[r$selected]),
        tags$td(class = status_class, status_text)
      )
    })

    # Build plain-text report for clipboard
    report_lines_txt <- c(
      "===== CHART MATCHMAKER -- COMPLETION REPORT =====",
      paste0("Player:    ", player),
      paste0("Date:      ", format(Sys.Date(), "%B %d, %Y")),
      paste0("Score:     ", rv$score, " / ", length(SCENARIOS) * 10, "  (", pct, "%)"),
      paste0("Grade:     ", grade),
      paste0("Hash:      ", hash_code),
      ""
    )
    for (r in rv$results) {
      sc <- SCENARIOS[[r$scenario_id]]
      status <- if (r$correct) "Correct" else "Wrong"
      report_lines_txt <- c(report_lines_txt,
        sprintf("Q%d: %s | Answer: %s | You: %s | %s",
                r$q_num, substr(sc$question, 1, 50), CHART_NAMES[sc$answer], CHART_NAMES[r$selected], status))
    }
    report_text <- paste(report_lines_txt, collapse = "\n")

    tagList(
      tags$div(
        class = "completion-card",

        # Header
        tags$div(
          class = "completion-header",
          tags$h3(icon("trophy"), " Game Complete!"),
          tags$p(paste0(player, " | ", format(Sys.Date(), "%B %d, %Y")))
        ),

        # Body
        tags$div(
          class = "completion-body",

          # Score summary
          tags$div(
            style = "text-align: center; margin-bottom: 18px;",
            tags$h2(style = paste0("color: ", NAVY, "; margin: 0;"),
                    paste0(rv$score, " / ", total)),
            tags$p(style = "color: #64748b; font-size: 0.95rem; margin: 4px 0;",
                   paste0(pct, "% | Grade: ", grade))
          ),

          # Results table
          tags$table(
            tags$thead(
              tags$tr(
                tags$th("#"),
                tags$th("Scenario"),
                tags$th("Correct Answer"),
                tags$th("Your Answer"),
                tags$th("Result")
              )
            ),
            tags$tbody(result_rows)
          ),

          # Hash code
          tags$div(
            class = "hash-box",
            tags$div(style = "font-size: 0.78rem; color: #64748b; margin-bottom: 4px;",
                     "Verification Code"),
            tags$div(class = "hash-code", id = "hash-display", hash_code),
            tags$button(
              id = "copy-hash-btn",
              class = "btn btn-sm btn-outline-primary",
              style = "margin-top: 8px;",
              onclick = paste0(
                "var code = document.getElementById('hash-display').innerText;",
                "navigator.clipboard.writeText(code).then(function(){",
                "document.getElementById('copy-hash-btn').innerText='Copied!';",
                "setTimeout(function(){document.getElementById('copy-hash-btn').innerText='Copy Code';},2000);",
                "});"
              ),
              icon("copy"), " Copy Code"
            )
          ),

          # Reflection input
          tags$div(
            class = "reflection-box",
            tags$label(
              "for" = "reflection_input",
              style = "display:block; margin-bottom:8px; font-size:0.9rem;",
              tags$strong("Ignatian Reflection: "),
              "Why does choosing the right chart type matter for the people whose stories the data represents?"
            ),
            tags$textarea(
              id = "reflection_input",
              class = "form-control",
              rows = "3",
              placeholder = "Write 1-2 sentences here...",
              style = "font-size:0.88rem; resize:vertical;"
            )
          ),

          # Copy full report button
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

          # Play again button
          tags$div(
            style = "text-align: center; margin-top: 20px;",
            actionButton("play_again_btn", "Play Again",
                         icon = icon("rotate-right"),
                         class = "btn-next")
          )
        )
      )
    )
  }

  # ---- Play again ----
  observeEvent(input$play_again_btn, {
    rv$game_started  <- FALSE
    rv$current_q     <- 0
    rv$score         <- 0
    rv$answered      <- FALSE
    rv$selected      <- NULL
    rv$order         <- NULL
    rv$results       <- list()
    rv$game_complete <- FALSE

    # Reset button styles
    for (cid in CHART_IDS) {
      shinyjs::removeClass(paste0("wrapper_", cid), "chart-btn-correct")
      shinyjs::removeClass(paste0("wrapper_", cid), "chart-btn-wrong")
      shinyjs::removeClass(paste0("wrapper_", cid), "chart-btn-disabled")
    }
  })
}

# ---------------------------------------------------------------------------
shinyApp(ui, server)
