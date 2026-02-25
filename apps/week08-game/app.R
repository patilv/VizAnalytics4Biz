library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(patchwork)

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
# Shared base theme for dashboard panels
# ---------------------------------------------------------------------------
base_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      axis.title = element_text(size = 10),
      legend.position = "bottom"
    )
}

# ---------------------------------------------------------------------------
# Synthetic data generation (set seed for reproducibility)
# ---------------------------------------------------------------------------
set.seed(42)

# --- Dashboard 1 data: Monthly Sales Performance ---
categories <- c("Electronics", "Apparel", "Home & Garden", "Sports", "Books", "Food & Bev")
months_lab <- month.abb

sales_cat <- data.frame(
  category = factor(categories, levels = categories),
  revenue = c(425000, 310000, 275000, 195000, 148000, 220000)
)

monthly_trend <- data.frame(
  month = factor(months_lab, levels = months_lab),
  revenue = c(120, 135, 148, 162, 155, 178, 192, 205, 198, 215, 230, 248) * 1000
)

salespeople <- data.frame(
  name = paste0("Rep ", LETTERS[1:10]),
  sales = sort(runif(10, 80000, 260000), decreasing = TRUE)
)

# --- Dashboard 2 data: Customer Satisfaction Survey ---
sat_cats <- c("Very Satisfied", "Satisfied", "Neutral", "Dissatisfied", "Very Dissatisfied")
sat_data <- data.frame(
  category = factor(sat_cats, levels = sat_cats),
  pct = c(32, 28, 22, 12, 6)
)

sat_scatter <- data.frame(
  satisfaction = runif(80, 1, 10),
  retention = NA
)
sat_scatter$retention <- 30 + 6 * sat_scatter$satisfaction + rnorm(80, 0, 8)
sat_scatter$retention <- pmax(10, pmin(100, sat_scatter$retention))

gauge_data <- data.frame(
  metric = c("NPS Score", "Response Rate", "Avg Rating"),
  value = c(72, 64, 8.1),
  max_val = c(100, 100, 10)
)

# --- Dashboard 3 data: Employee Wellness Program ---
departments <- c("Engineering", "Marketing", "Sales", "HR", "Finance", "Operations")

wellness_ba <- expand.grid(
  department = departments,
  period = c("Before", "After"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    score = ifelse(
      period == "Before",
      round(runif(length(departments), 45, 65)),
      round(runif(length(departments), 62, 85))
    )
  ) %>%
  mutate(period = factor(period, levels = c("Before", "After")))

set.seed(123)
violin_data <- do.call(rbind, lapply(departments, function(d) {
  n <- 40
  data.frame(
    department = d,
    score = rnorm(n, mean = sample(60:78, 1), sd = sample(5:12, 1))
  )
}))

set.seed(456)
heat_months <- month.abb[1:12]
heatmap_data <- expand.grid(
  month = factor(heat_months, levels = heat_months),
  department = departments,
  stringsAsFactors = FALSE
) %>%
  mutate(participation = round(runif(n(), 40, 95)))

enrollment_trend <- data.frame(
  month = factor(heat_months, levels = heat_months),
  enrolled = cumsum(c(45, 22, 18, 30, 15, 25, 20, 12, 28, 10, 8, 14))
)

# --- Dashboard 4 data: Website Traffic Analysis ---
set.seed(789)
traffic_months <- month.abb
channels <- c("Organic", "Paid", "Social", "Referral", "Direct", "Email")
area_data <- do.call(rbind, lapply(channels, function(ch) {
  data.frame(
    month = factor(traffic_months, levels = traffic_months),
    channel = ch,
    visits = round(runif(12, 5000, 30000))
  )
}))

dual_y_data <- data.frame(
  month = factor(traffic_months, levels = traffic_months),
  visitors = c(45, 52, 58, 63, 70, 68, 74, 82, 78, 85, 90, 95) * 1000,
  bounce_rate = c(42, 40, 38, 41, 35, 37, 33, 30, 32, 28, 27, 25)
)

# Truncated axis data
bar_trunc <- data.frame(
  page = paste("Page", LETTERS[1:6]),
  views = c(9200, 9450, 9800, 10100, 9600, 9350)
)

# 3D bar data
bar_3d <- data.frame(
  source = c("Google", "Bing", "Yahoo", "DuckDuckGo"),
  sessions = c(45000, 12000, 8000, 5000)
)

# --- Dashboard 5 data: Supply Chain Metrics ---
warehouses <- c("East", "West", "Central", "South")
products <- c("Raw Material", "WIP", "Finished Goods")

stacked_supply <- expand.grid(
  warehouse = warehouses,
  product = factor(products, levels = products),
  stringsAsFactors = FALSE
) %>%
  mutate(inventory = round(runif(n(), 200, 1200)))

delivery_data <- data.frame(
  month = factor(traffic_months, levels = traffic_months),
  days = c(5.2, 4.8, 5.1, 4.5, 4.2, 4.0, 3.8, 3.9, 4.1, 3.7, 3.5, 3.6)
)

heat_supply <- expand.grid(
  warehouse = warehouses,
  metric = c("On-Time %", "Defect Rate", "Fill Rate", "Lead Time", "Cost/Unit"),
  stringsAsFactors = FALSE
) %>%
  mutate(value = runif(n(), 0, 100))

metrics_table <- data.frame(
  Metric = c("Total Inventory Units", "Avg Delivery Days", "On-Time Delivery %",
             "Order Fill Rate", "Defect Rate", "Warehouses Active"),
  Value = c("42,850", "4.12", "91.3%", "96.7%", "1.8%", "4")
)

# --- Dashboard 6 data: Public Health Outcomes ---
regions <- c("Northeast", "Southeast", "Midwest", "Southwest", "West", "Pacific NW")

facet_bar_data <- data.frame(
  region = rep(regions, each = 4),
  condition = rep(c("Heart Disease", "Diabetes", "Obesity", "Respiratory"), times = 6),
  rate = round(runif(24, 5, 30), 1)
)

slope_data <- data.frame(
  condition = rep(c("Heart Disease", "Diabetes", "Obesity", "Respiratory"), each = 2),
  year = rep(c("2020", "2024"), times = 4),
  rate = c(22.1, 19.4, 14.3, 15.8, 31.2, 28.5, 12.7, 10.9)
)

diverging_data <- data.frame(
  region = regions,
  change = c(-3.2, 1.5, -4.8, 0.8, -2.1, -5.3)
) %>%
  mutate(
    direction = ifelse(change >= 0, "Worsened", "Improved"),
    region = factor(region, levels = region[order(change)])
  )

# ============================================================================
# DASHBOARD BUILDER FUNCTIONS
# ============================================================================

make_dashboard_1 <- function() {
  # Monthly Sales Performance (GOOD)
  prof_colors <- c("#2C6FAC", "#3D8FD1", "#5AACE0", "#7BC4EA", "#A2D8F0", "#C8E9F7")

  p1 <- ggplot(sales_cat, aes(x = reorder(category, revenue), y = revenue, fill = category)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    scale_fill_manual(values = prof_colors) +
    scale_y_continuous(labels = function(x) paste0("$", x / 1000, "K")) +
    coord_flip() +
    labs(title = "Revenue by Product Category", x = NULL, y = "Revenue") +
    base_theme()

  p2 <- ggplot(monthly_trend, aes(x = month, y = revenue, group = 1)) +
    geom_line(colour = "#2C6FAC", linewidth = 1.2) +
    geom_point(colour = "#2C6FAC", size = 2.5) +
    scale_y_continuous(labels = function(x) paste0("$", x / 1000, "K")) +
    labs(title = "Monthly Revenue Trend", x = NULL, y = "Revenue") +
    base_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

  p3 <- ggplot(salespeople, aes(x = reorder(name, sales), y = sales)) +
    geom_segment(aes(xend = name, y = 0, yend = sales), colour = "grey60", linewidth = 0.8) +
    geom_point(colour = "#2C6FAC", size = 3.5) +
    scale_y_continuous(labels = function(x) paste0("$", round(x / 1000), "K")) +
    coord_flip() +
    labs(title = "Top 10 Salespeople", x = NULL, y = "Total Sales") +
    base_theme()

  p4_text <- paste0(
    "TOTAL REVENUE:  $1,573K\n",
    "YoY GROWTH:  +14.2%\n",
    "AVG DEAL SIZE:  $8,450\n",
    "ACTIVE ACCOUNTS:  186"
  )
  p4 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = p4_text,
             size = 4.5, hjust = 0.5, vjust = 0.5, fontface = "bold",
             colour = "#2C6FAC", lineheight = 1.8) +
    theme_void() +
    labs(title = "Key Performance Indicators") +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.background = element_rect(fill = "#F0F4F8", colour = NA)
    )

  (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = "Monthly Sales Performance Dashboard",
      subtitle = "FY 2025 | All Regions Combined",
      theme = theme(
        plot.title = element_text(face = "bold", size = 16, colour = "#2C3E50"),
        plot.subtitle = element_text(size = 11, colour = "grey50")
      )
    )
}

make_dashboard_2 <- function() {
  # Customer Satisfaction Survey (MEDIOCRE)

  # Panel 1: Pie chart (poor choice for comparison)
  sat_data$ymax <- cumsum(sat_data$pct)
  sat_data$ymin <- c(0, head(sat_data$ymax, -1))
  sat_data$mid <- (sat_data$ymax + sat_data$ymin) / 2

  p1 <- ggplot(sat_data, aes(x = 2, y = pct, fill = category)) +
    geom_col(width = 1, colour = "white", linewidth = 2) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = c("#2ecc71", "#82e0aa", "#f7dc6f", "#e59866", "#e74c3c")) +
    labs(title = "Satisfaction Breakdown", fill = NULL) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "right",
      legend.text = element_text(size = 8)
    )

  # Panel 2: Bars with heavy gridlines and borders
  sat_bar <- data.frame(
    quarter = c("Q1", "Q2", "Q3", "Q4"),
    score = c(7.2, 7.5, 7.8, 7.6)
  )
  p2 <- ggplot(sat_bar, aes(x = quarter, y = score)) +
    geom_col(fill = "#3498db", width = 0.6) +
    labs(title = "Quarterly Avg Score", x = NULL, y = "Score (1-10)") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      panel.grid.major = element_line(colour = "grey30", linewidth = 0.8),
      panel.grid.minor = element_line(colour = "grey50", linewidth = 0.5),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
      axis.title = element_text(size = 10)
    )

  # Panel 3: Good scatter plot
  p3 <- ggplot(sat_scatter, aes(x = satisfaction, y = retention)) +
    geom_point(alpha = 0.6, colour = "#2C6FAC", size = 2) +
    geom_smooth(method = "lm", se = TRUE, colour = "#C41E3A", linewidth = 0.8) +
    labs(
      title = "Satisfaction vs Retention",
      x = "Satisfaction Score",
      y = "Retention Rate (%)"
    ) +
    base_theme()

  # Panel 4: Overly decorated gauge-style chart
  p4 <- ggplot(gauge_data, aes(x = metric, y = value)) +
    geom_col(fill = c("#e74c3c", "#f39c12", "#2ecc71"), width = 0.5) +
    geom_col(aes(y = max_val), fill = NA, colour = "black", linewidth = 1.2, width = 0.5) +
    geom_text(aes(label = value), vjust = -0.5, fontface = "bold", size = 5, colour = "black") +
    labs(title = "Key Metrics", x = NULL, y = "Value") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      panel.grid.major.x = element_blank(),
      panel.background = element_rect(fill = "#FFF8E7", colour = "gold", linewidth = 2),
      plot.background = element_rect(fill = "#FFFDF0", colour = NA)
    )

  (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = "Customer Satisfaction Survey Results",
      subtitle = "Annual Survey 2025 | N = 1,247 respondents",
      theme = theme(
        plot.title = element_text(face = "bold", size = 16, colour = "#2C3E50"),
        plot.subtitle = element_text(size = 11, colour = "grey50")
      )
    )
}

make_dashboard_3 <- function() {
  # Employee Wellness Program (GOOD)

  well_colors <- c("Before" = "#B0BEC5", "After" = "#26A69A")

  p1 <- ggplot(wellness_ba, aes(x = department, y = score, fill = period)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    scale_fill_manual(values = well_colors) +
    labs(title = "Wellness Scores: Before vs After", x = NULL, y = "Avg Score", fill = NULL) +
    base_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))

  p2 <- ggplot(violin_data, aes(x = department, y = score, fill = department)) +
    geom_violin(alpha = 0.7, show.legend = FALSE) +
    geom_boxplot(width = 0.15, fill = "white", outlier.shape = NA) +
    scale_fill_brewer(palette = "Set3") +
    labs(title = "Score Distribution by Department", x = NULL, y = "Wellness Score") +
    base_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))

  p3 <- ggplot(heatmap_data, aes(x = month, y = department, fill = participation)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    scale_fill_gradient(low = "#E8F5E9", high = "#1B5E20", name = "Participation %") +
    labs(title = "Monthly Participation Heatmap", x = NULL, y = NULL) +
    base_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

  p4 <- ggplot(enrollment_trend, aes(x = month, y = enrolled, group = 1)) +
    geom_line(colour = "#26A69A", linewidth = 1.2) +
    geom_point(colour = "#26A69A", size = 2.5) +
    geom_area(alpha = 0.15, fill = "#26A69A") +
    labs(title = "Cumulative Program Enrollment", x = NULL, y = "Employees Enrolled") +
    base_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

  (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = "Employee Wellness Program Dashboard",
      subtitle = "2025 Program Year | 6 Departments | 247 Participants",
      theme = theme(
        plot.title = element_text(face = "bold", size = 16, colour = "#2C3E50"),
        plot.subtitle = element_text(size = 11, colour = "grey50")
      )
    )
}

make_dashboard_4 <- function() {
  # Website Traffic Analysis (POOR)

  # Panel 1: Rainbow-colored area chart
  rainbow_cols <- c("#FF0000", "#FF7F00", "#FFFF00", "#00FF00", "#0000FF", "#8B00FF")
  p1 <- ggplot(area_data, aes(x = month, y = visits, fill = channel, group = channel)) +
    geom_area(alpha = 0.85, position = "stack") +
    scale_fill_manual(values = rainbow_cols) +
    labs(title = "Traffic by Channel", x = NULL, y = "Visits", fill = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      legend.position = "right",
      panel.background = element_rect(fill = "#F5F5F5", colour = NA)
    )

  # Panel 2: Dual y-axis line chart (misleading)
  scale_factor <- max(dual_y_data$visitors) / max(dual_y_data$bounce_rate)
  p2 <- ggplot(dual_y_data, aes(x = month, group = 1)) +
    geom_line(aes(y = visitors), colour = "blue", linewidth = 1.2) +
    geom_line(aes(y = bounce_rate * scale_factor), colour = "red", linewidth = 1.2) +
    scale_y_continuous(
      name = "Visitors",
      labels = function(x) paste0(x / 1000, "K"),
      sec.axis = sec_axis(~ . / scale_factor, name = "Bounce Rate (%)")
    ) +
    labs(title = "Visitors vs Bounce Rate (Dual Axis)", x = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.title.y.left = element_text(colour = "blue"),
      axis.title.y.right = element_text(colour = "red")
    )

  # Panel 3: Truncated y-axis bar chart
  p3 <- ggplot(bar_trunc, aes(x = page, y = views)) +
    geom_col(fill = "#e74c3c", width = 0.6) +
    coord_cartesian(ylim = c(9000, 10200)) +
    labs(title = "Page Views (Note the Y-axis!)", x = NULL, y = "Views") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      panel.grid.major.x = element_blank()
    )

  # Panel 4: Simulated 3D bar chart with heavy decoration
  bar_3d_offset <- bar_3d %>%
    mutate(
      xnum = as.numeric(factor(source)),
      x_back = xnum + 0.15,
      y_back = sessions * 0.92
    )
  p4 <- ggplot() +
    geom_col(
      data = bar_3d_offset,
      aes(x = x_back, y = y_back),
      fill = "grey50", width = 0.55, alpha = 0.5
    ) +
    geom_col(
      data = bar_3d_offset,
      aes(x = xnum, y = sessions, fill = source),
      width = 0.55, show.legend = FALSE
    ) +
    scale_fill_manual(values = c("#FF6347", "#FFD700", "#00CED1", "#FF69B4")) +
    scale_x_continuous(
      breaks = bar_3d_offset$xnum,
      labels = bar_3d_offset$source
    ) +
    labs(title = "Sessions by Search Engine (3D)", x = NULL, y = "Sessions") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      panel.background = element_rect(fill = "#FFFDE7", colour = "orange", linewidth = 1),
      panel.grid = element_line(colour = "orange", linewidth = 0.3)
    )

  (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = "Website Traffic Analysis",
      subtitle = "2025 Performance Overview",
      theme = theme(
        plot.title = element_text(face = "bold", size = 16, colour = "#2C3E50"),
        plot.subtitle = element_text(size = 11, colour = "grey50")
      )
    )
}

make_dashboard_5 <- function() {
  # Supply Chain Metrics (MEDIOCRE)

  # Panel 1: Good stacked bar
  p1 <- ggplot(stacked_supply, aes(x = warehouse, y = inventory, fill = product)) +
    geom_col(width = 0.65) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Inventory by Warehouse", x = NULL, y = "Units", fill = NULL) +
    base_theme()

  # Panel 2: Reasonable line chart
  p2 <- ggplot(delivery_data, aes(x = month, y = days, group = 1)) +
    geom_line(colour = "#2C6FAC", linewidth = 1) +
    geom_point(colour = "#2C6FAC", size = 2.5) +
    labs(title = "Avg Delivery Time (Days)", x = NULL, y = "Days") +
    base_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

  # Panel 3: Heatmap with rainbow colors (bad for sequential data)
  p3 <- ggplot(heat_supply, aes(x = warehouse, y = metric, fill = value)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = round(value, 1)), size = 3, colour = "black") +
    scale_fill_gradientn(
      colours = c("purple", "blue", "cyan", "green", "yellow", "orange", "red"),
      name = "Value"
    ) +
    labs(title = "Performance Heatmap (Rainbow Scale)", x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 8)
    )

  # Panel 4: Text-heavy table instead of visualization
  table_text <- paste(
    apply(metrics_table, 1, function(r) paste0(r["Metric"], ":  ", r["Value"])),
    collapse = "\n"
  )
  p4 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = table_text,
             size = 3.8, hjust = 0.5, vjust = 0.5, fontface = "plain",
             colour = "black", lineheight = 2.0) +
    labs(title = "Key Supply Chain Metrics") +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      panel.border = element_rect(colour = "grey70", fill = NA, linewidth = 0.5),
      plot.background = element_rect(fill = "#FAFAFA", colour = NA)
    )

  (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = "Supply Chain Metrics Dashboard",
      subtitle = "Q4 2025 | 4 Regional Warehouses",
      theme = theme(
        plot.title = element_text(face = "bold", size = 16, colour = "#2C3E50"),
        plot.subtitle = element_text(size = 11, colour = "grey50")
      )
    )
}

make_dashboard_6 <- function() {
  # Public Health Outcomes (GOOD)

  # Panel 1: Faceted bar chart by region
  p1 <- ggplot(facet_bar_data, aes(x = condition, y = rate, fill = condition)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    facet_wrap(~ region, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("#5C6BC0", "#26A69A", "#EF5350", "#FFA726")) +
    labs(title = "Prevalence Rates by Region & Condition", x = NULL, y = "Rate per 1,000") +
    base_theme() +
    theme(
      axis.text.x = element_text(angle = 40, hjust = 1, size = 6),
      strip.text = element_text(face = "bold", size = 8)
    )

  # Panel 2: Slope chart comparing 2 time periods
  p2 <- ggplot(slope_data, aes(x = year, y = rate, group = condition, colour = condition)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_text(
      data = slope_data %>% filter(year == "2024"),
      aes(label = paste0(rate, "%")),
      hjust = -0.3, size = 3, show.legend = FALSE
    ) +
    geom_text(
      data = slope_data %>% filter(year == "2020"),
      aes(label = paste0(rate, "%")),
      hjust = 1.3, size = 3, show.legend = FALSE
    ) +
    scale_colour_manual(values = c("#5C6BC0", "#26A69A", "#EF5350", "#FFA726")) +
    labs(title = "Change in Prevalence: 2020 vs 2024", x = NULL, y = "Rate per 1,000", colour = NULL) +
    base_theme() +
    theme(legend.position = "bottom")

  # Panel 3: Diverging bar chart
  p3 <- ggplot(diverging_data, aes(x = region, y = change, fill = direction)) +
    geom_col(width = 0.6) +
    geom_hline(yintercept = 0, linewidth = 0.6) +
    scale_fill_manual(values = c("Improved" = "#26A69A", "Worsened" = "#EF5350")) +
    coord_flip() +
    labs(
      title = "Overall Health Outcome Change by Region",
      x = NULL, y = "Change in Composite Index", fill = NULL
    ) +
    base_theme()

  # Panel 4: Clean text annotations with source
  note_text <- paste0(
    "DATA SOURCES\n\n",
    "CDC National Health Statistics, 2024\n",
    "WHO Global Health Observatory\n",
    "Regional Health Department Reports\n\n",
    "METHODOLOGY\n\n",
    "Rates are age-adjusted per 1,000 population.\n",
    "Composite index: weighted average of 4 conditions.\n",
    "95% confidence intervals available in full report."
  )
  p4 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = note_text,
             size = 3.3, hjust = 0.5, vjust = 0.5, fontface = "italic",
             colour = "#37474F", lineheight = 1.5) +
    theme_void() +
    labs(title = "Notes & Data Sources") +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.background = element_rect(fill = "#F5F5F5", colour = "#B0BEC5", linewidth = 0.5)
    )

  (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = "Public Health Outcomes Dashboard",
      subtitle = "National Overview | 2020-2024 Comparison | 6 Regions",
      theme = theme(
        plot.title = element_text(face = "bold", size = 16, colour = "#2C3E50"),
        plot.subtitle = element_text(size = 11, colour = "grey50")
      )
    )
}

# ============================================================================
# Dashboard metadata & expert scores
# ============================================================================
dashboard_info <- list(
  list(
    id = 1,
    title = "Monthly Sales Performance",
    quality = "Good",
    make_fn = make_dashboard_1,
    expert = c(
      Appropriateness = 5, DataInk = 4, Color = 5, Narrative = 4, Ethics = 5
    ),
    expert_justifications = c(
      Appropriateness = "Bar, line, and lollipop charts are all excellent choices for the data types shown. KPIs are displayed as text, which is appropriate for single numbers.",
      DataInk = "Clean and efficient, though the KPI panel could be a bit more polished. Minimal chartjunk overall.",
      Color = "Consistent professional blue palette. Colors are accessible and meaningful without being distracting.",
      Narrative = "Good logical flow from category breakdown to trend to individual performance to KPIs. A clear story emerges.",
      Ethics = "All axes start at zero, data is presented honestly, and context (FY 2025, All Regions) is provided."
    )
  ),
  list(
    id = 2,
    title = "Customer Satisfaction Survey",
    quality = "Mediocre",
    make_fn = make_dashboard_2,
    expert = c(
      Appropriateness = 2, DataInk = 2, Color = 3, Narrative = 3, Ethics = 4
    ),
    expert_justifications = c(
      Appropriateness = "A pie chart is a poor choice for comparing satisfaction categories. A bar chart would allow easier comparison. The gauge chart is also an inefficient way to show 3 numbers.",
      DataInk = "Heavy gridlines and thick borders on the bar chart add visual clutter. The gauge panel has excessive decoration with colored backgrounds.",
      Color = "The color scheme is somewhat reasonable, though the pie chart colors could be more consistent. The scatter plot colors work well.",
      Narrative = "There is some logical grouping, but the dashboard lacks a clear narrative thread connecting satisfaction to business outcomes.",
      Ethics = "Data is presented honestly. Sample size is noted. No misleading scales or cherry-picking."
    )
  ),
  list(
    id = 3,
    title = "Employee Wellness Program",
    quality = "Good",
    make_fn = make_dashboard_3,
    expert = c(
      Appropriateness = 5, DataInk = 5, Color = 4, Narrative = 5, Ethics = 5
    ),
    expert_justifications = c(
      Appropriateness = "Grouped bar for before/after, violin for distributions, heatmap for two-dimensional patterns, and line for trends. All are ideal choices.",
      DataInk = "Very clean design. Minimal ink is wasted. Every visual element serves a purpose. Excellent data-ink ratio.",
      Color = "Good accessible palette. The before/after colors are intuitive (grey vs teal). The heatmap green gradient is appropriate for participation. Could be slightly more unified.",
      Narrative = "Excellent flow: Did the program work? (before/after) -> What do scores look like? (violin) -> Who participated when? (heatmap) -> How did enrollment grow? (trend). Very clear story.",
      Ethics = "Honest representation. Context about program year, departments, and participant count is provided. No misleading scales."
    )
  ),
  list(
    id = 4,
    title = "Website Traffic Analysis",
    quality = "Poor",
    make_fn = make_dashboard_4,
    expert = c(
      Appropriateness = 2, DataInk = 1, Color = 1, Narrative = 2, Ethics = 1
    ),
    expert_justifications = c(
      Appropriateness = "The dual y-axis chart is inherently misleading and is rarely an appropriate choice. The simulated 3D bar chart distorts perception of values.",
      DataInk = "Excessive decoration everywhere: colored backgrounds, orange gridlines, 3D shadow effects. Very poor data-ink ratio.",
      Color = "Rainbow colors on the area chart are a classic anti-pattern. They are not colorblind-safe, not ordered, and not meaningful. The 3D chart uses clashing garish colors.",
      Narrative = "Some panels relate logically, but the poor design choices make it hard to extract insights. The story gets lost in the visual noise.",
      Ethics = "The truncated y-axis on the bar chart makes small differences look enormous. The dual y-axis can create false correlations. These are serious ethical violations in data visualization."
    )
  ),
  list(
    id = 5,
    title = "Supply Chain Metrics",
    quality = "Mediocre",
    make_fn = make_dashboard_5,
    expert = c(
      Appropriateness = 3, DataInk = 3, Color = 2, Narrative = 3, Ethics = 4
    ),
    expert_justifications = c(
      Appropriateness = "The stacked bar and line chart are good choices. However, displaying a text table of metrics instead of visualizing them is a missed opportunity.",
      DataInk = "The stacked bar and line chart are clean. But the rainbow heatmap and text-heavy table reduce efficiency. Mixed quality.",
      Color = "The rainbow color scale on the heatmap is a major issue. Rainbow palettes distort perception of sequential data and are not colorblind-safe. Other panels use reasonable colors.",
      Narrative = "Some logical grouping exists, but the text table and rainbow heatmap disrupt the flow. The dashboard tells a partial story.",
      Ethics = "Data is presented honestly overall. No misleading axes or cherry-picking. Context about the time period and warehouses is provided."
    )
  ),
  list(
    id = 6,
    title = "Public Health Outcomes",
    quality = "Good",
    make_fn = make_dashboard_6,
    expert = c(
      Appropriateness = 5, DataInk = 4, Color = 5, Narrative = 5, Ethics = 5
    ),
    expert_justifications = c(
      Appropriateness = "Faceted bars for regional comparison, slope chart for change over time, diverging bars for improvement/decline. All are ideal for the story being told.",
      DataInk = "Clean and efficient. The annotations panel uses space well. Minor improvement possible in facet label sizing. Overall very good.",
      Color = "Consistent, accessible 4-color palette. The diverging bar uses green/red intuitively for improved/worsened. All panels share a cohesive look.",
      Narrative = "Excellent: Where are the problems? (faceted bars) -> Are things improving? (slope chart) -> Which regions improved? (diverging bars) -> Where does the data come from? (sources). Exemplary storytelling.",
      Ethics = "Rates are age-adjusted (noted). Data sources are cited. Methodology is explained. This is a model for ethical data communication."
    )
  )
)

criteria_names <- c("Appropriateness", "DataInk", "Color", "Narrative", "Ethics")

criteria_labels <- c(
  Appropriateness = "Chart Appropriateness",
  DataInk = "Data-Ink Ratio",
  Color = "Color Effectiveness",
  Narrative = "Narrative Clarity",
  Ethics = "Ethical Representation"
)

criteria_descriptions <- c(
  Appropriateness = "Are the right chart types used for the data and questions?",
  DataInk = "Is ink used efficiently? No chartjunk, unnecessary decoration, or 3D effects?",
  Color = "Are colors meaningful, accessible, and consistent?",
  Narrative = "Does the dashboard tell a clear story? Is there logical flow?",
  Ethics = "Is data shown honestly? No misleading scales, cherry-picking, or missing context?"
)

# Scoring function: exact = 5, within 1 = 3, within 2 = 1, else 0
score_criterion <- function(student, expert) {
  diff <- abs(student - expert)
  if (diff == 0) return(5)
  if (diff == 1) return(3)
  if (diff == 2) return(1)
  return(0)
}

# ============================================================================
# UI
# ============================================================================
ui <- page_sidebar(
  title = tags$span(
    style = paste0("color:white; font-weight:700; font-size:1.3rem;"),
    "Dashboard Jury"
  ),
  fillable = FALSE,
  theme = bs_theme(
    bootswatch = "flatly",
    primary   = NAVY,
    base_font = font_google("Inter")
  ),

  # Include shinyjs if available
  if (has_shinyjs) shinyjs::useShinyjs(),

  # --- Sidebar ---
  sidebar = sidebar(
    width = 320,
    bg = "#f8f9fa",

    # Instructions accordion
    accordion(
      open = FALSE,
      accordion_panel(
        title = "How to Play",
        icon = bsicons::bs_icon("info-circle"),
        tags$p(
          tags$strong("You are the client."),
          "You will view 6 dashboards and rate each on 5 criteria using sliders (1-5)."
        ),
        tags$p("After submitting your ratings, you will see how your scores compare to expert evaluations."),
        tags$p("The goal is to develop your critical eye for dashboard design by aligning your assessments with expert standards."),
        tags$hr(),
        tags$p(tags$strong("Scoring:")),
        tags$ul(
          tags$li("Exact match with expert: 5 pts"),
          tags$li("Within 1 of expert: 3 pts"),
          tags$li("Within 2 of expert: 1 pt"),
          tags$li("Off by 3 or more: 0 pts")
        ),
        tags$p("Maximum: 25 pts per dashboard, 150 pts total."),
        tags$p(tags$em("After all 6 dashboards, you'll write a brief reflection before copying your report."),
               style = "font-size:0.82rem; color:#475569; margin-top:6px;")
      )
    ),

    tags$hr(),

    # Player name
    textInput("player_name", "Your Name:", placeholder = "Enter your name"),

    # Start button
    actionButton("start_btn", "Start Game",
                 class = "btn-primary w-100 mb-3",
                 icon = icon("play")),

    tags$hr(),

    # Progress
    conditionalPanel(
      condition = "output.game_active",
      tags$div(
        tags$strong("Progress"),
        uiOutput("progress_bar"),
        tags$br(),
        tags$strong("Current Score: "),
        textOutput("current_score", inline = TRUE),
        tags$span(" / "),
        textOutput("max_so_far", inline = TRUE)
      ),
      tags$hr(),

      # Criteria reference
      tags$div(
        style = "font-size: 0.85rem;",
        tags$strong("Rating Criteria (1-5):"),
        tags$div(
          style = "margin-top:8px;",
          lapply(seq_along(criteria_names), function(i) {
            tags$div(
              style = "margin-bottom: 6px; padding: 4px 8px; background: white; border-radius: 4px; border-left: 3px solid #002967;",
              tags$strong(paste0(i, ". ", criteria_labels[criteria_names[i]])),
              tags$br(),
              tags$small(criteria_descriptions[criteria_names[i]])
            )
          })
        )
      )
    )
  ),

  # --- Main panel ---
  # Welcome card
  conditionalPanel(
    condition = "!output.game_active && !output.game_complete",
    card(
      card_header(
        class = "text-white",
        style = paste0("background-color:", NAVY, ";"),
        tags$h3(class = "mb-0", "Dashboard Jury")
      ),
      card_body(
        tags$p(class = "lead",
               tags$em("You are the client. Rate this dashboard.")),
        tags$p("In this game, you will review 6 dashboards and evaluate each on five key criteria of effective data visualization:"),
        tags$ol(
          tags$li(tags$strong("Chart Appropriateness"), " - Are the right chart types used?"),
          tags$li(tags$strong("Data-Ink Ratio"), " - Is ink used efficiently without chartjunk?"),
          tags$li(tags$strong("Color Effectiveness"), " - Are colors meaningful and accessible?"),
          tags$li(tags$strong("Narrative Clarity"), " - Does the dashboard tell a clear story?"),
          tags$li(tags$strong("Ethical Representation"), " - Is data shown honestly?")
        ),
        tags$p("After you rate each dashboard, you will see how your scores compare to expert evaluations, complete with explanations for each criterion."),
        tags$p("Enter your name in the sidebar and press ", tags$strong("Start Game"), " to begin."),
        tags$hr(),
        tags$p(class = "text-muted", tags$em("This game integrates concepts from all 8 weeks of the course: data representation, aesthetics, chart types, storytelling, interactivity, geography, and dashboard design."))
      )
    )
  ),

  # Active game panel
  conditionalPanel(
    condition = "output.game_active",
    uiOutput("dashboard_panel")
  ),

  # Completion panel
  conditionalPanel(
    condition = "output.game_complete",
    uiOutput("completion_panel")
  ),

  # Footer
  tags$footer(
    style = paste0(
      "text-align:center; padding:18px; margin-top:30px; ",
      "background-color:", NAVY, "; color:white; font-size:0.85rem; ",
      "border-radius:6px;"
    ),
    "Gonzaga University | Dr. Vivek H. Patil"
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {

  # --- Reactive values ---
  rv <- reactiveValues(
    game_started   = FALSE,
    game_finished  = FALSE,
    current_dash   = 1,
    scores         = list(),        # per-dashboard score detail
    total_score    = 0,
    submitted      = FALSE,         # whether current dashboard has been submitted
    show_feedback  = FALSE,
    reflection     = "",
    biggest_diff_id = NULL
  )

  # --- Outputs to control conditional panels ---
  output$game_active <- reactive({ rv$game_started && !rv$game_finished })
  outputOptions(output, "game_active", suspendWhenHidden = FALSE)

  output$game_complete <- reactive({ rv$game_finished })
  outputOptions(output, "game_complete", suspendWhenHidden = FALSE)

  # --- Start game ---
  observeEvent(input$start_btn, {
    req(nchar(trimws(input$player_name)) > 0)
    rv$game_started  <- TRUE
    rv$game_finished <- FALSE
    rv$current_dash  <- 1
    rv$scores        <- list()
    rv$total_score   <- 0
    rv$submitted     <- FALSE
    rv$show_feedback <- FALSE

    # Reset all sliders to 3
    for (cn in criteria_names) {
      updateSliderInput(session, paste0("slider_", cn), value = 3)
    }
  })

  # --- Progress bar ---
  output$progress_bar <- renderUI({
    done <- rv$current_dash - 1 + as.integer(rv$submitted)
    pct  <- round(done / 6 * 100)
    tags$div(
      class = "progress mt-2",
      style = "height: 22px;",
      tags$div(
        class = "progress-bar",
        role = "progressbar",
        style = paste0("width:", pct, "%; background-color:", NAVY, ";"),
        paste0(done, " / 6")
      )
    )
  })

  output$current_score <- renderText({ rv$total_score })
  output$max_so_far <- renderText({
    done <- length(rv$scores)
    paste0(done * 25)
  })

  # --- Dashboard display panel ---
  output$dashboard_panel <- renderUI({
    dash <- dashboard_info[[rv$current_dash]]

    tagList(
      # Dashboard title
      card(
        card_header(
          class = "text-white",
          style = paste0("background-color:", NAVY, ";"),
          tags$h4(class = "mb-0", paste0(
            "Dashboard ", rv$current_dash, " of 6: \"", dash$title, "\""
          ))
        ),
        card_body(
          style = "padding:8px;",
          wrap_spinner(plotOutput("dashboard_plot", height = "620px"))
        )
      ),

      tags$br(),

      # Rating sliders (only if not yet submitted)
      if (!rv$submitted) {
        card(
          card_header(
            class = "text-white",
            style = paste0("background-color:", GOLD, "; color:", NAVY, "; font-weight:700;"),
            "Rate This Dashboard"
          ),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              fill = FALSE,
              tags$div(
                sliderInput("slider_Appropriateness", "1. Chart Appropriateness",
                            min = 1, max = 5, value = 3, step = 1, ticks = TRUE),
                sliderInput("slider_DataInk", "2. Data-Ink Ratio",
                            min = 1, max = 5, value = 3, step = 1, ticks = TRUE),
                sliderInput("slider_Color", "3. Color Effectiveness",
                            min = 1, max = 5, value = 3, step = 1, ticks = TRUE)
              ),
              tags$div(
                sliderInput("slider_Narrative", "4. Narrative Clarity",
                            min = 1, max = 5, value = 3, step = 1, ticks = TRUE),
                sliderInput("slider_Ethics", "5. Ethical Representation",
                            min = 1, max = 5, value = 3, step = 1, ticks = TRUE),
                tags$br(),
                actionButton("submit_ratings", "Submit Ratings",
                             class = "btn-primary btn-lg w-100",
                             icon = icon("check"))
              )
            )
          )
        )
      },

      # Feedback (after submission)
      if (rv$show_feedback) {
        dash_score <- rv$scores[[rv$current_dash]]
        tagList(
          card(
            card_header(
              class = "text-white",
              style = paste0("background-color:", NAVY, ";"),
              paste0("Expert Comparison - Score: ", dash_score$total, " / 25")
            ),
            card_body(
              # Comparison bars
              lapply(seq_along(criteria_names), function(i) {
                cn <- criteria_names[i]
                student_val <- dash_score$student[cn]
                expert_val  <- dash_score$expert[cn]
                pts         <- dash_score$points[cn]
                diff        <- abs(student_val - expert_val)
                bar_color   <- if (diff == 0) "#27ae60" else if (diff <= 1) "#f39c12" else if (diff <= 2) "#e67e22" else "#e74c3c"
                label       <- criteria_labels[cn]

                tags$div(
                  style = "margin-bottom: 16px; padding: 10px; background: #f8f9fa; border-radius: 6px;",
                  tags$div(
                    style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:4px;",
                    tags$strong(label),
                    tags$span(
                      style = paste0("background:", bar_color, "; color:white; padding:2px 10px; border-radius:12px; font-size:0.85rem;"),
                      paste0("+", pts, " pts")
                    )
                  ),
                  # Student bar
                  tags$div(
                    style = "display:flex; align-items:center; margin-bottom:3px;",
                    tags$span(style = "width:65px; font-size:0.8rem; color:grey;", "You:"),
                    tags$div(
                      style = "flex:1; background:#e0e0e0; border-radius:4px; height:18px; position:relative;",
                      tags$div(
                        style = paste0(
                          "width:", student_val / 5 * 100, "%; background:", NAVY,
                          "; height:100%; border-radius:4px; display:flex; align-items:center; justify-content:center; color:white; font-size:0.75rem; font-weight:700;"
                        ),
                        student_val
                      )
                    )
                  ),
                  # Expert bar
                  tags$div(
                    style = "display:flex; align-items:center; margin-bottom:4px;",
                    tags$span(style = "width:65px; font-size:0.8rem; color:grey;", "Expert:"),
                    tags$div(
                      style = "flex:1; background:#e0e0e0; border-radius:4px; height:18px; position:relative;",
                      tags$div(
                        style = paste0(
                          "width:", expert_val / 5 * 100, "%; background:", GOLD,
                          "; height:100%; border-radius:4px; display:flex; align-items:center; justify-content:center; color:white; font-size:0.75rem; font-weight:700;"
                        ),
                        expert_val
                      )
                    )
                  ),
                  # Justification
                  tags$div(
                    style = "font-size:0.82rem; color:#555; margin-top:4px; padding-left:65px; font-style:italic;",
                    dash$expert_justifications[cn]
                  )
                )
              }),

              tags$hr(),
              tags$div(
                style = "text-align:center; margin-top: 10px;",
                tags$h5(paste0("Dashboard Quality: ", tags$strong(dash$quality))),
                tags$h5(paste0("Your Alignment Score: ", dash_score$total, " / 25"))
              ),

              tags$hr(),

              # Next button or finish
              if (rv$current_dash < 6) {
                actionButton("next_dash", "Next Dashboard",
                             class = "btn-primary btn-lg w-100",
                             icon = icon("arrow-right"))
              } else {
                tagList(
                  tags$h5(class = "text-center mb-3", "All dashboards rated! Now reflect on your experience."),
                  textAreaInput("reflection_text",
                                "For the dashboard where your scores differed most from the experts, explain what you think accounts for the difference.",
                                placeholder = "Write 1-2 sentences here...",
                                rows = 4, width = "100%"),
                  actionButton("finish_game", "Finish & See Report",
                               class = "btn-primary btn-lg w-100 mt-2",
                               icon = icon("flag-checkered"))
                )
              }
            )
          )
        )
      }
    )
  })

  # --- Render dashboard plot ---
  output$dashboard_plot <- renderPlot({
    dash <- dashboard_info[[rv$current_dash]]
    dash$make_fn()
  }, height = 620, res = 96)

  # --- Submit ratings ---
  observeEvent(input$submit_ratings, {
    dash <- dashboard_info[[rv$current_dash]]

    student_scores <- setNames(
      sapply(criteria_names, function(cn) input[[paste0("slider_", cn)]]),
      criteria_names
    )

    expert_scores <- dash$expert
    points <- setNames(
      sapply(criteria_names, function(cn) {
        score_criterion(student_scores[cn], expert_scores[cn])
      }),
      criteria_names
    )

    total <- sum(points)

    rv$scores[[rv$current_dash]] <- list(
      dashboard_id   = dash$id,
      dashboard_title = dash$title,
      quality        = dash$quality,
      student        = student_scores,
      expert         = expert_scores,
      points         = points,
      total          = total
    )

    rv$total_score <- rv$total_score + total
    rv$submitted   <- TRUE
    rv$show_feedback <- TRUE
  })

  # --- Next dashboard ---
  observeEvent(input$next_dash, {
    rv$current_dash <- rv$current_dash + 1
    rv$submitted    <- FALSE
    rv$show_feedback <- FALSE

    # Reset sliders
    for (cn in criteria_names) {
      updateSliderInput(session, paste0("slider_", cn), value = 3)
    }
  })

  # --- Finish game ---
  observeEvent(input$finish_game, {
    rv$reflection <- input$reflection_text

    # Identify dashboard with biggest total discrepancy
    diffs <- sapply(rv$scores, function(s) {
      sum(abs(s$student - s$expert))
    })
    rv$biggest_diff_id <- which.max(diffs)

    rv$game_finished <- TRUE
  })

  # --- Completion panel ---
  output$completion_panel <- renderUI({
    req(rv$game_finished)

    player_name <- trimws(input$player_name)
    date_str    <- format(Sys.Date(), "%B %d, %Y")
    total       <- rv$total_score
    max_total   <- 150
    pct         <- round(total / max_total * 100, 1)

    # Generate hash code
    hash_raw <- paste0(player_name, "-", total, "-", Sys.Date(), "-w8jury")
    if (requireNamespace("digest", quietly = TRUE)) {
      hash_code <- toupper(substring(digest::digest(hash_raw, algo = "crc32"), 1, 8))
    } else {
      hash_code <- toupper(format(as.hexmode(sum(utf8ToInt(hash_raw)) * 31L), width = 8))
    }

    # Performance tier
    tier <- if (pct >= 90) {
      list(label = "Expert Juror", color = "#27ae60")
    } else if (pct >= 75) {
      list(label = "Senior Juror", color = "#2980b9")
    } else if (pct >= 60) {
      list(label = "Juror", color = GOLD)
    } else if (pct >= 40) {
      list(label = "Apprentice Juror", color = "#e67e22")
    } else {
      list(label = "Novice Juror", color = RED)
    }

    # Build per-dashboard breakdown rows
    breakdown_rows <- lapply(seq_along(rv$scores), function(i) {
      s <- rv$scores[[i]]
      is_biggest_diff <- (i == rv$biggest_diff_id)

      criterion_cells <- lapply(criteria_names, function(cn) {
        diff <- abs(s$student[cn] - s$expert[cn])
        bg <- if (diff == 0) "#d5f5e3" else if (diff == 1) "#fef9e7" else if (diff == 2) "#fdebd0" else "#fadbd8"
        tags$td(
          style = paste0("padding:6px; text-align:center; background:", bg, "; font-size:0.82rem;"),
          paste0(s$student[cn], " / ", s$expert[cn])
        )
      })

      tags$tr(
        style = if (is_biggest_diff) "border-left: 4px solid #C41E3A;" else "",
        tags$td(
          style = "padding:6px 10px; font-weight:600; font-size:0.85rem;",
          paste0(i, ". ", s$dashboard_title),
          if (is_biggest_diff) tags$span(style = "color:#C41E3A; font-size:0.75rem;", " (largest gap)")
        ),
        criterion_cells,
        tags$td(
          style = "padding:6px; text-align:center; font-weight:700;",
          paste0(s$total, "/25")
        )
      )
    })

    # Full report text for copying
    report_lines <- c(
      "========================================",
      "DASHBOARD JURY - COMPLETION REPORT",
      "========================================",
      paste0("Player:  ", player_name),
      paste0("Date:    ", date_str),
      paste0("Score:   ", total, " / ", max_total, " (", pct, "%)"),
      paste0("Rank:    ", tier$label),
      "",
      "--- Per-Dashboard Breakdown (You / Expert) ---"
    )

    for (i in seq_along(rv$scores)) {
      s <- rv$scores[[i]]
      report_lines <- c(report_lines, paste0("\nDashboard ", i, ": ", s$dashboard_title, " [", s$quality, "]"))
      for (cn in criteria_names) {
        report_lines <- c(report_lines, paste0(
          "  ", criteria_labels[cn], ": ", s$student[cn], " / ", s$expert[cn],
          "  (+", s$points[cn], " pts)"
        ))
      }
      report_lines <- c(report_lines, paste0("  Dashboard Total: ", s$total, " / 25"))
    }

    biggest_s <- rv$scores[[rv$biggest_diff_id]]
    report_lines <- c(
      report_lines,
      "",
      paste0("Largest Gap Dashboard: ", biggest_s$dashboard_title),
      paste0("Reflection: ", rv$reflection),
      "",
      paste0("Verification Hash: ", hash_code),
      "========================================"
    )

    report_text <- paste(report_lines, collapse = "\n")

    # --- UI ---
    tagList(
      card(
        card_header(
          class = "text-white",
          style = paste0("background-color:", NAVY, ";"),
          tags$h3(class = "mb-0", "Dashboard Jury - Completion Report")
        ),
        card_body(
          # Header info
          layout_columns(
            col_widths = c(4, 4, 4),
            tags$div(
              tags$strong("Player"), tags$br(),
              tags$span(style = "font-size:1.1rem;", player_name)
            ),
            tags$div(
              style = "text-align:center;",
              tags$strong("Date"), tags$br(),
              tags$span(style = "font-size:1.1rem;", date_str)
            ),
            tags$div(
              style = "text-align:right;",
              tags$strong("Rank"), tags$br(),
              tags$span(
                style = paste0("font-size:1.1rem; color:", tier$color, "; font-weight:700;"),
                tier$label
              )
            )
          ),

          tags$hr(),

          # Score display
          tags$div(
            style = "text-align:center; margin: 15px 0;",
            tags$h2(
              style = paste0("color:", NAVY, "; margin-bottom:5px;"),
              paste0(total, " / ", max_total)
            ),
            tags$div(
              class = "progress",
              style = "height: 28px; max-width: 500px; margin: 0 auto;",
              tags$div(
                class = "progress-bar",
                role = "progressbar",
                style = paste0("width:", pct, "%; background-color:", tier$color, "; font-weight:700; font-size:0.9rem;"),
                paste0(pct, "%")
              )
            )
          ),

          tags$hr(),

          # Breakdown table
          tags$h5("Per-Dashboard Breakdown", style = "margin-bottom:10px;"),
          tags$div(
            style = "overflow-x:auto;",
            tags$table(
              class = "table table-sm table-bordered",
              style = "font-size:0.85rem;",
              tags$thead(
                style = paste0("background-color:", NAVY, "; color:white;"),
                tags$tr(
                  tags$th("Dashboard", style = "padding:8px;"),
                  lapply(criteria_labels, function(l) {
                    tags$th(style = "padding:8px; text-align:center; font-size:0.78rem;", l)
                  }),
                  tags$th(style = "padding:8px; text-align:center;", "Total")
                )
              ),
              tags$tbody(breakdown_rows)
            )
          ),
          tags$p(
            style = "font-size:0.8rem; color:grey;",
            "Cell format: Your Score / Expert Score. Green = exact match, Yellow = within 1, Orange = within 2, Red = off by 3+"
          ),

          tags$hr(),

          # Reflection
          tags$div(
            style = "background:#f8f9fa; padding:15px; border-radius:8px; border-left:4px solid #C41E3A; margin-bottom:15px;",
            tags$h5("Your Reflection"),
            tags$p(
              style = "margin-bottom:4px;",
              tags$strong(paste0(
                "Dashboard with largest gap: \"",
                rv$scores[[rv$biggest_diff_id]]$dashboard_title, "\""
              ))
            ),
            tags$p(style = "font-style:italic;", rv$reflection)
          ),

          # Ignatian reflection
          tags$div(
            style = paste0(
              "background:", NAVY, "; color:white; padding:20px; border-radius:8px; ",
              "margin:15px 0; font-style:italic; line-height:1.7;"
            ),
            tags$p(
              "Throughout this course, you've learned to see data through the lens of design, ",
              "ethics, and purpose. As you go forward, remember: every visualization is an act of ",
              "communication with real consequences for real people. Use your skills in service of ",
              "truth, clarity, and the common good."
            )
          ),

          tags$hr(),

          # Hash and copy
          tags$div(
            style = "text-align:center;",
            tags$p(
              style = "font-family:monospace; font-size:1.1rem; letter-spacing:2px;",
              paste0("Verification Hash: ", hash_code)
            ),
            tags$textarea(
              id = "report_text_area",
              style = "position:absolute; left:-9999px;",
              report_text
            ),
            tags$button(
              id = "copy_report_btn",
              class = "btn btn-outline-primary btn-lg",
              onclick = paste0(
                "var t=document.getElementById('report_text_area');",
                "t.style.position='static';t.select();document.execCommand('copy');",
                "t.style.position='absolute';",
                "var b=document.getElementById('copy_report_btn');",
                "b.innerText='Copied!';setTimeout(function(){b.innerText='Copy Report to Clipboard';},2000);"
              ),
              icon("clipboard"),
              " Copy Report to Clipboard"
            )
          )
        )
      )
    )
  })
}

# ============================================================================
# RUN
# ============================================================================
shinyApp(ui, server)
