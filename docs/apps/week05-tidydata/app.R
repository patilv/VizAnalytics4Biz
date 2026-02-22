library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(DT)

# ---------------------------------------------------------------------------
# Gonzaga palette
# ---------------------------------------------------------------------------
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ---------------------------------------------------------------------------
# Spinner helper -- graceful fallback when shinycssloaders is not installed
# ---------------------------------------------------------------------------
has_spinners <- requireNamespace("shinycssloaders", quietly = TRUE)
if (has_spinners) library(shinycssloaders)

with_spinner <- function(ui_element) {
  if (has_spinners) {
    shinycssloaders::withSpinner(ui_element, color = NAVY, type = 6)
  } else {
    ui_element
  }
}

# ---------------------------------------------------------------------------
# Example messy (wide) datasets
# ---------------------------------------------------------------------------
datasets <- list(
  "Quarterly Sales" = data.frame(
    product = c("Laptop", "Tablet", "Phone", "Monitor", "Keyboard"),
    Q1 = c(12500, 8400, 22100, 5600, 3200),
    Q2 = c(13100, 9200, 24500, 6100, 3800),
    Q3 = c(11800, 7800, 21000, 5900, 3500),
    Q4 = c(15200, 10500, 28700, 7200, 4100),
    stringsAsFactors = FALSE
  ),
  "Student Scores" = data.frame(
    student = c("Alice", "Bob", "Carlos", "Diana", "Elena", "Frank"),
    math    = c(92, 78, 85, 95, 88, 72),
    science = c(88, 82, 79, 91, 94, 68),
    english = c(76, 91, 88, 82, 85, 80),
    stringsAsFactors = FALSE
  ),
  "City Temperatures" = data.frame(
    city = c("Spokane", "Seattle", "Portland", "Boise", "Denver"),
    Jan = c(30, 42, 40, 33, 35),
    Feb = c(35, 44, 44, 38, 37),
    Mar = c(42, 48, 49, 45, 44),
    Apr = c(49, 52, 53, 52, 50),
    May = c(58, 59, 60, 62, 60),
    Jun = c(66, 64, 65, 72, 70),
    stringsAsFactors = FALSE
  )
)

# Metadata for default names_to / values_to labels per dataset
dataset_defaults <- list(
  "Quarterly Sales"  = list(names_to = "quarter",  values_to = "revenue",
                            id_col = "product"),
  "Student Scores"   = list(names_to = "subject",  values_to = "score",
                            id_col = "student"),
  "City Temperatures" = list(names_to = "month",    values_to = "temperature",
                             id_col = "city")
)

# Color palette for visual mapping -- one color per pivotable column
color_pool <- c(
  "#2563eb", "#dc2626", "#16a34a", "#9333ea", "#ea580c", "#0891b2",
  "#4f46e5", "#be123c", "#15803d", "#7c3aed", "#c2410c", "#0e7490"
)

# ===========================================================================
# UI
# ===========================================================================
ui <- page_sidebar(
  title = NULL,
  fillable = FALSE,
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),

  # -- Custom CSS -----------------------------------------------------------
  tags$head(tags$style(HTML(sprintf("
    .gonzaga-footer {
      text-align: center;
      padding: 10px 0;
      font-size: 0.78rem;
      color: #64748b;
      border-top: 1px solid #e2e8f0;
      margin-top: 16px;
    }
    .tidy-rules {
      background: #f8fafc;
      border-left: 4px solid %s;
      padding: 12px 16px;
      border-radius: 4px;
      font-size: 0.88rem;
      line-height: 1.7;
      color: #334155;
    }
    .code-display {
      background: #1e293b;
      color: #e2e8f0;
      padding: 14px 18px;
      border-radius: 6px;
      font-family: 'Fira Code', 'Consolas', monospace;
      font-size: 0.85rem;
      line-height: 1.6;
      white-space: pre-wrap;
      overflow-x: auto;
    }
    .color-swatch {
      display: inline-block;
      width: 12px;
      height: 12px;
      border-radius: 2px;
      margin-right: 4px;
      vertical-align: middle;
    }
    .mapping-legend-item {
      display: inline-block;
      margin-right: 14px;
      font-size: 0.82rem;
      color: #334155;
      white-space: nowrap;
    }
    .wide-selected td {
      font-weight: 700 !important;
    }
  ", GOLD)))),

  sidebar = sidebar(
    width = 290,
    bg    = "#f1f5f9",

    # -- Instruction accordion -----------------------------------------------
    accordion(
      id   = "help_accordion",
      open = FALSE,
      accordion_panel(
        title = "How to Use This Explorer",
        icon  = bsicons::bs_icon("info-circle"),
        tags$ul(
          style = "font-size: 0.82rem; padding-left: 1.1rem; margin-bottom: 0;",
          tags$li("Choose an example dataset from the dropdown."),
          tags$li("The left table shows the ", tags$strong("wide (messy)"),
                  " format; the right shows the ", tags$strong("long (tidy)"), " result."),
          tags$li("Use the ", tags$strong("names_to"), " and ",
                  tags$strong("values_to"), " text inputs to rename the new columns."),
          tags$li("Toggle ", tags$strong("column checkboxes"), " to control which columns get pivoted."),
          tags$li("The generated R code updates live -- copy it into your own script!"),
          tags$li("Color bands connect wide columns to their tidy rows.")
        )
      )
    ),

    hr(style = "margin-top: 8px; margin-bottom: 8px;"),

    # -- Dataset selector ----------------------------------------------------
    selectInput(
      "dataset", "Example dataset:",
      choices  = names(datasets),
      selected = "Quarterly Sales"
    ),

    tags$hr(),

    p(strong("pivot_longer() controls:"),
      style = "color:#002967; margin-bottom:6px;"),

    # -- names_to input ------------------------------------------------------
    textInput(
      "names_to", "names_to (new key column):",
      value = "quarter"
    ),

    # -- values_to input -----------------------------------------------------
    textInput(
      "values_to", "values_to (new value column):",
      value = "revenue"
    ),

    tags$hr(),

    # -- Columns to pivot checkboxes -----------------------------------------
    p(strong("Columns to pivot (cols):"),
      style = "color:#002967; margin-bottom:4px; font-size:0.88rem;"),
    p("Select which columns become rows:",
      style = "font-size:0.78rem; color:#64748b; margin-bottom:6px;"),

    uiOutput("col_checkboxes"),

    tags$hr(),

    # -- "Why Tidy?" info card -----------------------------------------------
    card(
      card_header(
        tagList(bsicons::bs_icon("lightbulb"), " Why Tidy Data?"),
        class = "bg-primary text-white"
      ),
      card_body(
        div(
          class = "tidy-rules",
          tags$strong("Hadley Wickham's three rules:"),
          tags$ol(
            style = "margin-top: 6px; margin-bottom: 0; padding-left: 1.2rem;",
            tags$li("Every ", tags$strong("variable"), " is a column."),
            tags$li("Every ", tags$strong("observation"), " is a row."),
            tags$li("Every ", tags$strong("value"), " is a cell.")
          ),
          tags$br(),
          tags$span(
            style = "font-size:0.82rem; color:#475569;",
            "Tidy data works seamlessly with ggplot2, dplyr, and the entire tidyverse. ",
            "pivot_longer() reshapes wide data into this tidy form."
          )
        )
      )
    )
  ),

  # =========================================================================
  # MAIN CONTENT
  # =========================================================================

  # -- Generated code card ---------------------------------------------------
  card(
    card_header(
      tagList(bsicons::bs_icon("code-slash"), " Generated R Code"),
      class = "bg-primary text-white"
    ),
    card_body(
      with_spinner(uiOutput("code_display")),
      style = "padding: 10px 14px;"
    )
  ),

  # -- Color legend ----------------------------------------------------------
  card(
    card_body(
      uiOutput("color_legend"),
      style = "padding: 8px 14px;"
    )
  ),

  # -- Side-by-side tables ---------------------------------------------------
  layout_columns(
    col_widths = c(5, 7),

    # Left: Wide (messy) table
    card(
      full_screen = TRUE,
      card_header(
        tagList(bsicons::bs_icon("table"), " Wide (Messy) Data"),
        class = "bg-primary text-white"
      ),
      card_body(
        with_spinner(DTOutput("wide_table")),
        style = "padding: 8px;"
      ),
      card_footer(
        uiOutput("wide_dims"),
        style = "font-size:0.8rem; color:#64748b;"
      )
    ),

    # Right: Long (tidy) table
    card(
      full_screen = TRUE,
      card_header(
        tagList(bsicons::bs_icon("arrow-down-up"), " Long (Tidy) Data"),
        class = "bg-primary text-white"
      ),
      card_body(
        with_spinner(DTOutput("long_table")),
        style = "padding: 8px;"
      ),
      card_footer(
        uiOutput("long_dims"),
        style = "font-size:0.8rem; color:#64748b;"
      )
    )
  ),

  # -- Footer ----------------------------------------------------------------
  tags$footer(
    class = "gonzaga-footer",
    "Gonzaga University | Dr. Vivek H. Patil"
  )
)

# ===========================================================================
# SERVER
# ===========================================================================
server <- function(input, output, session) {

  # -------------------------------------------------------------------------
  # Reactive: current wide dataset
  # -------------------------------------------------------------------------
  wide_data <- reactive({
    req(input$dataset)
    datasets[[input$dataset]]
  })

  # -------------------------------------------------------------------------
  # Reactive: pivotable column names (everything except the ID column)
  # -------------------------------------------------------------------------
  pivot_cols_available <- reactive({
    d   <- wide_data()
    req(d)
    meta <- dataset_defaults[[input$dataset]]
    setdiff(names(d), meta$id_col)
  })

  # -------------------------------------------------------------------------
  # Update names_to / values_to defaults when dataset changes
  # -------------------------------------------------------------------------
  observeEvent(input$dataset, {
    meta <- dataset_defaults[[input$dataset]]
    updateTextInput(session, "names_to",  value = meta$names_to)
    updateTextInput(session, "values_to", value = meta$values_to)
  })

  # -------------------------------------------------------------------------
  # Render dynamic column checkboxes
  # -------------------------------------------------------------------------
  output$col_checkboxes <- renderUI({
    cols <- pivot_cols_available()
    req(length(cols) > 0)

    # Build color-coded choice labels using HTML
    color_map <- setNames(color_pool[seq_along(cols)], cols)

    choice_labels <- lapply(cols, function(col_name) {
      HTML(paste0(
        '<span style="display:inline-block; width:12px; height:12px; ',
        'border-radius:2px; background:', color_map[col_name],
        '; margin-right:5px; vertical-align:middle;"></span>',
        '<span style="vertical-align:middle;">', col_name, '</span>'
      ))
    })

    checkboxGroupInput(
      "pivot_cols", NULL,
      choiceNames  = choice_labels,
      choiceValues = cols,
      selected     = cols
    )
  })

  # -------------------------------------------------------------------------
  # Reactive: currently selected pivot columns
  # -------------------------------------------------------------------------
  selected_pivot_cols <- reactive({
    available <- pivot_cols_available()
    sel <- input$pivot_cols
    if (is.null(sel) || length(sel) == 0) return(character(0))
    intersect(sel, available)
  })

  # -------------------------------------------------------------------------
  # Reactive: color mapping for selected columns
  # -------------------------------------------------------------------------
  col_color_map <- reactive({
    available <- pivot_cols_available()
    setNames(color_pool[seq_along(available)], available)
  })

  # -------------------------------------------------------------------------
  # Reactive: tidy (long) data
  # -------------------------------------------------------------------------
  long_data <- reactive({
    d    <- wide_data()
    cols <- selected_pivot_cols()
    n_to <- input$names_to
    v_to <- input$values_to

    validate(
      need(length(cols) > 0, "Select at least one column to pivot."),
      need(nchar(trimws(n_to)) > 0, "Enter a name for names_to."),
      need(nchar(trimws(v_to)) > 0, "Enter a name for values_to.")
    )

    n_to <- trimws(n_to)
    v_to <- trimws(v_to)

    # Guard against names_to == values_to
    validate(
      need(n_to != v_to, "names_to and values_to must be different.")
    )

    tryCatch({
      d %>%
        pivot_longer(
          cols      = all_of(cols),
          names_to  = n_to,
          values_to = v_to
        )
    }, error = function(e) {
      validate(paste("pivot_longer error:", e$message))
    })
  })

  # -------------------------------------------------------------------------
  # Reactive: long data with source-column color assignment
  # -------------------------------------------------------------------------
  long_data_colored <- reactive({
    ld   <- long_data()
    req(ld)
    n_to <- trimws(input$names_to)
    cmap <- col_color_map()
    ld$`.src_color` <- cmap[ld[[n_to]]]
    ld
  })

  # -------------------------------------------------------------------------
  # Render: color legend
  # -------------------------------------------------------------------------
  output$color_legend <- renderUI({
    cols <- selected_pivot_cols()
    req(length(cols) > 0)
    cmap <- col_color_map()

    items <- lapply(cols, function(col_name) {
      tags$span(
        class = "mapping-legend-item",
        tags$span(
          class = "color-swatch",
          style = paste0("background:", cmap[col_name], ";")
        ),
        col_name
      )
    })

    tagList(
      tags$span(
        style = "font-size:0.82rem; font-weight:600; color:#002967; margin-right:10px;",
        bsicons::bs_icon("palette2"),
        " Column color map:"
      ),
      items
    )
  })

  # -------------------------------------------------------------------------
  # Render: generated R code
  # -------------------------------------------------------------------------
  output$code_display <- renderUI({
    cols <- selected_pivot_cols()
    n_to <- trimws(input$names_to)
    v_to <- trimws(input$values_to)
    ds   <- input$dataset

    if (length(cols) == 0 || nchar(n_to) == 0 || nchar(v_to) == 0) {
      return(div(
        class = "code-display",
        style = "color:#94a3b8;",
        "# Adjust controls to generate code..."
      ))
    }

    # Build the cols argument string
    cols_str <- paste0(cols, collapse = ", ")

    # Build variable name from dataset name
    var_name <- switch(ds,
      "Quarterly Sales"   = "sales_data",
      "Student Scores"    = "scores_data",
      "City Temperatures" = "temps_data"
    )

    code_text <- paste0(
      var_name, " %>%\n",
      "  pivot_longer(\n",
      "    cols      = c(", cols_str, "),\n",
      "    names_to  = \"", n_to, "\",\n",
      "    values_to = \"", v_to, "\"\n",
      "  )"
    )

    div(class = "code-display", code_text)
  })

  # -------------------------------------------------------------------------
  # Render: wide (messy) DT table with colored column headers
  # -------------------------------------------------------------------------
  output$wide_table <- renderDT({
    d <- wide_data()
    req(d)

    cmap     <- col_color_map()
    sel_cols <- selected_pivot_cols()
    meta     <- dataset_defaults[[input$dataset]]

    datatable(
      d,
      options = list(
        pageLength = 10,
        scrollX    = TRUE,
        dom        = "t",
        ordering   = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      rownames   = FALSE,
      selection  = "single",
      class      = "compact stripe hover",
      callback   = JS("return table;")
    ) %>%
      # Style the ID column header
      formatStyle(
        columns         = meta$id_col,
        fontWeight      = "bold",
        backgroundColor = "#f1f5f9",
        color           = NAVY
      ) %>%
      # Style pivotable columns with their assigned colors
      {
        tbl <- .
        for (col_name in names(d)) {
          if (col_name %in% sel_cols && col_name %in% names(cmap)) {
            tbl <- tbl %>%
              formatStyle(
                columns         = col_name,
                backgroundColor = paste0(cmap[col_name], "20"),
                borderBottom    = paste0("3px solid ", cmap[col_name])
              )
          }
        }
        tbl
      }
  })

  # -------------------------------------------------------------------------
  # Render: long (tidy) DT table with colored rows
  # -------------------------------------------------------------------------
  output$long_table <- renderDT({
    ld   <- long_data_colored()
    req(ld)

    n_to <- trimws(input$names_to)
    cmap <- col_color_map()

    # Build display data (drop internal color column)
    display_df <- ld %>% select(-`.src_color`)

    # Create row-level background colors (light tint)
    row_colors <- ld$`.src_color`

    datatable(
      display_df,
      options = list(
        pageLength = 20,
        scrollX    = TRUE,
        dom        = "tip",
        ordering   = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      rownames  = FALSE,
      selection = "none",
      class     = "compact stripe hover"
    ) %>%
      formatStyle(
        columns         = n_to,
        fontWeight      = "bold"
      ) %>%
      # Color-code each row based on its source column
      formatStyle(
        columns         = names(display_df),
        backgroundColor = styleEqual(
          levels = names(cmap),
          values = paste0(cmap, "20")
        ),
        target          = "row",
        valueColumns    = n_to
      )
  })

  # -------------------------------------------------------------------------
  # Render: dimension labels
  # -------------------------------------------------------------------------
  output$wide_dims <- renderUI({
    d <- wide_data()
    req(d)
    tagList(
      bsicons::bs_icon("table"),
      strong(nrow(d)), " rows x ",
      strong(ncol(d)), " columns"
    )
  })

  output$long_dims <- renderUI({
    ld <- long_data()
    req(ld)
    tagList(
      bsicons::bs_icon("arrow-down-up"),
      strong(nrow(ld)), " rows x ",
      strong(ncol(ld)), " columns ",
      tags$span(
        style = "color:#002967; font-weight:600;",
        paste0("(", nrow(ld) - nrow(wide_data()), " new rows from pivoting)")
      )
    )
  })
}

# ===========================================================================
# Run
# ===========================================================================
shinyApp(ui, server)
