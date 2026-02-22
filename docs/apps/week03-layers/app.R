library(shiny)
library(bslib)
library(ggplot2)

# ---------------------------------------------------------------------------
# Optional packages — graceful fallback if not installed
# ---------------------------------------------------------------------------
has_shinyjs <- requireNamespace("shinyjs", quietly = TRUE)
if (has_shinyjs) library(shinyjs)

has_spinner <- requireNamespace("shinycssloaders", quietly = TRUE)
if (has_spinner) library(shinycssloaders)

has_penguins <- requireNamespace("palmerpenguins", quietly = TRUE)

# ---------------------------------------------------------------------------
# Gonzaga brand colours
# ---------------------------------------------------------------------------
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ---------------------------------------------------------------------------
# Dataset definitions
# ---------------------------------------------------------------------------
dataset_choices <- c("mpg", "diamonds")
if (has_penguins) dataset_choices <- c(dataset_choices, "penguins")

dataset_meta <- list(
  mpg = list(
    x = "displ", y = "hwy", color = "class",
    x_lab = "Engine Displacement (L)", y_lab = "Highway MPG",
    color_lab = "Vehicle Class",
    title = "Engine Size vs. Highway MPG",
    subtitle = "Larger engines tend to have lower fuel economy",
    facet_var = "drv",
    facet_labeller = labeller(drv = c("4" = "4WD", "f" = "FWD", "r" = "RWD"))
  ),
  diamonds = list(
    x = "carat", y = "price", color = "cut",
    x_lab = "Carat", y_lab = "Price (USD)",
    color_lab = "Cut Quality",
    title = "Diamond Carat vs. Price",
    subtitle = "Higher carat diamonds command higher prices",
    facet_var = "clarity",
    facet_labeller = NULL
  ),
  penguins = list(
    x = "bill_length_mm", y = "body_mass_g", color = "species",
    x_lab = "Bill Length (mm)", y_lab = "Body Mass (g)",
    color_lab = "Species",
    title = "Penguin Bill Length vs. Body Mass",
    subtitle = "Morphological differences across penguin species",
    facet_var = "island",
    facet_labeller = NULL
  )
)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),

  # Include shinyjs if available
  if (has_shinyjs) shinyjs::useShinyjs() else NULL,

  # Custom CSS for badge, copy-button, footer, accordion tweaks

  tags$head(tags$style(HTML(sprintf("
    .layer-badge {
      display: inline-block;
      background: %s;
      color: #fff;
      padding: 2px 10px;
      border-radius: 12px;
      font-size: 0.82rem;
      font-weight: 600;
      letter-spacing: 0.3px;
    }
    .btn-copy {
      background: %s;
      color: #fff;
      border: none;
      padding: 4px 14px;
      border-radius: 4px;
      font-size: 0.78rem;
      cursor: pointer;
      margin-top: 4px;
    }
    .btn-copy:hover { background: %s; color: #fff; }
    .btn-reset { margin-top: 6px; width: 100%%; }
    .site-footer {
      text-align: center;
      font-size: 0.78rem;
      color: #64748b;
      padding: 10px 0 4px 0;
      border-top: 1px solid #e2e8f0;
      margin-top: 12px;
    }
    .site-footer strong { color: %s; }
  ", NAVY, NAVY, RED, NAVY)))),

  sidebar = sidebar(
    width = 300, bg = "#f1f5f9",

    # --- Instruction accordion ---
    accordion(
      id = "help_accordion",
      open = FALSE,
      accordion_panel(
        title = "How to Use This Explorer",
        icon  = bsicons::bs_icon("question-circle"),
        p("1. Choose a dataset from the dropdown below."),
        p("2. Toggle each checkbox to add or remove a ggplot2 layer."),
        p("3. Watch the plot and the generated code update in real-time."),
        p("4. Use ", tags$b("Copy Code"), " to paste the snippet into your own R script."),
        p("5. Hit ", tags$b("Reset All"), " to start over."),
        style = "font-size:0.82rem; line-height:1.45;"
      )
    ),

    tags$hr(style = "margin:6px 0;"),

    # --- Dataset selector ---
    selectInput("dataset", "Choose dataset:",
                choices  = dataset_choices,
                selected = "mpg"),

    tags$hr(style = "margin:6px 0;"),

    # --- Layer counter badge ---
    p(
      strong("Build your plot layer by layer:"),
      uiOutput("layer_badge", inline = TRUE),
      style = "margin-bottom:8px; color:#002967;"
    ),

    # --- Layer checkboxes ---
    checkboxInput("l_color",  "Map color aesthetic",          FALSE),
    checkboxInput("l_alpha",  "Set transparency (alpha = 0.7)", FALSE),
    checkboxInput("l_smooth", "Add trend line (geom_smooth)",  FALSE),
    checkboxInput("l_facet",  "Facet by grouping variable",    FALSE),
    checkboxInput("l_labels", "Add informative labels",        FALSE),
    checkboxInput("l_theme",  "Apply theme_minimal()",         FALSE),

    # --- Reset button ---
    actionButton("reset_all", "Reset All",
                 icon  = icon("rotate-left"),
                 class = "btn-sm btn-outline-danger btn-reset"),

    tags$hr(style = "margin:8px 0;"),

    # --- Generated code ---
    p(strong("Generated code:"),
      style = "margin-bottom:4px; font-size:0.85rem;"),
    verbatimTextOutput("code_out"),

    # --- Copy Code button ---
    if (has_shinyjs) {
      actionButton("copy_code", "Copy Code",
                   icon  = icon("clipboard"),
                   class = "btn-copy btn-sm")
    } else {
      NULL
    }
  ),

  # --- Main panel ---
  card(
    full_screen = TRUE,
    card_header("Grammar of Graphics: Layer Builder",
                class = "bg-primary text-white"),
    card_body(
      if (has_spinner) {
        shinycssloaders::withSpinner(
          plotOutput("layer_plot", height = "480px"),
          color = NAVY, type = 6
        )
      } else {
        plotOutput("layer_plot", height = "480px")
      }
    ),
    card_footer(
      tags$div(
        "Each checkbox adds one layer or property. Watch how the code and the plot change together.",
        style = "font-size:0.8rem; color:#64748b; margin-bottom:6px;"
      ),
      tags$div(
        class = "site-footer",
        HTML("<strong>Gonzaga University</strong> | Dr.&nbsp;Vivek&nbsp;H.&nbsp;Patil")
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # --- Active data ---------------------------------------------------------
  plot_data <- reactive({
    req(input$dataset)
    switch(input$dataset,
      mpg      = ggplot2::mpg,
      diamonds = {
        set.seed(42)
        dplyr::slice_sample(ggplot2::diamonds, n = 500)
      },
      penguins = {
        validate(need(has_penguins,
                      "palmerpenguins package is not installed."))
        df <- palmerpenguins::penguins
        df <- df[complete.cases(df), ]
        validate(need(nrow(df) > 0, "No complete cases in penguins data."))
        df
      }
    )
  })

  meta <- reactive({
    req(input$dataset)
    dataset_meta[[input$dataset]]
  })

  # --- Layer counter badge -------------------------------------------------
  layer_count <- reactive({
    sum(c(
      isTRUE(input$l_color),
      isTRUE(input$l_alpha),
      isTRUE(input$l_smooth),
      isTRUE(input$l_facet),
      isTRUE(input$l_labels),
      isTRUE(input$l_theme)
    ))
  })

  output$layer_badge <- renderUI({
    tags$span(class = "layer-badge",
              paste0("Layers: ", layer_count(), "/6"))
  })

  # --- Reset button --------------------------------------------------------
  observeEvent(input$reset_all, {
    updateCheckboxInput(session, "l_color",  value = FALSE)
    updateCheckboxInput(session, "l_alpha",  value = FALSE)
    updateCheckboxInput(session, "l_smooth", value = FALSE)
    updateCheckboxInput(session, "l_facet",  value = FALSE)
    updateCheckboxInput(session, "l_labels", value = FALSE)
    updateCheckboxInput(session, "l_theme",  value = FALSE)
    updateSelectInput(session, "dataset",    selected = "mpg")
  })

  # --- Code builder --------------------------------------------------------
  build_code <- reactive({
    req(input$dataset)
    m <- meta()
    ds_name <- input$dataset

    # Data expression for the code display
    data_expr <- switch(ds_name,
      mpg      = "mpg",
      diamonds = "diamonds %>% slice_sample(n = 500)",
      penguins = "penguins"
    )

    lines <- character()
    aes_args <- paste0("x = ", m$x, ", y = ", m$y)
    if (isTRUE(input$l_color)) aes_args <- paste0(aes_args, ", color = ", m$color)

    lines <- c(lines, paste0("ggplot(", data_expr, ", aes(", aes_args, "))"))

    geom_args <- ""
    if (isTRUE(input$l_alpha)) geom_args <- "alpha = 0.7"
    lines <- c(lines, paste0("  geom_point(", geom_args, ")"))

    if (isTRUE(input$l_smooth))
      lines <- c(lines, '  geom_smooth(method = "lm", se = FALSE)')

    if (isTRUE(input$l_facet))
      lines <- c(lines, paste0("  facet_wrap(~ ", m$facet_var, ")"))

    if (isTRUE(input$l_labels))
      lines <- c(lines, paste0(
        '  labs(title = "', m$title, '",\n',
        '       subtitle = "', m$subtitle, '",\n',
        '       x = "', m$x_lab, '",\n',
        '       y = "', m$y_lab, '",\n',
        '       color = "', m$color_lab, '")'
      ))

    if (isTRUE(input$l_theme))
      lines <- c(lines, "  theme_minimal(base_size = 14)")

    paste(lines, collapse = " +\n")
  })

  output$code_out <- renderText({ build_code() })

  # --- Copy Code -----------------------------------------------------------
  if (has_shinyjs) {
    observeEvent(input$copy_code, {
      code_text <- build_code()
      # Escape for JS string literal
      safe <- gsub("\\\\", "\\\\\\\\", code_text)
      safe <- gsub("'", "\\\\'", safe)
      safe <- gsub("\n", "\\\\n", safe)
      shinyjs::runjs(sprintf(
        "navigator.clipboard.writeText('%s').then(function(){
           Shiny.notifications.show({html:'<strong>Copied!</strong> Code is on your clipboard.', type:'message', duration:2500});
         }).catch(function(){
           var ta=document.createElement('textarea');ta.value='%s';document.body.appendChild(ta);ta.select();document.execCommand('copy');document.body.removeChild(ta);
           Shiny.notifications.show({html:'<strong>Copied!</strong>', type:'message', duration:2000});
         });", safe, safe
      ))
    })
  }

  # --- Plot ----------------------------------------------------------------
  output$layer_plot <- renderPlot({
    req(input$dataset)
    df <- plot_data()
    m  <- meta()

    validate(need(nrow(df) > 0, "No data available for the selected dataset."))

    x_sym     <- rlang::sym(m$x)
    y_sym     <- rlang::sym(m$y)
    color_sym <- rlang::sym(m$color)
    facet_sym <- rlang::sym(m$facet_var)

    # Base plot
    if (isTRUE(input$l_color)) {
      p <- ggplot(df, aes(x = !!x_sym, y = !!y_sym, color = !!color_sym))
    } else {
      p <- ggplot(df, aes(x = !!x_sym, y = !!y_sym))
    }

    # Geom point
    geom_args <- list()
    if (!isTRUE(input$l_color)) geom_args$color <- NAVY
    if (isTRUE(input$l_alpha))  geom_args$alpha <- 0.7
    p <- p + do.call(geom_point, geom_args)

    # Smooth
    if (isTRUE(input$l_smooth)) {
      smooth_color <- if (isTRUE(input$l_color)) "grey40" else RED
      p <- p + geom_smooth(method = "lm", se = FALSE, color = smooth_color)
    }

    # Facet
    if (isTRUE(input$l_facet)) {
      if (!is.null(m$facet_labeller)) {
        p <- p + facet_wrap(vars(!!facet_sym), labeller = m$facet_labeller)
      } else {
        p <- p + facet_wrap(vars(!!facet_sym))
      }
    }

    # Labels
    if (isTRUE(input$l_labels)) {
      p <- p + labs(
        title    = m$title,
        subtitle = m$subtitle,
        x        = m$x_lab,
        y        = m$y_lab,
        color    = m$color_lab
      )
    }

    # Theme
    if (isTRUE(input$l_theme)) {
      p <- p + theme_minimal(base_size = 14) +
        theme(plot.title    = element_text(color = NAVY, face = "bold"),
              plot.subtitle = element_text(color = "#475569"))
    }

    # Colour scale
    if (isTRUE(input$l_color)) {
      p <- p + scale_color_brewer(palette = "Dark2")
    }

    p
  }, res = 110)
}

# ---------------------------------------------------------------------------
shinyApp(ui, server)
