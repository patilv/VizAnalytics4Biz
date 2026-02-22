library(shiny)
library(bslib)
library(ggplot2)

has_spinners <- requireNamespace("shinycssloaders", quietly = TRUE)
if (has_spinners) library(shinycssloaders)

wrap_spinner <- function(ui_element, color = "#002967") {
  if (has_spinners) {
    shinycssloaders::withSpinner(ui_element, color = color, type = 6)
  } else {
    ui_element
  }
}

# -- Gonzaga brand palette --
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# -- Column choices from ggplot2::mpg --
numeric_cols      <- c("displ", "cty", "hwy", "year")
categorical_cols  <- c("class", "drv", "fl", "manufacturer")
none_option       <- c("None" = "none")

ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),

  sidebar = sidebar(
    width = 290, bg = "#f1f5f9",

    # -- Instruction accordion --
    accordion(
      id = "help_accordion",
      open = FALSE,
      accordion_panel(
        title = "How to Use This Explorer",
        icon  = bsicons::bs_icon("info-circle"),
        tags$ul(
          style = "font-size: 0.82rem; padding-left: 1.1rem; margin-bottom: 0;",
          tags$li("Pick variables for ", tags$strong("X"), " and ", tags$strong("Y"),
                  " to set the scatterplot axes."),
          tags$li("Map ", tags$strong("Color"), ", ", tags$strong("Size"), ", or ",
                  tags$strong("Shape"), " to additional variables to encode more dimensions."),
          tags$li("Watch the ", tags$strong("Generated Code"), " panel update in real time ",
                  "to see the exact ", tags$code("aes()"), " call."),
          tags$li("If you turn on too many aesthetics at once, a ",
                  tags$strong("warning"), " will remind you about cognitive overload."),
          tags$li("Set any optional aesthetic to ", tags$em("None"), " to remove it.")
        )
      )
    ),

    hr(style = "margin-top: 8px; margin-bottom: 8px;"),

    p(strong("Axis mappings"), style = "margin-bottom: 4px; color: #002967;"),

    selectInput("x_var", "X axis:",
                choices  = numeric_cols,
                selected = "displ"),

    selectInput("y_var", "Y axis:",
                choices  = numeric_cols,
                selected = "hwy"),

    tags$hr(),

    p(strong("Optional aesthetics"), style = "margin-bottom: 4px; color: #002967;"),

    selectInput("color_var", "Color:",
                choices  = c(none_option, setNames(categorical_cols, categorical_cols)),
                selected = "none"),

    selectInput("size_var", "Size:",
                choices  = c(none_option, setNames(numeric_cols, numeric_cols)),
                selected = "none"),

    selectInput("shape_var", "Shape:",
                choices  = c(none_option, setNames(categorical_cols, categorical_cols)),
                selected = "none"),

    tags$hr(),

    p(strong("Generated code:"),
      style = "margin-bottom: 4px; font-size: 0.85rem; color: #002967;"),
    verbatimTextOutput("code_out")
  ),

  # -- Main content area --
  card(
    full_screen = TRUE,
    card_header("Aesthetic Mapping Sandbox", class = "bg-primary text-white"),
    card_body(
      wrap_spinner(plotOutput("aes_plot", height = "440px"))
    ),
    card_footer(
      "Each dropdown maps a data variable to a visual property. ",
      "The plot and generated code update live.",
      style = "font-size: 0.8rem; color: #64748b;"
    )
  ),

  # -- Warning callout (conditionally visible) --
  uiOutput("overload_warning"),

  tags$footer(
    style = paste0(
      "text-align: center; padding: 10px 0; font-size: 0.78rem; ",
      "color: #64748b; border-top: 1px solid #e2e8f0; margin-top: 16px;"
    ),
    "Gonzaga University | Dr. Vivek H. Patil"
  )
)

server <- function(input, output, session) {

  # -- Count how many optional aesthetics are active --
  active_count <- reactive({
    sum(c(
      input$color_var != "none",
      input$size_var  != "none",
      input$shape_var != "none"
    ))
  })

  # -- Build the generated aes() code string --
  aes_code <- reactive({
    req(input$x_var, input$y_var)

    parts <- c(
      paste0("x = ", input$x_var),
      paste0("y = ", input$y_var)
    )

    if (input$color_var != "none") {
      parts <- c(parts, paste0("color = ", input$color_var))
    }
    if (input$size_var != "none") {
      parts <- c(parts, paste0("size = ", input$size_var))
    }
    if (input$shape_var != "none") {
      parts <- c(parts, paste0("shape = ", input$shape_var))
    }

    paste0("ggplot(mpg, aes(", paste(parts, collapse = ", "), ")) +\n  geom_point(alpha = 0.7)")
  })

  output$code_out <- renderText({
    aes_code()
  })

  # -- Render the scatterplot --
  output$aes_plot <- renderPlot({
    req(input$x_var, input$y_var)
    validate(
      need(input$x_var %in% numeric_cols,
           "Please select a valid numeric column for X."),
      need(input$y_var %in% numeric_cols,
           "Please select a valid numeric column for Y.")
    )

    # Build the aes mapping dynamically
    mapping_args <- list(
      x = as.name(input$x_var),
      y = as.name(input$y_var)
    )

    use_color <- input$color_var != "none"
    use_size  <- input$size_var  != "none"
    use_shape <- input$shape_var != "none"

    if (use_color) mapping_args$colour <- as.name(input$color_var)
    if (use_size)  mapping_args$size   <- as.name(input$size_var)
    if (use_shape) mapping_args$shape  <- as.name(input$shape_var)

    mapping <- do.call(aes, mapping_args)

    # Validate shape variable has at most 6 unique levels
    if (use_shape) {
      n_levels <- length(unique(mpg[[input$shape_var]]))
      validate(
        need(n_levels <= 6,
             paste0("Shape works best with 6 or fewer categories. '",
                    input$shape_var, "' has ", n_levels,
                    " unique values. Try 'drv' or 'fl' instead."))
      )
    }

    # Start building the plot — build geom args dynamically to avoid
    # passing NULL which can override aesthetic mappings
    point_args <- list(alpha = 0.7)
    if (!use_size)  point_args$size  <- 3
    if (!use_color) point_args$color <- NAVY

    p <- ggplot(mpg, mapping) +
      do.call(geom_point, point_args)

    # Friendly axis labels
    label_lookup <- c(
      displ = "Engine Displacement (L)",
      cty   = "City MPG",
      hwy   = "Highway MPG",
      year  = "Model Year"
    )

    labs_args <- list(
      x = label_lookup[input$x_var],
      y = label_lookup[input$y_var]
    )

    if (use_color) labs_args$colour <- input$color_var
    if (use_size)  labs_args$size   <- input$size_var
    if (use_shape) labs_args$shape  <- input$shape_var

    p <- p + do.call(labs, labs_args)

    # Title that describes the current mapping
    active_aesthetics <- c("position (x, y)")
    if (use_color) active_aesthetics <- c(active_aesthetics, "color")
    if (use_size)  active_aesthetics <- c(active_aesthetics, "size")
    if (use_shape) active_aesthetics <- c(active_aesthetics, "shape")

    p <- p +
      labs(
        title    = paste0("Aesthetics: ", paste(active_aesthetics, collapse = " + ")),
        subtitle = paste0(input$y_var, " vs. ", input$x_var,
                          if (use_color) paste0(", colored by ", input$color_var) else "")
      )

    # Color palette for categorical color
    if (use_color) {
      p <- p + scale_color_brewer(palette = "Dark2")
    }

    # Size scale for continuous size
    if (use_size) {
      p <- p + scale_size_area(max_size = 10)
    }

    p + theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(color = NAVY, face = "bold", size = 15),
        plot.subtitle = element_text(color = "#475569", size = 11),
        legend.position = "right"
      )

  }, res = 110)

  # -- Warning panel when too many aesthetics are active --
  output$overload_warning <- renderUI({
    req(active_count())
    if (active_count() >= 3) {
      card(
        class = "border-warning",
        card_header(
          class = "bg-warning text-dark",
          tags$span(
            bsicons::bs_icon("exclamation-triangle-fill"),
            " Cognitive Overload Warning"
          )
        ),
        card_body(
          p(
            "You are mapping ", tags$strong(active_count() + 2),
            " variables simultaneously (x, y, ",
            paste(
              c(
                if (input$color_var != "none") "color",
                if (input$size_var  != "none") "size",
                if (input$shape_var != "none") "shape"
              ),
              collapse = ", "
            ),
            "). Research shows that viewers can track about ",
            tags$strong("3-4 visual channels"),
            " before accuracy drops sharply (Ware, 2021).",
            style = "font-size: 0.85rem; margin-bottom: 6px;"
          ),
          p(
            tags$em("Tip:"),
            " Consider whether each extra aesthetic truly adds insight, ",
            "or whether it adds visual noise. Often a simpler chart ",
            "with a single highlighted comparison tells a clearer story.",
            style = "font-size: 0.82rem; color: #374151; margin-bottom: 0;"
          )
        )
      )
    }
  })
}

shinyApp(ui, server)
