library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(datasauRus)

has_spinners <- requireNamespace("shinycssloaders", quietly = TRUE)
if (has_spinners) library(shinycssloaders)

wrap_spinner <- function(ui_element, color = "#002967") {
  if (has_spinners) {
    shinycssloaders::withSpinner(ui_element, color = color, type = 6)
  } else {
    ui_element
  }
}

NAVY  <- "#002967"
RED   <- "#C41E3A"
GOLD  <- "#B4975A"

datasets <- sort(unique(datasaurus_dozen$dataset))

make_plot <- function(data, title_label, point_color = NAVY) {
  ggplot(data, aes(x = x, y = y)) +
    geom_point(color = point_color, size = 2.5, alpha = 0.75) +
    coord_cartesian(xlim = c(10, 100), ylim = c(-5, 105)) +
    labs(
      title    = paste0("Dataset: '", title_label, "'"),
      subtitle = "Same summary stats \u2014 completely different picture",
      x = "X", y = "Y"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title    = element_text(color = NAVY, face = "bold"),
      plot.subtitle = element_text(color = "#475569", size = 11)
    )
}

compute_stats <- function(d) {
  data.frame(
    Statistic = c("Mean X", "Mean Y", "SD X", "SD Y", "Cor(X,Y)"),
    Value     = c(
      round(mean(d$x), 2),
      round(mean(d$y), 2),
      round(sd(d$x),   2),
      round(sd(d$y),   2),
      round(cor(d$x, d$y), 2)
    )
  )
}

ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = NAVY,
    base_font  = font_google("Inter")
  ),
  sidebar = sidebar(
    width = 260,
    bg = "#f1f5f9",

    accordion(
      id = "help_accordion",
      open = FALSE,
      accordion_panel(
        title = "How to Use This Explorer",
        icon  = bsicons::bs_icon("info-circle"),
        tags$ul(
          style = "font-size: 0.82rem; padding-left: 1.1rem; margin-bottom: 0;",
          tags$li("Use the dropdown to pick any of the 13 datasets and see its scatterplot."),
          tags$li("Enable ", tags$strong("Compare Mode"), " to view two datasets side by side and spot visual differences."),
          tags$li("Press ", tags$strong("Play All"), " to auto-cycle through every dataset (1.5 s each). Hit ", tags$strong("Stop"), " to pause."),
          tags$li("Check the Summary Statistics table \u2014 the numbers stay nearly identical across all datasets!")
        )
      )
    ),

    hr(style = "margin-top: 8px; margin-bottom: 8px;"),

    selectInput("dataset", "Choose a dataset:",
                choices  = datasets,
                selected = "dino"),

    checkboxInput("compare_mode", "Compare Mode", value = FALSE),

    conditionalPanel(
      condition = "input.compare_mode == true",
      selectInput("dataset2", "Second dataset:",
                  choices  = datasets,
                  selected = "star")
    ),

    hr(style = "margin-top: 4px; margin-bottom: 8px;"),

    fluidRow(
      column(6, actionButton("play_btn", "Play All",
                             icon  = icon("play"),
                             class = "btn-sm btn-primary w-100")),
      column(6, actionButton("stop_btn", "Stop",
                             icon  = icon("stop"),
                             class = "btn-sm btn-outline-danger w-100"))
    ),

    hr(style = "margin-top: 8px; margin-bottom: 8px;"),

    card(
      card_header("Summary Statistics"),
      card_body(
        tableOutput("stats_tbl"),
        p(em("Notice: these never change!"),
          style = "font-size:0.78rem; color:#64748b; margin-top:4px;")
      )
    )
  ),

  card(
    full_screen = TRUE,
    card_header(
      "Always Plot Your Data",
      class = "bg-primary text-white"
    ),
    card_body(
      conditionalPanel(
        condition = "input.compare_mode == false",
        wrap_spinner(plotOutput("main_plot", height = "420px"))
      ),
      conditionalPanel(
        condition = "input.compare_mode == true",
        fluidRow(
          column(6, wrap_spinner(plotOutput("compare_plot_left",  height = "420px"))),
          column(6, wrap_spinner(plotOutput("compare_plot_right", height = "420px")))
        )
      )
    ),
    card_footer(
      "All 13 datasets share: mean x \u2248 54.3, mean y \u2248 47.8, sd x \u2248 16.8, sd y \u2248 26.9, cor \u2248 \u22120.06",
      style = "font-size:0.8rem; color:#64748b;"
    )
  ),

  tags$footer(
    style = paste0(
      "text-align:center; padding:10px 0; font-size:0.78rem; ",
      "color:#64748b; border-top:1px solid #e2e8f0; margin-top:16px;"
    ),
    "Gonzaga University | Dr. Vivek H. Patil"
  )
)

server <- function(input, output, session) {

  animation <- reactiveValues(playing = FALSE, index = 1L)

  observeEvent(input$play_btn, {
    current_idx <- match(input$dataset, datasets)
    if (is.na(current_idx)) current_idx <- 1L
    animation$index   <- current_idx
    animation$playing <- TRUE
  })

  observeEvent(input$stop_btn, {
    animation$playing <- FALSE
  })

  observe({
    req(animation$playing)
    if (!animation$playing) return()

    invalidateLater(1500, session)

    isolate({
      idx <- animation$index
      updateSelectInput(session, "dataset", selected = datasets[idx])

      next_idx <- idx + 1L
      if (next_idx > length(datasets)) {
        animation$playing <- FALSE
        animation$index   <- 1L
      } else {
        animation$index <- next_idx
      }
    })
  })

  selected_data <- reactive({
    req(input$dataset)
    datasaurus_dozen |> filter(dataset == input$dataset)
  })

  selected_data2 <- reactive({
    req(input$dataset2)
    datasaurus_dozen |> filter(dataset == input$dataset2)
  })

  output$stats_tbl <- renderTable({
    d <- selected_data()
    compute_stats(d)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
     width = "100%", align = "lr")

  output$main_plot <- renderPlot({
    req(input$dataset)
    make_plot(selected_data(), input$dataset)
  }, res = 110)

  output$compare_plot_left <- renderPlot({
    req(input$compare_mode)
    req(input$dataset)
    make_plot(selected_data(), input$dataset, point_color = NAVY)
  }, res = 110)

  output$compare_plot_right <- renderPlot({
    req(input$compare_mode)
    req(input$dataset2)
    make_plot(selected_data2(), input$dataset2, point_color = RED)
  }, res = 110)
}

shinyApp(ui, server)
