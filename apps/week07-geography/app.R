library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)
library(scales)

# ---------------------------------------------------------------------------
# Gonzaga brand palette
# ---------------------------------------------------------------------------
NAVY <- "#002967"
RED  <- "#C41E3A"
GOLD <- "#B4975A"

# ---------------------------------------------------------------------------
# Optional packages: graceful fallback if not installed
# ---------------------------------------------------------------------------
has_leaflet <- requireNamespace("leaflet", quietly = TRUE) &&
               requireNamespace("sf", quietly = TRUE)
if (has_leaflet) {
  library(leaflet)
  library(sf)
}

has_spinner <- requireNamespace("shinycssloaders", quietly = TRUE)
if (has_spinner) library(shinycssloaders)

# Helper: wrap output in spinner if shinycssloaders is available
wrap_spinner <- function(output_ui) {
  if (has_spinner) {
    shinycssloaders::withSpinner(output_ui, type = 6, color = NAVY)
  } else {
    output_ui
  }
}

# ---------------------------------------------------------------------------
# Build state dataset once (R built-in data)
# ---------------------------------------------------------------------------
state_df <- tibble(
  state      = tolower(state.name),
  abbr       = state.abb,
  income     = state.x77[, "Income"],
  life_exp   = state.x77[, "Life Exp"],
  illiteracy = state.x77[, "Illiteracy"],
  murder     = state.x77[, "Murder"],
  hs_grad    = state.x77[, "HS Grad"]
)

us_map <- map_data("state")

var_meta <- list(
  income     = list(label = "Per Capita Income ($)",    palette = "plasma", dir =  1, fmt = "dollar"),
  life_exp   = list(label = "Life Expectancy (years)",  palette = "RdYlGn", dir =  1, fmt = "number"),
  illiteracy = list(label = "Illiteracy Rate (%)",      palette = "YlOrRd", dir =  1, fmt = "number"),
  murder     = list(label = "Murder Rate (per 100k)",   palette = "Reds",   dir =  1, fmt = "number"),
  hs_grad    = list(label = "HS Graduation Rate (%)",   palette = "Blues",  dir =  1, fmt = "number")
)

proj_list <- c(
  "Albers Equal Area" = "albers",
  "Mercator"          = "mercator",
  "Mollweide"         = "mollweide"
)

# ---------------------------------------------------------------------------
# Prepare Leaflet-ready sf object once (if leaflet available)
# ---------------------------------------------------------------------------
if (has_leaflet) {
  states_sf <- tryCatch({
    raw <- maps::map("state", fill = TRUE, plot = FALSE)
    sf_obj <- sf::st_as_sf(raw)
    sf_obj$state <- tolower(sf_obj$ID)
    sf_obj <- sf_obj |> left_join(state_df, by = "state")
    sf_obj
  }, error = function(e) NULL)
  if (is.null(states_sf)) has_leaflet <- FALSE
}

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

  # --- Sidebar -----------------------------------------------------------
  sidebar = sidebar(
    width = 280, bg = "#f1f5f9",

    # Instruction accordion
    accordion(
      open = FALSE,
      accordion_panel(
        title = "How to Use This Explorer",
        icon  = bsicons::bs_icon("question-circle"),
        p("1. Choose a variable to map from the radio buttons below."),
        p("2. Switch between the Static (ggplot2) and Interactive (Leaflet)
           tabs to explore the data in different ways."),
        p("3. Adjust the projection, color palette, and legend position
           to customise the static map."),
        p("4. Enable the colorblind simulation to see how the map appears
           under deuteranopia (red-green colorblindness)."),
        p("5. On the Leaflet tab, hover over or click a state to see its
           value in the info panel below."),
        style = "font-size: 0.82rem; line-height: 1.55;"
      )
    ),

    tags$hr(style = "margin: 6px 0;"),

    radioButtons("variable", "Variable to map:",
      choices  = c("Income"          = "income",
                   "Life Expectancy" = "life_exp",
                   "Illiteracy Rate" = "illiteracy",
                   "Murder Rate"     = "murder",
                   "HS Graduation"   = "hs_grad"),
      selected = "income"
    ),
    tags$hr(style = "margin: 6px 0;"),

    radioButtons("projection", "Map projection:",
      choices  = proj_list,
      selected = "albers"
    ),
    tags$hr(style = "margin: 6px 0;"),

    radioButtons("palette", "Color palette:",
      choices  = c("Variable default" = "default",
                   "Viridis (plasma)"  = "plasma",
                   "Red-Yellow-Green"  = "RdYlGn",
                   "Sequential Blues"  = "Blues"),
      selected = "default"
    ),
    checkboxInput("reverse_pal", "Reverse palette", FALSE),
    tags$hr(style = "margin: 6px 0;"),

    # Legend position controls
    radioButtons("legend_pos", "Legend position:",
      choices  = c("Bottom" = "bottom",
                   "Right"  = "right",
                   "None"   = "none"),
      selected = "bottom"
    ),
    tags$hr(style = "margin: 6px 0;"),

    # Colorblind simulation toggle
    checkboxInput("colorblind", "Simulate deuteranopia (red-green colorblindness)", FALSE),
    conditionalPanel(
      condition = "input.colorblind",
      card(
        card_body(
          p(tags$strong("Why this matters:"),
            "Approximately 8% of men and 0.5% of women have red-green color
             vision deficiency. Using perceptually uniform palettes like viridis
             ensures your data visualisations are accessible to everyone.",
            style = "font-size: 0.78rem; color: #374151; line-height: 1.5;"),
          style = "padding: 6px;"
        )
      )
    ),

    tags$hr(style = "margin: 6px 0;"),

    # Projection note
    card(
      card_body(uiOutput("map_note"), style = "padding: 6px;")
    ),

    tags$hr(style = "margin: 6px 0;"),

    # State info panel (populated on hover/click)
    card(
      card_header(
        tags$span(bsicons::bs_icon("geo-alt-fill"), " State Info"),
        class = "bg-primary text-white",
        style = "padding: 6px 10px; font-size: 0.85rem;"
      ),
      card_body(
        uiOutput("state_info"),
        style = "padding: 8px; min-height: 50px;"
      )
    )
  ),

  # --- Main panel --------------------------------------------------------
  navset_card_tab(
    id        = "map_tabs",
    full_screen = TRUE,
    header    = card_header(
      "U.S. Choropleth Map Explorer",
      class = "bg-primary text-white"
    ),

    # Tab 1: Static ggplot2 map
    nav_panel(
      title = "Static (ggplot2)",
      card_body(
        wrap_spinner(plotOutput("choropleth", height = "500px",
                                click = "ggplot_click",
                                hover = hoverOpts("ggplot_hover", delay = 150,
                                                   delayType = "throttle")))
      )
    ),

    # Tab 2: Interactive Leaflet map (or fallback message)
    nav_panel(
      title = "Interactive (Leaflet)",
      card_body(
        if (has_leaflet) {
          wrap_spinner(leafletOutput("leaflet_map", height = "500px"))
        } else {
          div(
            style = "padding: 40px; text-align: center; color: #64748b;",
            h4("Leaflet is not available"),
            p("Install the", tags$code("leaflet"), "and", tags$code("sf"),
              "packages to enable the interactive map tab."),
            p(tags$code('install.packages(c("leaflet", "sf"))'))
          )
        }
      )
    ),

    footer = card_footer(
      div(
        style = "display: flex; justify-content: space-between; align-items: center;
                 font-size: 0.78rem; color: #64748b;",
        span("Data: U.S. state.x77 dataset (c. 1977). ",
             "Try switching projections \u2014 Mercator distorts area at higher latitudes."),
        span(style = paste0("color:", GOLD, "; font-weight: 600;"),
             "Gonzaga University | Dr. Vivek H. Patil")
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # --- Joined map data for ggplot -----------------------------------------
  map_data_joined <- reactive({
    us_map |> left_join(state_df, by = c("region" = "state"))
  })

  # --- Determine active palette & direction -------------------------------
  active_palette <- reactive({
    req(input$variable, input$palette)
    vm <- var_meta[[input$variable]]
    if (input$colorblind) {
      list(pal = "viridis", dir = 1)
    } else {
      pal <- if (input$palette == "default") vm$palette else input$palette
      dir <- if (input$reverse_pal) -vm$dir else vm$dir
      list(pal = pal, dir = dir)
    }
  })

  # --- Format helper ------------------------------------------------------
  fmt_value <- function(val, variable) {
    vm <- var_meta[[variable]]
    if (is.null(val) || is.na(val)) return("N/A")
    if (vm$fmt == "dollar") {
      scales::dollar(val)
    } else {
      formatC(val, format = "f", digits = 1)
    }
  }

  # --- Reactive: selected state info (from ggplot click/hover) -----------
  selected_state <- reactiveVal(NULL)

  # Observe ggplot hover
  observe({
    hover <- input$ggplot_hover
    if (!is.null(hover)) {
      d <- map_data_joined()
      # Find nearest state by hover coordinates
      near <- nearPoints(d, hover, xvar = "long", yvar = "lat",
                         maxpoints = 1, threshold = 20)
      if (nrow(near) > 0) {
        st <- near$region[1]
        row <- state_df |> filter(state == st)
        if (nrow(row) > 0) {
          selected_state(list(
            name  = tools::toTitleCase(row$state),
            abbr  = row$abbr,
            value = row[[input$variable]],
            var   = input$variable
          ))
        }
      }
    }
  })

  # Observe ggplot click (sticky selection)
  observe({
    click <- input$ggplot_click
    if (!is.null(click)) {
      d <- map_data_joined()
      near <- nearPoints(d, click, xvar = "long", yvar = "lat",
                         maxpoints = 1, threshold = 20)
      if (nrow(near) > 0) {
        st <- near$region[1]
        row <- state_df |> filter(state == st)
        if (nrow(row) > 0) {
          selected_state(list(
            name  = tools::toTitleCase(row$state),
            abbr  = row$abbr,
            value = row[[input$variable]],
            var   = input$variable
          ))
        }
      }
    }
  })

  # --- State info panel output -------------------------------------------
  output$state_info <- renderUI({
    info <- selected_state()
    if (is.null(info)) {
      return(
        p("Hover over or click a state to see its details.",
          style = "font-size: 0.8rem; color: #94a3b8; font-style: italic;")
      )
    }
    vm <- var_meta[[info$var]]
    tagList(
      h5(paste0(info$name, " (", info$abbr, ")"),
         style = paste0("margin: 0 0 4px 0; color: ", NAVY, "; font-weight: 700;")),
      p(tags$strong(vm$label, ": "),
        fmt_value(info$value, info$var),
        style = "margin: 0; font-size: 0.88rem;")
    )
  })

  # -----------------------------------------------------------------------
  # TAB 1: Static ggplot2 choropleth
  # -----------------------------------------------------------------------
  output$choropleth <- renderPlot({
    d <- map_data_joined()
    req(nrow(d) > 0)
    validate(need(input$variable %in% names(var_meta),
                  "Please select a valid variable."))

    vm     <- var_meta[[input$variable]]
    ap     <- active_palette()
    pal    <- ap$pal
    dir    <- if (input$reverse_pal && !input$colorblind) ap$dir else ap$dir

    # Legend position
    leg_pos <- input$legend_pos %||% "bottom"

    p <- ggplot(d, aes(x = long, y = lat, group = group,
                       fill = .data[[input$variable]])) +
      geom_polygon(color = "white", linewidth = 0.25) +
      labs(
        title   = paste("U.S. States by", vm$label),
        fill    = vm$label,
        caption = "Source: R built-in state.x77 dataset"
      ) +
      theme_void(base_size = 13) +
      theme(
        plot.title       = element_text(color = NAVY, face = "bold",
                                        hjust = 0.5, size = 15),
        legend.position  = leg_pos,
        legend.key.width = unit(2, "cm"),
        plot.caption     = element_text(color = "#94a3b8", size = 9)
      )

    # Adjust legend for "right"
    if (leg_pos == "right") {
      p <- p + theme(legend.key.width = unit(0.6, "cm"),
                     legend.key.height = unit(1.5, "cm"))
    }

    # Label formatter
    lbl_fn <- if (vm$fmt == "dollar") {
      scales::dollar_format()
    } else {
      scales::number_format(accuracy = 0.1)
    }

    # Apply palette
    if (pal %in% c("viridis", "plasma", "magma", "inferno", "cividis")) {
      p <- p + scale_fill_viridis_c(option = pal, direction = dir, labels = lbl_fn)
    } else {
      p <- p + scale_fill_distiller(palette = pal, direction = dir, labels = lbl_fn)
    }

    # Apply projection
    if (input$projection == "albers") {
      p <- p + coord_map("albers", lat0 = 39, lat1 = 45)
    } else if (input$projection == "mollweide") {
      p <- p + coord_map("mollweide")
    } else {
      p <- p + coord_map("mercator")
    }

    p
  }, res = 110)

  # -----------------------------------------------------------------------
  # TAB 2: Interactive Leaflet map
  # -----------------------------------------------------------------------
  if (has_leaflet) {
    output$leaflet_map <- renderLeaflet({
      req(input$variable)
      validate(need(input$variable %in% names(var_meta),
                    "Please select a valid variable."))
      req(states_sf)

      vm  <- var_meta[[input$variable]]
      ap  <- active_palette()
      pal <- ap$pal
      dir <- ap$dir

      vals <- states_sf[[input$variable]]
      req(length(vals) > 0)

      # Build colour palette for Leaflet
      if (pal %in% c("viridis", "plasma", "magma", "inferno", "cividis")) {
        color_fn <- colorNumeric(
          palette = viridisLite::viridis(256, option = pal,
                                         direction = if (dir == -1) -1 else 1),
          domain  = vals, na.color = "#cccccc"
        )
      } else {
        # Use RColorBrewer distiller-style palette
        brew_cols <- tryCatch({
          RColorBrewer::brewer.pal(9, pal)
        }, error = function(e) {
          RColorBrewer::brewer.pal(9, "Blues")
        })
        if (dir == -1) brew_cols <- rev(brew_cols)
        color_fn <- colorNumeric(
          palette = colorRampPalette(brew_cols)(256),
          domain  = vals, na.color = "#cccccc"
        )
      }

      # Label formatter for tooltip
      lbl_fmt <- if (vm$fmt == "dollar") {
        function(x) scales::dollar(x)
      } else {
        function(x) formatC(x, format = "f", digits = 1)
      }

      # Build labels
      labels <- sprintf(
        "<strong>%s (%s)</strong><br/>%s: %s",
        tools::toTitleCase(states_sf$state),
        states_sf$abbr,
        vm$label,
        lbl_fmt(vals)
      ) |> lapply(htmltools::HTML)

      leaflet(states_sf) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(lng = -96, lat = 37.8, zoom = 4) |>
        addPolygons(
          fillColor   = ~color_fn(vals),
          fillOpacity = 0.75,
          weight      = 1,
          color       = "white",
          dashArray   = "",
          highlight   = highlightOptions(
            weight    = 3,
            color     = NAVY,
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label        = labels,
          layerId      = ~state,
          labelOptions = labelOptions(
            style     = list("font-weight" = "normal", padding = "4px 8px"),
            textsize  = "13px",
            direction = "auto"
          )
        ) |>
        addLegend(
          position = "bottomright",
          pal      = color_fn,
          values   = vals,
          title    = vm$label,
          opacity  = 0.85,
          labFormat = labelFormat(prefix = if (vm$fmt == "dollar") "$" else "",
                                  digits = if (vm$fmt == "dollar") 0 else 1)
        )
    })

    # Observe Leaflet shape clicks to populate state info panel
    observe({
      click <- input$leaflet_map_shape_click
      if (!is.null(click)) {
        st <- click$id
        if (!is.null(st)) {
          row <- state_df |> filter(state == st)
          if (nrow(row) > 0) {
            selected_state(list(
              name  = tools::toTitleCase(row$state),
              abbr  = row$abbr,
              value = row[[input$variable]],
              var   = input$variable
            ))
          }
        }
      }
    })

    # Observe Leaflet hover (mouseover) to populate state info panel
    observe({
      hover <- input$leaflet_map_shape_mouseover
      if (!is.null(hover)) {
        st <- hover$id
        if (!is.null(st)) {
          row <- state_df |> filter(state == st)
          if (nrow(row) > 0) {
            selected_state(list(
              name  = tools::toTitleCase(row$state),
              abbr  = row$abbr,
              value = row[[input$variable]],
              var   = input$variable
            ))
          }
        }
      }
    })
  }

  # --- Projection note ---------------------------------------------------
  output$map_note <- renderUI({
    req(input$projection)
    note <- switch(input$projection,
      albers    = "Albers Equal Area: preserves relative area sizes. Best for the U.S. \u2014 the standard for American choropleth maps.",
      mercator  = "Mercator: preserves shape but distorts area. Note how northern states appear larger than they should be.",
      mollweide = "Mollweide: equal-area projection often used for world maps. Shows how projection choice changes the story."
    )
    validate(need(!is.null(note), "Select a projection."))
    p(note, style = "font-size: 0.8rem; color: #374151; line-height: 1.5;")
  })
}

shinyApp(ui, server)
