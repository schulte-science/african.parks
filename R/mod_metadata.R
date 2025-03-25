#' metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny bs4Dash
mod_metadata_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
    .dropdown-menu {
      z-index: 1051 !important;
    }

    .bootstrap-select {
      z-index: 1051 !important;
    }
  ")),

    bs4Dash::tabItem(
      tabName = "metadata",
      fluidRow(
        bs4Dash::valueBoxOutput(ns("map")),
        bs4Dash::valueBoxOutput(ns("table"))
      ),

      div(
        id = ns("dynamic_boxes"),
        style = "display: flex; flex-wrap: wrap; gap: 10px; justify-content: flex-start;"
      )
    )
  )
}


#' metadata Server Functions
#'
#' @importFrom shiny NS tagList
#' @noRd
mod_metadata_server <- function(id, rv, parentSession) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # Track which boxes are open (dynamically)
    show_boxes <- reactiveValues()

    # Render the Map ValueBox
    output$map <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Map"),
        subtitle = "View all samples on a map",
        icon = icon("map"),
        footer = dash_open(target = "map_box", session = session)
      )
    })

    # Render the Table ValueBox dynamically
    output$table <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Table"),
        subtitle = "View all samples in a table",
        icon = icon("table"),
        footer = dash_open(target = "table_box", session = session)
      )
    })

    # List of ValueBoxes and corresponding Boxes
    boxes <- list(
      map_box = list(
        title = "Metadata Map",
        outputId = ns("metadata_map"),
        content = tagList(
          div(
            style = "margin-bottom: 10px; display: flex; flex-wrap: nowrap; gap: 15px; justify-content: space-between;",

            # Park filter
            div(
              style = "flex: 1;",
              shinyWidgets::pickerInput(
                inputId = ns("filter_park"),
                label = "Filter by Park",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  liveSearch = TRUE,
                  selectedTextFormat = "count > 1",
                  countSelectedText = "{0} selected"
                ),
                width = "100%"
              )
            ),

            # Status filter
            div(
              style = "flex: 1;",
              shinyWidgets::pickerInput(
                inputId = ns("filter_status"),
                label = "Filter by Run Status",
                choices = c("Completed", "Pending run", "Pending rerun"),
                selected = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  liveSearch = TRUE,
                  selectedTextFormat = "count > 1",
                  countSelectedText = "{0} selected"
                ),
                width = "100%"
              )
            ),

            # Host species filter
            div(
              style = "flex: 1;",
              shinyWidgets::pickerInput(
                inputId = ns("filter_species"),
                label = "Filter by Host Species",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  liveSearch = TRUE,
                  selectedTextFormat = "count > 1",
                  countSelectedText = "{0} selected"
                ),
                width = "100%"
              )
            )
          ),

          # Map output
          leaflet::leafletOutput(ns("metadata_map"), height = "600px")
        )
      ),
      table_box = list(
        title = "Metadata Table",
        outputId = ns("metadata_table"),
        content = DT::dataTableOutput(ns("metadata_table"))
      )
    )

    # Generalized Click Event Handling
    observeEvent(input$toggle_box, {
      box_id <- input$toggle_box  # Get clicked box ID

      # If there is an active box and it's different from the one clicked, remove it
      if (!is.null(rv$active_box) && rv$active_box != box_id) {
        removeUI(selector = paste0("#", ns(rv$active_box)))
        show_boxes[[rv$active_box]] <- FALSE
      }

      if (!is.null(boxes[[box_id]])) {
        if (is.null(show_boxes[[box_id]]) || !show_boxes[[box_id]]) {
          # Insert the new box dynamically
          insertUI(
            selector = paste0("#", ns("dynamic_boxes")),
            where = "beforeEnd",
            ui = div(
              id = ns(box_id),
              style = "width: 100%; box-sizing: border-box; margin-bottom: 10px;",  # Full width, stack, with spacing
              bs4Dash::box(
                title = boxes[[box_id]]$title,
                solidHeader = FALSE,
                status = "success",
                collapsible = FALSE,
                width = 12,  # Make it span full width inside the layout
                boxes[[box_id]]$content
              )
            )
          )
          show_boxes[[box_id]] <- TRUE
          rv$active_box <- box_id  # Update active box
        } else {
          # Remove the currently displayed box
          removeUI(selector = paste0("#", ns(box_id)))
          show_boxes[[box_id]] <- FALSE
          rv$active_box <- NULL  # Reset active box
        }
      }
    })

    # Observe metadata map box for updating picker input choices
    observeEvent(rv$active_box, {
      if (rv$active_box == "map_box") {
        # Delay slightly to let the UI render first
        shinyjs::delay(100, {
          req(rv$meta)

          shinyWidgets::updatePickerInput(
            session, "filter_park",
            choices = sort(unique(rv$meta$name_of_park))
          )

          shinyWidgets::updatePickerInput(
            session, "filter_species",
            choices = sort(unique(rv$meta$species_id_dna_common))
          )
        })
      }
    })

    # Render metadata map
    output$metadata_map <- leaflet::renderLeaflet({
      req(rv$meta)
      validate(
        need(nrow(rv$meta) > 0, "No data available for mapping")
      )

      # Create color palette based on name_of_park
      pal <- leaflet::colorFactor(
        palette = "Set2",  # You can also use custom colors
        domain = rv$meta$name_of_park
      )

      popup_data <- reactive({
        df <- rv$meta |>
          dplyr::mutate(
            status = dplyr::case_when(
              Reads_12SVert > 0 ~ "Completed",
              Reads_12SVert == 0 ~ "Pending rerun",
              TRUE ~ "Pending run"
            ),
            popup = paste0(
              "<strong>Sample ID:</strong> ", barcode_id,
              "<br><strong>Park:</strong> ", name_of_park,
              "<br><strong>Status:</strong> ", status,
              ifelse(
                is.na(species_id_dna_common) | species_id_dna_common == "",
                "",
                paste0("<br><strong>Host Species:</strong> ", species_id_dna_common)
              )
            ),
            fill_color = ifelse(is.na(species_id_dna_common), "#FFFFFF00", pal(name_of_park)),  # Transparent fill
            border_color = ifelse(is.na(species_id_dna_common), pal(name_of_park), "#000000")
          )

        if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
          df <- df[df$name_of_park %in% input$filter_park, ]
        }

        if (!is.null(input$filter_species) && length(input$filter_species) > 0) {
          df <- df[df$species_id_dna_common %in% input$filter_species, ]
        }

        if (!is.null(input$filter_status) && length(input$filter_status) > 0) {
          df <- df[df$status %in% input$filter_status, ]
        }

        df
      })


      leaflet::leaflet(data = popup_data()) |>
        leaflet::addTiles() |>
        leaflet::addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          popup = ~popup,
          radius = 6,
          fillColor = ~fill_color,
          fillOpacity = ifelse(is.na(popup_data()$species_id_dna_common), 0, 0.9),
          color = ~border_color,
          weight = 1,
          clusterOptions = leaflet::markerClusterOptions()
        )
    })

    # Render metadata table
    output$metadata_table <- DT::renderDT({
      req(rv$meta)

      DT::datatable(
        rv$meta,
        options = list(
          scrollX = TRUE,
          scrollY = "400px",
          paging = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('colvis'),
          columnDefs = list(
            list(visible = FALSE,
                 targets = which(!names(rv$meta) %in% c("barcode_id", "DateRun", "name_of_park", "date", "latitude", "longitude", "habitat_final", "species_id", "species_id_dna_common", "species_id_dna")) - 1)  # Select initial columns
          )
        ),
        rownames = FALSE,
        editable = TRUE,
        extensions = c("Buttons"),
        selection = "none"
      )
    })


  })
}
