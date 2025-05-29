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

    # Get filtered metadata
    meta <- reactiveVal({
      df <- isolate(rv$meta) |>
        dplyr::mutate(status = dplyr::case_when(
          Reads_12SVert > 0 ~ "Completed",
          Reads_12SVert == 0 ~ "Pending rerun",
          TRUE ~ "Pending run"
          )
        )
      df
    })

    meta_filtered <- reactive({
      df <- meta()

      # Apply filters only if something is selected
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

          div(style = "margin-bottom: 10px; display: flex;",
              span("Clustered", style = "vertical-align: middle; font-size: 14px; margin-right: 10px;"),
              div(style = "display: flex; align-items: center;",
                  materialSwitch(inputId = ns("map_toggle"),
                                 label = NULL,
                                 value = FALSE,
                                 status = "primary",
                                 inline = TRUE)
              ),
              span("Unclustered", style = "vertical-align: middle; font-size: 14px;")
          ),

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
          leaflet::leafletOutput(ns("metadata_map"), height = "600px"),
          div(style = "margin-top: 10px; font-style: italic;",
              textOutput(ns("metadata_warning")))
        )
      ),
      table_box = list(
        title = "Metadata Table",
        outputId = ns("metadata_table"),
        content = div(
          style = "position: relative;",
          div(
            style = "position: absolute; top: -59px; right: 10px; z-index: 10;",
            downloadButton(
              outputId = ns("metadata_download"),
              label = "Download Data",
              class = "btn btn-sm btn-outline-secondary"
            )
          ),
          DT::dataTableOutput(ns("metadata_table"))
        )
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

    # Observe box for updating picker input choices
    observeEvent(rv$active_box, {
      if (rv$active_box == "map_box") {
        # Delay slightly to let the UI render first
        shinyjs::delay(100, {
          req(meta_filtered())

          shinyWidgets::updatePickerInput(
            session, "filter_park",
            choices = sort(unique(meta_filtered()$name_of_park))
          )

          shinyWidgets::updatePickerInput(
            session, "filter_species",
            choices = sort(unique(meta_filtered()$species_id_dna_common))
          )
        })
      }
    })

    # Update park choices
    observe({
      req(meta())

      df <- meta() |>
        dplyr::filter(!is.na(latitude))
      if (!is.null(input$filter_species) && length(input$filter_species) > 0) {
        df <- df[df$species_id_dna_common %in% input$filter_species, ]
      }
      if (!is.null(input$filter_status) && length(input$filter_status) > 0) {
        df <- df[df$status %in% input$filter_status, ]
      }

      updatePickerInput(session, "filter_park",
                        choices = sort(unique(df$name_of_park)),
                        selected = input$filter_park[input$filter_park %in% df$name_of_park])
    })

    # Update species choices
    observe({
      req(meta())

      df <- meta() |>
        dplyr::filter(!is.na(latitude))
      if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
        df <- df[df$name_of_park %in% input$filter_park, ]
      }
      if (!is.null(input$filter_status) && length(input$filter_status) > 0) {
        df <- df[df$status %in% input$filter_status, ]
      }

      updatePickerInput(session, "filter_species",
                        choices = sort(unique(df$species_id_dna_common)),
                        selected = input$filter_species[input$filter_species %in% df$species_id_dna_common])
    })

    # Update status choices
    observe({
      req(meta())

      df <- meta() |>
        dplyr::filter(!is.na(latitude))
      if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
        df <- df[df$name_of_park %in% input$filter_park, ]
      }
      if (!is.null(input$filter_species) && length(input$filter_species) > 0) {
        df <- df[df$species_id_dna_common %in% input$filter_species, ]
      }

      updatePickerInput(session, "filter_status",
                        choices = sort(unique(df$status)),
                        selected = input$filter_status[input$filter_status %in% df$status])
    })

    # Get metadata map data
    meta_map <- reactive({
      req(meta_filtered())

      # Create color palette and popup data
      colors <- data.frame(
        name_of_park = c("akagera", "iona", "kafue", "odzala_okoua", "zakouma"),
        fill_color = c("#901e7c", "#b52727", "#28a745", "#002790", "#ec7627")
      )

      popup_data <- meta_filtered() |>
        dplyr::mutate(
          popup = paste0(
            "<strong>Sample ID:</strong> ", barcode_id,
            "<br><strong>Park:</strong> ", stringr::str_to_sentence(name_of_park),
            "<br><strong>Status:</strong> ", status,
            ifelse(
              is.na(species_id_dna_common) | species_id_dna_common == "",
              "",
              paste0("<br><strong>Host Species:</strong> ", species_id_dna_common)
            )
          )
        ) |>
        dplyr::left_join(colors, by = "name_of_park") |>
        dplyr::mutate(
          fill_color = ifelse(is.na(species_id_dna_common), "#FFFFFF00", fill_color),
          border_color = ifelse(is.na(species_id_dna_common), fill_color, "#000000")
        )

      if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
        popup_data <- popup_data[popup_data$name_of_park %in% input$filter_park, ]
      }

      if (!is.null(input$filter_species) && length(input$filter_species) > 0) {
        popup_data <- popup_data[popup_data$species_id_dna_common %in% input$filter_species, ]
      }

      if (!is.null(input$filter_status) && length(input$filter_status) > 0) {
        popup_data <- popup_data[popup_data$status %in% input$filter_status, ]
      }

      popup_data
    })

    # Initialize metadata map
    output$metadata_map <- leaflet::renderLeaflet({
      req(meta_filtered())
      validate(
        need(nrow(meta_filtered()) > 0, "No data available for mapping")
      )

      leaflet::leaflet() |>
        leaflet::addTiles(attribution = "") |>
        leaflet::addCircleMarkers(
          data = meta_map(),
          lng = ~longitude,
          lat = ~latitude,
          popup = ~popup,
          radius = 6,
          fillColor = ~fill_color,
          fillOpacity = ifelse(is.na(meta_map()$species_id_dna_common), 0, 0.9),
          color = ~border_color,
          weight = 1,
          clusterOptions = leaflet::markerClusterOptions()
        ) |>
        leaflet::addScaleBar(position = "bottomleft", options = leaflet::scaleBarOptions(metric = TRUE))
    })

    # Render metadata map
    observeEvent({
      input$map_toggle
      meta_filtered()
    }, {
      req(meta_filtered())

      leaflet::leafletProxy("metadata_map") |>
        leaflet::clearMarkers() |>
        leaflet::clearMarkerClusters() |>
        leaflet::addCircleMarkers(
          data = meta_map(),
          lng = ~longitude,
          lat = ~latitude,
          popup = ~popup,
          radius = 6,
          fillColor = ~fill_color,
          fillOpacity = ifelse(is.na(meta_map()$species_id_dna_common), 0, 0.9),
          color = ~border_color,
          weight = 1,
          clusterOptions = if (is.null(input$map_toggle) || input$map_toggle == FALSE) {
            leaflet::markerClusterOptions()
          } else {
            NULL
          }
        )
    })

    # Render metadata table
    output$metadata_table <- DT::renderDT({
      req(meta())

      DT::datatable(
        meta() |> dplyr::rename(date_run = DateRun),
        options = list(
          scrollX = TRUE,
          scrollY = "400px",
          paging = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('colvis'),
          columnDefs = list(
            list(visible = FALSE,
                 targets = which(!names(meta()) %in% c("barcode_id", "name_of_park", "date_run", "date", "latitude", "longitude", "habitat_final", "species_id", "species_id_dna_common", "species_id_dna")) - 1)  # Select initial columns
          )
        ),
        rownames = FALSE,
        editable = FALSE,
        extensions = c("Buttons"),
        selection = "none"
      )
    })

    # Download data
    output$metadata_download <- downloadHandler(
      filename = function() {
        paste0("AfricanParks_Metadata_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(meta_filtered())
        write.csv(meta_filtered(), file, na = "", row.names = FALSE)
      }
    )

  })
}
