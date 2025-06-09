#' vertebrates UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_vertebrates_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    bs4Dash::tabItem(
      tabName = "vertebrates",
      fluidRow(
        bs4Dash::valueBoxOutput(ns("prey_presence")),
        bs4Dash::valueBoxOutput(ns("prey_composition"))
      ),

      div(
        id = ns("dynamic_boxes"),
        style = "display: flex; flex-wrap: wrap; gap: 10px; justify-content: flex-start;"
      )
    )
  )
}

#' vertebrates Server Functions
#'
#' @importFrom shiny NS tagList
#' @noRd
mod_vertebrates_server <- function(id, rv, parentSession) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # Get filtered data
    vert_filtered <- reactive({
      df <- rv$vert |>
        dplyr::filter(DNA_Diet_Host %in% c("Carnivore", "Omnivore"))

      # Apply filters only if something is selected
      if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
        df <- df[df$name_of_park %in% input$filter_park, ]
      }

      if (!is.null(input$filter_species) && length(input$filter_species) > 0) {
        df <- df[df$DNA_Common_Host %in% input$filter_species, ]
      }

      if (!is.null(input$filter_prey) && length(input$filter_prey) > 0 && active_box() %in% "prey_composition_box") {
        df <- df[df$DNA_Common_Prey %in% input$filter_prey, ]
      }

      df
    })

    # Track which boxes are open (dynamically)
    active_box <- reactiveVal(NULL)
    show_boxes <- reactiveValues()

    # Render the Runs ValueBox
    output$prey_presence <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Prey presence"),
        subtitle = "Carnivore samples with prey",
        icon = icon("circle-half-stroke"),
        footer = dash_open(target = "prey_presence_box", session = session)
      )
    })

    # Render the Metadata ValueBox dynamically
    output$prey_composition <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Prey composition"),
        subtitle = "Composition of carnivore diet",
        icon = icon("bars-progress"),
        footer = dash_open(target = "prey_composition_box", session = session)
      )
    })

    # List of ValueBoxes and corresponding Boxes
    boxes <- list(
      prey_presence_box = list(
        title = "Proportion of Samples with Prey",
        outputId = ns("prey_presence_plot"),
        content = div(
          style = "position: relative;",  # Enable absolute positioning inside
          # Floating Download button
          div(
            style = "position: absolute; top: -59px; right: 10px; z-index: 10;",
            downloadButton(
              outputId = ns("prey_presence_download"),
              label = "Download Data",
              class = "btn btn-sm btn-outline-secondary"
            )
          ),

          # Filters and plot
          tagList(
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
              ),

              # Variable selection
              conditionalPanel(
                condition = sprintf("input['%s'] != null && input['%s'] != ''",
                                    ns("filter_species"), ns("filter_species")),
                div(
                  style = "flex: 1;",
                  shinyWidgets::pickerInput(
                    inputId = ns("select_variable"),
                    label = "Select Grouping Variable",
                    choices = c("", "faecal_age", "faecal_texture", "species_sex", "species_age"),
                    selected = NULL,
                    multiple = FALSE,
                    options = list(
                      `actions-box` = TRUE,
                      liveSearch = TRUE,
                      title = "Nothing selected"
                    ),
                    width = "100%"
                  )
                )
              )
            ),
            ggiraph::girafeOutput(ns("prey_presence_plot"))
          )
        )
      ),
      prey_composition_box = list(
        title = "Composition of carnivore diet",
        outputId = ns("prey_composition_plot"),
        content = div(
          style = "position: relative;",  # allows absolute positioning for the button

          # Download button floating in the top-right
          div(
            style = "position: absolute; top: -59px; right: 10px; z-index: 10;",
            downloadButton(
              outputId = ns("prey_composition_download"),
              label = "Download Data",
              class = "btn btn-sm btn-outline-secondary"
            )
          ),

          # Main content layout
          tagList(
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
              ),

              # Prey species filter
              div(
                style = "flex: 1;",
                shinyWidgets::pickerInput(
                  inputId = ns("filter_prey"),
                  label = "Filter by Prey Species",
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

            # Bar chart
            shinycssloaders::withSpinner(
              ggiraph::girafeOutput(ns("prey_composition_plot")),
              color = "#8175AABF", type = 8),
          )
        )
      )
    )

    # Generalized click event handling
    observeEvent(input$toggle_box, {
      box_id <- input$toggle_box  # Get clicked box ID
      old_id <- active_box()

      # If another box is open, close it
      if (!is.null(old_id) && old_id != box_id) {
        removeUI(selector = paste0("#", ns(old_id)))
        show_boxes[[old_id]] <- FALSE
      }

      # Toggle the clicked box
      if (isTRUE(show_boxes[[box_id]])) {
        # If it's already open, close it
        removeUI(selector = paste0("#", ns(box_id)))
        show_boxes[[box_id]] <- FALSE
        active_box(NULL)
      } else {
        # If it's closed, open it
        insertUI(
          selector = paste0("#", ns("dynamic_boxes")),
          where = "beforeEnd",
          ui = div(
            id = ns(box_id),
            style = "width:100%; box-sizing:border-box; margin-bottom:10px;",
            bs4Dash::box(
              title = boxes[[box_id]]$title,
              solidHeader = FALSE,
              status = "success",
              collapsible = FALSE,
              width = 12,
              boxes[[box_id]]$content
            )
          )
        )
        show_boxes[[box_id]] <- TRUE
        active_box(box_id)
      }
    })

    # Observe box for updating picker input choices
    observeEvent(active_box(), {
      shinyjs::delay(100, {
        req(vert_filtered())

        shinyWidgets::updatePickerInput(
          session, "filter_park",
          choices = sort(unique(vert_filtered()$name_of_park))
        )

        if(active_box() %in% "prey_composition_box") {
          shinyWidgets::updatePickerInput(
            session, "filter_species",
            choices = sort(unique(vert_filtered() |> dplyr::filter(!is.na(DNA_Common_Prey)) |> dplyr::pull(DNA_Common_Host)))
          )

          shinyWidgets::updatePickerInput(
            session, "filter_prey",
            choices = sort(unique(vert_filtered() |> dplyr::filter(!is.na(DNA_Common_Prey)) |> dplyr::pull(DNA_Common_Prey)))
          )
        } else {
          shinyWidgets::updatePickerInput(
            session, "filter_species",
            choices = sort(unique(vert_filtered() |> dplyr::pull(DNA_Common_Host)))
          )

          shinyWidgets::updatePickerInput(
            session, "filter_prey",
            selected = NULL
          )
        }
      })
    })

    # Update park choices
    observe({
      req(vert_filtered())

      df <- vert_filtered()
      if (!is.null(input$filter_species) && length(input$filter_species) > 0) {
        df <- df[df$DNA_Common_Host %in% input$filter_species, ]
      }

      if (!is.null(input$filter_prey) && length(input$filter_prey) > 0) {
        df <- df[df$DNA_Common_Prey %in% input$filter_prey, ]
      }

      if(!is.null(active_box()) && active_box() %in% "prey_composition_box") {
        df <- df |>
          dplyr::filter(!is.na(DNA_Common_Prey))
      }

      updatePickerInput(session, "filter_park",
                        choices = sort(unique(df$name_of_park)),
                        selected = input$filter_park[input$filter_park %in% df$name_of_park])
    })

    # Update species choices
    observe({
      req(vert_filtered())

      df <- vert_filtered()
      if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
        df <- df[df$name_of_park %in% input$filter_park, ]
      }
      if (!is.null(input$filter_prey) && length(input$filter_prey) > 0) {
        df <- df[df$DNA_Common_Prey %in% input$filter_prey, ]
      }
      if(!is.null(active_box()) && active_box() %in% "prey_composition_box") {
        df <- df |>
          dplyr::filter(!is.na(DNA_Common_Prey))
      }

      updatePickerInput(session, "filter_species",
                        choices = sort(unique(df$DNA_Common_Host)),
                        selected = input$filter_species[input$filter_species %in% df$DNA_Common_Host])
    })

    # Update prey choices
    observe({
      req(vert_filtered())
      req(active_box() %in% "prey_composition_box")

      df <- vert_filtered()
      if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
        df <- df[df$name_of_park %in% input$filter_park, ]
      }
      if (!is.null(input$filter_species) && length(input$filter_species) > 0) {
        df <- df[df$DNA_Common_Host %in% input$filter_species, ]
      }

      updatePickerInput(session, "filter_prey",
                        choices = sort(unique(df$DNA_Common_Prey)),
                        selected = input$filter_prey[input$filter_prey %in% df$DNA_Common_Prey])
    })

    # Render prey presence plot
    prey_presence_data <- reactive({
      req(vert_filtered())

      df <- vert_filtered() |>
        dplyr::filter(DNA_Diet_Host %in% c("Carnivore", "Omnivore"),
                      is.na(DNA_Species_Miscellaneous)) |>
        dplyr::left_join(
          rv$meta[, c("barcode_id", "faecal_age", "faecal_texture", "species_sex", "species_age")],
          by = c("Barcode" = "barcode_id")
        ) |>
        dplyr::group_by(DNA_Common_Host, Barcode, name_of_park, faecal_age, faecal_texture, species_sex, species_age) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(Category = ifelse(n == 1, "No prey detected", "Prey detected"),
                      Category2 = paste0(Category, ";", name_of_park)) |>
        dplyr::mutate(Category2 = factor(Category2, levels = sort(unique(Category2)))) |>
        dplyr::filter(n > 0) |>
        dplyr::mutate(n = 1) |>
        dplyr::mutate(color = dplyr::case_when(
          name_of_park %in% "Akagera" & Category %in% "Prey detected" ~ "#901e7c",
          name_of_park %in% "Akagera" & Category %in% "No prey detected" ~ "#f5c8e0",
          name_of_park %in% "Iona" & Category %in% "Prey detected" ~ "#b52727",
          name_of_park %in% "Iona" & Category %in% "No prey detected" ~ "#FF9999",
          name_of_park %in% "Kafue" & Category %in% "Prey detected" ~ "#28a745",
          name_of_park %in% "Kafue" & Category %in% "No prey detected" ~ "#A8D5A1",
          name_of_park %in% "Odzala" & Category %in% "Prey detected" ~ "#002790",
          name_of_park %in% "Odzala" & Category %in% "No prey detected" ~ "#99CCFF",
          name_of_park %in% "Zakouma" & Category %in% "Prey detected" ~ "#ec7627",
          name_of_park %in% "Zakouma" & Category %in% "No prey detected" ~ "#fbcfaf",
          TRUE ~ "gray90"
        ),
        color = factor(color, levels = sort(unique(color), decreasing = TRUE)))

      if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
        df <- df[df$name_of_park %in% input$filter_park, ]
      }

      if (!is.null(input$filter_species) && length(input$filter_species) > 0) {
        df <- df[df$DNA_Common_Host %in% input$filter_species, ]
      }

      if (nzchar(input$select_variable)) {
        df <- df |>
          dplyr::count(Barcode, name_of_park, DNA_Common_Host, Category2, Category, color, !!rlang::sym(input$select_variable))
      }

      df
    })

    output$prey_presence_plot <- ggiraph::renderGirafe({
      req(prey_presence_data())

      # Use select_variable if filter_species is set
      x_var <- if (input$filter_species != "" && input$select_variable != "") {
        rlang::sym(input$select_variable)
      } else {
        rlang::sym("DNA_Common_Host")
      }

      p <- ggplot2::ggplot(prey_presence_data(), ggplot2::aes(x = !!x_var, y = n, fill = color)) +
        ggiraph::geom_bar_interactive(
          ggplot2::aes(
            tooltip = paste0("Sample ID: ", Barcode, "\nPark: ", name_of_park, "\nConsumer: ", DNA_Common_Host, "\nStatus: ", Category),
            group = Category2
          ),
          stat = "identity", alpha = 0.75
        ) +
        ggplot2::scale_fill_identity() +
        ggplot2::labs(y = "\n\n\nSamples") +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 6, angle = 45, hjust = 1, vjust = 1),
          axis.title.x = ggplot2::element_blank(),
          strip.background = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(0, "lines")
        )

      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_toolbar(saveaspng = TRUE)
        )
      )
    })

    # Render prey composition plot
    prey_composition_data <- reactive({
      req(rv$vert)

      df <- vert_filtered() |>
        dplyr::filter(!is.na(DNA_Common_Prey)) |>
        dplyr::group_by(Barcode, name_of_park, DNA_Common_Host, DNA_Common_Prey) |>
        dplyr::summarize(RRA_Prey = sum(RRA_Prey, na.rm = TRUE), .groups = "drop")

      if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
        df <- df[df$name_of_park %in% input$filter_park, ]
      }

      if (!is.null(input$filter_species) && length(input$filter_species) > 0) {
        df <- df[df$DNA_Common_Host %in% input$filter_species, ]
      }

      if (!is.null(input$filter_prey) && length(input$filter_prey) > 0) {
        df <- df[df$DNA_Common_Prey %in% input$filter_prey, ]
      }

      if(nrow(df) > 0 && length(unique(df$Barcode)) >= 2) {
        names_from_col <- if (is.null(input$filter_species) & !is.null(input$filter_prey)) {
          "DNA_Common_Host"
        } else {
          "DNA_Common_Prey"
        }

        clustered_order <- df |>
          dplyr::select(Barcode, name_of_park, DNA_Common_Host, DNA_Common_Prey, RRA_Prey) |>
          tidyr::pivot_wider(
            id_cols = c(Barcode, name_of_park),
            names_from = !!sym(names_from_col),
            values_from = RRA_Prey,
            values_fill = 0
          ) |>
          dplyr::group_split(name_of_park) |>
          purrr::keep(~ dplyr::n_distinct(.x$Barcode) >= 2) |>
          purrr::map_df(~ {
            mat <- as.matrix(dplyr::select(.x, -Barcode, -name_of_park))
            rownames(mat) <- .x$Barcode
            hc <- hclust(dist(mat))
            tibble::tibble(
              Barcode = hc$labels[hc$order],
              order = seq_along(hc$order),
              name_of_park = unique(.x$name_of_park)
            )
          })

        df <- df |>
          dplyr::left_join(clustered_order, by = c("Barcode", "name_of_park")) |>
          dplyr::mutate(Barcode = factor(Barcode, levels = clustered_order$Barcode[order(clustered_order$order)]))
      }

      df
    })

    output$prey_composition_plot <- ggiraph::renderGirafe({
      req(prey_composition_data())

      fill_var <- if (is.null(input$filter_species) && !is.null(input$filter_prey)) {
        "DNA_Common_Host"} else {
          "DNA_Common_Prey"}

      p <- ggplot2::ggplot(prey_composition_data(), ggplot2::aes(x = Barcode, y = RRA_Prey, fill = .data[[fill_var]])) +
        ggplot2::facet_grid(~ stringr::str_to_title(name_of_park), scales = "free_x", space = "free_x") +
        ggiraph::geom_bar_interactive(
          ggplot2::aes(
            tooltip = paste0("Sample ID: ", Barcode, "\nPark: ", name_of_park, "\nConsumer: ", DNA_Common_Host, "\nPrey: ", DNA_Common_Prey, "\nRRA: ", RRA_Prey),
            group = DNA_Common_Prey
          ),
          stat = "identity", alpha = 0.7
        ) +
        ggplot2::labs(y = "Relative Read Abundance") +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::guides(fill = "none") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          strip.background = ggplot2::element_blank(),
          strip.clip = "off",
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(0, "lines")
        )


      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_toolbar(saveaspng = TRUE)
        )
      )
    })

    # Download buttons
    output$prey_presence_download <- downloadHandler(
      filename = function() {
        paste0("AfricanParks_CarnivorePreyPresence_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(prey_presence_data())
        write.csv(prey_presence_data() |> dplyr::select(-c(n, Category2, color)) |> dplyr::relocate(Barcode, .before = DNA_Common_Host), file, na = "", row.names = FALSE)
      }
    )

    output$prey_composition_download <- downloadHandler(
      filename = function() {
        paste0("AfricanParks_CarnivorePreyComposition_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(prey_composition_data())
        write.csv(prey_composition_data() |> dplyr::select(-order), file, na = "", row.names = FALSE)
      }
    )

  })
}

dash_open <- function(target, session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  div(
    style = "display: flex; align-items: center; justify-content: center; width: 100%; height: 20px; cursor: pointer;",
    onclick = glue::glue("Shiny.setInputValue('{ns('toggle_box')}', '{target}', {{priority: 'event'}});"),
    div(
      style = "display: flex; flex-flow: row nowrap; align-items: center; justify-content: center; gap: 1em",
      "Details",
      tags$i(class = "fas fa-circle-arrow-right")
    )
  )
}
