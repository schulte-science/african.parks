#' vertebrates UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny bs4Dash
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

    # Track which boxes are open (dynamically)
    show_boxes <- reactiveValues()

    # Render the Runs ValueBox
    output$prey_presence <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Prey presence"),
        subtitle = "Carnivore samples with prey",
        icon = icon("bars-progress"),
        footer = dash_open(target = "prey_presence_box", session = session)
      )
    })

    # Render the Metadata ValueBox dynamically
    output$prey_composition <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Prey composition"),
        subtitle = "Composition of carnivore diet",
        icon = icon("circle-nodes"),
        footer = dash_open(target = "prey_composition_box", session = session)
      )
    })

    # List of ValueBoxes and corresponding Boxes
    boxes <- list(
      prey_presence_box = list(
        title = "Proportion of Samples with Prey",
        outputId = ns("prey_presence_plot"),
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

            # Host species filter
            div(
              style = "flex: 1;",
              shinyWidgets::pickerInput(
                inputId = ns("filter_species"),
                label = "Filter by Host Species",
                choices = NULL,
                multiple = FALSE,
                options = list(
                  `actions-box` = TRUE,
                  liveSearch = TRUE,
                  title = "Nothing selected"
                ),
                width = "100%"
              )
            ),

            # Variable selection
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
          ),

          # Bar chart
          ggiraph::girafeOutput(ns("prey_presence_plot"))
        )
      ),
      prey_composition_box = list(
        title = "Composition of carnivore diet",
        outputId = ns("prey_composition_plot"),
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

            # Host species filter
            div(
              style = "flex: 1;",
              shinyWidgets::pickerInput(
                inputId = ns("filter_species"),
                label = "Filter by Host Species",
                choices = NULL,
                multiple = FALSE,
                options = list(
                  `actions-box` = TRUE,
                  liveSearch = TRUE
                ),
                width = "100%"
              )
            )
          ),

          # Bar chart
          ggiraph::girafeOutput(ns("prey_composition_plot"))
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

    # Observe boxes for updating picker input choices
    observeEvent(rv$active_box, {

      # Delay slightly to let the UI render first
      shinyjs::delay(100, {
        shinyWidgets::updatePickerInput(
          session, "filter_park",
          choices = sort(unique(rv$meta$name_of_park))
        )

        shinyWidgets::updatePickerInput(
          session, "filter_species",
          choices = c("",
                      rv$vert |>
                        dplyr::filter(DNA_Diet_Host %in% c("Carnivore", "Omnivore")) |>
                        dplyr::distinct(DNA_Common_Host) |>
                        dplyr::arrange(DNA_Common_Host) |>
                        dplyr::pull(DNA_Common_Host)
          )
        )
      })
    })

    # Render prey presence plot
    output$prey_presence_plot <- ggiraph::renderGirafe({
      req(rv$vert)

      df <- rv$vert |>
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
        dplyr::mutate(n = 1)

      levels_cat <- levels(df$Category2)
      color_palette <- c("#FF9999", "#A8D5A1", "#99CCFF", "#fbcfaf", "#b52727", "#28a745", "#002790", "#ec7627") # "#f5c8e0", "#901e7c"
      named_colors <- setNames(color_palette[1:length(levels_cat)], levels_cat)

      if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
        df <- df[df$name_of_park %in% input$filter_park, ]
      }

      if (input$filter_species != "") {
        df <- df[df$DNA_Common_Host %in% input$filter_species, ]
      }

      if (input$select_variable != "") {
        df <- df |>
          dplyr::count(Barcode, name_of_park, DNA_Common_Host, Category2, Category, !!rlang::sym(input$select_variable))
      }

      # Use select_variable if filter_species is set
      x_var <- if (input$filter_species != "" && input$select_variable != "") {
        rlang::sym(input$select_variable)
      } else {
        rlang::sym("DNA_Common_Host")
      }

      p <- ggplot2::ggplot(df, ggplot2::aes(x = !!x_var, y = n, fill = Category2)) +
        ggiraph::geom_bar_interactive(
          ggplot2::aes(
            tooltip = paste0("Sample ID: ", Barcode, "\nPark: ", name_of_park, "\nStatus: ", Category),
            group = Category2
          ),
          stat = "identity"
        ) +
        ggplot2::scale_fill_manual(values = named_colors, guide = "none") +
        # ggplot2::scale_fill_manual(values = c("#FF0000", "#00C800", "#0066FF", "#FFA500", "#FF9999", "#99FF99", "#99CCFF", "#FFD580")) +
        # ggplot2::scale_fill_manual(values = c("gray70", "#28a745"), guide = "none") +
        ggplot2::labs(y = "\n\n\nSamples") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
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
    output$prey_composition_plot <- ggiraph::renderGirafe({
      req(rv$vert)

      df <- rv$vert |>
        dplyr::filter(DNA_Diet_Host %in% c("Carnivore", "Omnivore"),
                      !is.na(DNA_Species_Prey))

      if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
        df <- df[df$name_of_park %in% input$filter_park, ]
      }

      if (input$filter_species != "") {
        df <- df[df$DNA_Common_Host %in% input$filter_species, ]
      } else {
        df <- NULL
      }

      req(nrow(df) > 0)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = Barcode, y = RRA_Prey, fill = DNA_Common_Prey)) +
        ggplot2::facet_grid(~ name_of_park, scales = "free_x", space = "free_x") +
        ggiraph::geom_bar_interactive(
          ggplot2::aes(
            tooltip = paste0("Sample ID: ", Barcode, "\nPark: ", name_of_park, "\nPrey: ", DNA_Common_Prey, "\nRRA: ", RRA_Prey),
            group = DNA_Common_Prey
          ),
          stat = "identity"
        ) +
        ggplot2::labs(y = "Relative Read Abundance") +
        ggplot2::guides(fill = "none") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          # axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
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
