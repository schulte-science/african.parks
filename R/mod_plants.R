#' plants UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny bs4Dash
mod_plants_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    bs4Dash::tabItem(
      tabName = "plants",
      fluidRow(
        bs4Dash::valueBoxOutput(ns("diet_ordination")),
        bs4Dash::valueBoxOutput(ns("diet_ternary"))
      ),

      div(
        id = ns("dynamic_boxes"),
        style = "display: flex; flex-wrap: wrap; gap: 10px; justify-content: flex-start;"
      )
    )
  )
}

#' plants Server Functions
#'
#' @importFrom shiny NS tagList
#' @noRd
mod_plants_server <- function(id, rv, parentSession) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # Track which boxes are open (dynamically)
    show_boxes <- reactiveValues()

    # Render the Ordination ValueBox
    output$diet_ordination <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Ordination"),
        subtitle = "",
        icon = icon("circle"),
        footer = dash_open(target = "diet_ordination_box", session = session)
      )
    })

    # Render the Ternary ValueBox
    output$diet_ternary <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Ternary plot"),
        subtitle = "",
        icon = icon("caret-up"),
        footer = dash_open(target = "diet_ternary_box", session = session)
      )
    })

    # List of ValueBoxes and corresponding Boxes
    boxes <- list(
      diet_ordination_box = list(
        title = "Ordination of plant diet",
        outputId = ns("diet_ordination_plot"),
        content = tagList(
          div(
            style = "margin-bottom: 10px; display: flex; flex-wrap: nowrap; gap: 15px; justify-content: space-between;",

            # Park filter
            div(
              style = "flex: 1;",
              shinyWidgets::pickerInput(
                inputId = ns("filter_park_ordination"),
                label = "Filter by Park",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  liveSearch = TRUE,
                  selectedTextFormat = "count > 1",
                  countSelectedText = "{0} selected",
                  title = "Nothing selected (required)"
                ),
                width = "100%"
              )
            ),

            # Host species filter
            div(
              style = "flex: 1;",
              shinyWidgets::pickerInput(
                inputId = ns("filter_species_ordination"),
                label = "Filter by Host Species",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  liveSearch = TRUE,
                  title = "Nothing selected"
                ),
                width = "100%"
              )
            ),

            # Plant family filter
            div(
              style = "flex: 1;",
              shinyWidgets::pickerInput(
                inputId = ns("filter_family"),
                label = "Filter by Plant Family",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  liveSearch = TRUE,
                  title = "Nothing selected"
                ),
                width = "100%"
              )
            ),

            # Run button
            actionButton(ns("button_run_ordination"), "Run")
          ),

          # Ordination plot
          ggiraph::girafeOutput(ns("diet_ordination_plot"))
        )
      ),
      diet_ternary_box = list(
        title = "Ternary plot of plant diet",
        outputId = ns("diet_ternary_plot"),
        content = tagList(
          div(
            style = "margin-bottom: 10px; display: flex; flex-wrap: nowrap; gap: 15px; justify-content: space-between;",

            # Park filter
            div(
              style = "flex: 1;",
              shinyWidgets::pickerInput(
                inputId = ns("filter_park_ternary"),
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
                inputId = ns("filter_species_ternary"),
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

          # Ternary plot
          ggiraph::girafeOutput(ns("diet_ternary_plot"))
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
      shinyjs::delay(100, {
        shinyWidgets::updatePickerInput(
          session, "filter_park_ordination",
          choices = sort(unique(rv$meta$name_of_park))
        )
      })
    })

    observeEvent(input$filter_park_ordination, {
      req(input$filter_park_ordination)

      shinyWidgets::updatePickerInput(
        session, "filter_species_ordination",
        choices = c("",
                    rv$trnl |>
                      dplyr::filter(name_of_park %in% input$filter_park_ordination) |>
                      dplyr::distinct(Barcode, DNA_Common_Host) |>
                      dplyr::add_count(DNA_Common_Host) |>
                      dplyr::filter(n > 1) |>
                      dplyr::distinct(DNA_Common_Host) |>
                      dplyr::arrange(DNA_Common_Host) |>
                      dplyr::pull(DNA_Common_Host)
        )
      )
    })

    # Get function to flag NMDS outliers
    flag_outliers <- function(x) {
      qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
      iqr <- IQR(x, na.rm = TRUE)
      outlier_low <- x < (qnt[1] - 1.5 * iqr)
      outlier_high <- x > (qnt[2] + 1.5 * iqr)
      outlier_low | outlier_high
    }

    diet_ordination_plot_obj <- eventReactive(input$button_run_ordination, {
      req(rv$trnl)
      req(!is.null(input$filter_park_ordination) && length(input$filter_park_ordination) > 0)

      # Create tmp_wide
      if (!input$filter_species_ordination %in% "" | !is.null(input$filter_species_ordination)) {
        tmp_wide <- rv$trnl |>
          dplyr::filter(name_of_park %in% input$filter_park_ordination,
                        DNA_Common_Host %in% input$filter_species_ordination) |>
          tidyr::pivot_wider(names_from = ESVId, id_cols = Barcode, values_from = Reads) |>
          tibble::column_to_rownames(var = "Barcode") |>
          dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(., 0))) |>
          (\(x) x[rowSums(x) > 0, colSums(x) > 0])()
      } else {
        tmp_wide <- rv$trnl |>
          dplyr::filter(name_of_park %in% input$filter_park_ordination) |>
          tidyr::pivot_wider(names_from = ESVId, id_cols = Barcode, values_from = Reads) |>
          tibble::column_to_rownames(var = "Barcode") |>
          dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(., 0))) |>
          (\(x) x[rowSums(x) > 0, colSums(x) > 0])()
      }

      if (nrow(tmp_wide) <= 1) {
        return(
          ggiraph::girafe(
            ggobj = ggplot2::ggplot() +
              ggplot2::theme_void() +
              ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Filters yield too few observations for plotting", size = 6, hjust = 0.5) +
              ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1),
            options = list(ggiraph::opts_toolbar(saveaspng = TRUE))
          )
        )
      } else {

      # NMDS ordination
      set.seed(123)
      tmp_nmds <- vegan::metaMDS(tmp_wide, trymax = 1)

      # Site scores
      tmp_sites <- vegan::scores(tmp_nmds)$sites |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "Barcode") |>
        dplyr::mutate(outlier_NMDS1 = flag_outliers(NMDS1),
                      outlier_NMDS2 = flag_outliers(NMDS2)) |>
        dplyr::filter(!(outlier_NMDS1 | outlier_NMDS2)) |>
        dplyr::left_join(rv$meta[,c("barcode_id", "species_id_dna_common")], by = c("Barcode" = "barcode_id"))

      xrange <- range(tmp_sites$NMDS1, na.rm = TRUE)
      yrange <- range(tmp_sites$NMDS2, na.rm = TRUE)

      tmp_spp <- vegan::scores(tmp_nmds, "species") |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "ESVId") |>
        dplyr::filter(dplyr::between(NMDS1, xrange[1], xrange[2]),
                      dplyr::between(NMDS2, yrange[1], yrange[2])) |>
        dplyr::left_join(rv$trnl[,c("ESVId", "Class", "Order", "Family", "Genus", "Species")] |> dplyr::distinct(ESVId, .keep_all = T)) |>
        dplyr::mutate(Species = ifelse(grepl("\\.|[0-9]", Species), NA, Species)) |>
        dplyr::mutate(Consensus = paste(Class, Order, Family, Genus, Species, sep = ";"),
                      Consensus = gsub("\\;NA.*|\\;\\;.*", "", Consensus),
                      Consensus = stringr::str_remove(Consensus, ";$"),
                      Consensus = sub(".*;(.*)$", "\\1", Consensus),
                      Consensus = ifelse(Consensus %in% c("NA", ""), NA, Consensus)) |>
        dplyr::mutate(
          quadrant = dplyr::case_when(
            NMDS1 >= 0 & NMDS2 >= 0 ~ "Q1",
            NMDS1 <  0 & NMDS2 >= 0 ~ "Q2",
            NMDS1 <  0 & NMDS2 <  0 ~ "Q3",
            NMDS1 >= 0 & NMDS2 <  0 ~ "Q4"
          ),
          dist = sqrt(NMDS1^2 + NMDS2^2)
        ) |>
        dplyr::group_by(quadrant) |>
        dplyr::slice_max(order_by = dist, n = 5, with_ties = FALSE)

      # Plot
      tmp_p <- ggplot2::ggplot(tmp_sites, ggplot2::aes(x = NMDS1, y = NMDS2, fill = species_id_dna_common)) +
        ggplot2::geom_point(color = "black", size = 3, shape = 21, alpha = 0.7) +
        ggrepel::geom_text_repel(data = tmp_spp,
                                 ggplot2::aes(x = NMDS1, y = NMDS2, label = Consensus),
                                 color = "black", size = 3,
                                 inherit.aes = FALSE,
                                 point.color = "black", segment.color = "black") +
        ggplot2::labs(fill = "Host (from DNA)") +
        ggplot2::theme_bw()

      ggiraph::girafe(
        ggobj = tmp_p,
        options = list(
          ggiraph::opts_toolbar(saveaspng = TRUE)
        )
      )
      }
    })

    output$diet_ordination_plot <- ggiraph::renderGirafe({
      req(diet_ordination_plot_obj())
    })


  })

}
#
# dash_open <- function(target, session = shiny::getDefaultReactiveDomain()) {
#   ns <- session$ns
#   div(
#     style = "display: flex; align-items: center; justify-content: center; width: 100%; height: 20px; cursor: pointer;",
#     onclick = glue::glue("Shiny.setInputValue('{ns('toggle_box')}', '{target}', {{priority: 'event'}});"),
#     div(
#       style = "display: flex; flex-flow: row nowrap; align-items: center; justify-content: center; gap: 1em",
#       "Details",
#       tags$i(class = "fas fa-circle-arrow-right")
#     )
#   )
# }
