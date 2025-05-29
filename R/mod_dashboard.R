#' dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny bs4Dash shinyWidgets ggplot2
mod_dashboard_ui <- function(id) {
  ns <- NS(id)

  bs4Dash::tabItem(
    tabName = "dashboard",
    fluidRow(
      bs4Dash::valueBoxOutput(ns("runs")),
      bs4Dash::valueBoxOutput(ns("metadata")),
      bs4Dash::valueBoxOutput(ns("vertebrates"))
    ),

    div(
      id = ns("dynamic_boxes"),
      style = "display: flex; flex-wrap: wrap; gap: 10px; justify-content: flex-start;"
    )
  )
}

#' dashboard Server Functions
#'
#' @importFrom shiny NS tagList
#' @noRd
mod_dashboard_server <- function(id, rv, parentSession) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # Track which boxes are open (dynamically)
    show_boxes <- reactiveValues()

    # Render the Runs ValueBox
    output$runs <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Samples Over Time"),
        subtitle = renderText(paste0(
            length(unique(rv$runs$JVRunId)),
            " sequencing runs"
            )),
        icon = icon("clock", class = "fas", verify_fa = FALSE),
        footer = dash_open(target = "runs_box", session = session)
      )
    })

    # Render the Metadata ValueBox dynamically
    output$metadata <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Samples By Park"),
        subtitle = renderText(paste0(
          rv$meta |>
            dplyr::filter(!is.na(RunId_12SVert)) |>
            nrow(),
          " total samples"
        )),
        icon = icon("location-dot"),
        footer = dash_open(target = "metadata_box", session = session)
      )
    })

    # Render the Vertebrates ValueBox dynamically
    output$vertebrates <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Samples By Species"),
        subtitle = renderText(paste0(
          length(unique(rv$meta$species_id_dna)),
          " host species"
        )),
        icon = icon("cat"),
        footer = dash_open(target = "vertebrates_box", session = session)
      )
    })

    # List of ValueBoxes and corresponding Boxes
    boxes <- list(
      runs_box = list(
        title = "Number of Samples Processed by Date",
        outputId = ns("runs_plot"),
        content = div(
          style = "position: relative;",
          div(
            style = "position: absolute; top: -59px; right: 10px; z-index: 10;",
            downloadButton(
              outputId = ns("runs_download"),
              label = "Download Data",
              class = "btn btn-sm btn-outline-secondary"
            )
          ),
          ggiraph::girafeOutput(ns("runs_plot"))
        )
        # content = ggiraph::girafeOutput(ns("runs_plot"))
      ),
      metadata_box = list(
        title = "Number of Samples Processed by Park",
        outputId = ns("metadata_plot"),
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
          ggiraph::girafeOutput(ns("metadata_plot"))
        )
        # content = ggiraph::girafeOutput(ns("metadata_plot"))
      ),
      vertebrates_box = list(
        title = "Number of Samples Processed by Host Species",
        outputId = ns("vertebrates_plot"),
        content = div(
          style = "position: relative;",  # enables absolute positioning inside
          # Floating button
          div(
            style = "position: absolute; top: -59px; right: 10px; z-index: 10;",
            downloadButton(
              outputId = ns("vertebrates_download"),
              label = "Download Data",
              class = "btn btn-sm btn-outline-secondary"
            )
          ),
          # Toggle and plot
          div(style = "margin-bottom: 10px; display: flex;",
              span("Count", style = "vertical-align: middle; font-size: 14px; margin-right: 10px;"),
              div(style = "display: flex; align-items: center;",
                  materialSwitch(inputId = ns("order_toggle"),
                                 label = NULL,
                                 value = FALSE,
                                 status = "primary",
                                 inline = TRUE)
              ),
              span("Alphabetical", style = "vertical-align: middle; font-size: 14px;")
          ),
          ggiraph::girafeOutput(ns("vertebrates_plot"))
        )
      )
    )

    # Generalized Click Event Handling
    # observeEvent(input$toggle_box, {
    #   box_id <- input$toggle_box  # Get clicked box ID
    #
    #   if (!is.null(boxes[[box_id]])) {
    #     if (is.null(show_boxes[[box_id]]) || !show_boxes[[box_id]]) {
    #       # Insert the box dynamically inside the flex container
    #       insertUI(
    #         selector = paste0("#", ns("dynamic_boxes")),
    #         where = "beforeEnd",
    #         ui = div(
    #           id = ns(box_id),
    #           style = "flex: 1 1 40%; max-width: 40%; min-width: 400px; box-sizing: border-box;",  # Each box takes 50% width
    #           bs4Dash::box(
    #             title = boxes[[box_id]]$title,
    #             solidHeader = FALSE,
    #             status = "success",
    #             collapsible = FALSE,
    #             width = NULL,  # Let flexbox control width
    #             boxes[[box_id]]$content
    #           )
    #         )
    #       )
    #       show_boxes[[box_id]] <- TRUE
    #     } else {
    #       # Remove the Box dynamically
    #       removeUI(selector = paste0("#", ns(box_id)))
    #       show_boxes[[box_id]] <- FALSE
    #     }
    #   }
    # })

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

    # Get runs plot data
    runs_plot_data <- reactive({
      req(rv$runs)

      rv$runs |>
        dplyr::group_by(Amplicon, DateRun) |>
        dplyr::summarize(Samples = sum(Samples)) |>
        dplyr::ungroup() |>
        dplyr::group_by(Amplicon) |>
        dplyr::mutate(Samples = cumsum(Samples))
    })

    # Render runs plot
    output$runs_plot <- ggiraph::renderGirafe({
      req(runs_plot_data())

      p <- ggplot2::ggplot(runs_plot_data(), ggplot2::aes(x = DateRun, y = Samples, color = Amplicon, group = Amplicon)) +
        ggplot2::geom_line() +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = paste0("Date: ", DateRun, "\nAmplicon: ", Amplicon, "\nSamples: ", Samples),
                       data_id = as.factor(Amplicon))
        ) +
        ggplot2::scale_color_manual(values = c("gray50", "#28a745")) +
        ggplot2::scale_x_date(date_labels = "%b %d", date_breaks = "1 month", expand = ggplot2::expansion(mult = 0.1)) +
        ggplot2::scale_y_continuous(n.breaks = 10) +
        ggplot2::labs(x = "Run date", y = "Samples (cumulative)") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
          strip.background = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(0, "lines"),
          legend.position = c(0.02, 0.98),
          legend.justification = c(0, 1),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_blank()
        )

      ggiraph::girafe(ggobj = p,
                      options = list(
                        ggiraph::opts_hover(css = "fill:red;"),  # Change bar color on hover
                        ggiraph::opts_toolbar(saveaspng = TRUE)  # Enable toolbar with save button
                      ))
    })

    # Get metadata plot data
    metadata_plot_data <- reactive({
      req(rv$meta)

      rv$meta |>
        dplyr::mutate(name_of_park = stringr::str_to_title(ifelse(name_of_park %in% "odzala_okoua", "odzala", name_of_park))) |>
        dplyr::group_by(name_of_park) |>
        dplyr::summarize(
          `12SVert 3` = sum(Reads_12SVert > 0, na.rm = TRUE),
          `12SVert 2` = sum(Reads_12SVert == 0, na.rm = TRUE),
          `12SVert 1` = sum(is.na(RunId_12SVert), na.rm = TRUE),
          `trnL 3` = sum(Reads_trnL > 0, na.rm = TRUE),
          `trnL 2` = sum(Reads_trnL == 0, na.rm = TRUE),
          `trnL 1` = sum(is.na(RunId_trnL), na.rm = TRUE)
        ) |>
        tidyr::pivot_longer(
          cols = -name_of_park,
          names_to = c("Assay", "Run"),
          names_sep = " "
        ) |>
        dplyr::mutate(Run = dplyr::case_when(
          Run == 1 ~ "Not run",
          Run == 2 ~ "Zero reads",
          Run == 3 ~ "Completed"
        )) |>
        dplyr::mutate(Run = factor(Run, levels = c("Not run", "Zero reads", "Completed")),
                      color = dplyr::case_when(
                        name_of_park %in% "Akagera" & Run %in% "Completed" ~ "#901e7c",
                        name_of_park %in% "Akagera" & Run %in% "Zero reads" ~ "#f5c8e0",
                        name_of_park %in% "Iona" & Run %in% "Completed" ~ "#b52727",
                        name_of_park %in% "Iona" & Run %in% "Zero reads" ~ "#FF9999",
                        name_of_park %in% "Kafue" & Run %in% "Completed" ~ "#28a745",
                        name_of_park %in% "Kafue" & Run %in% "Zero reads" ~ "#A8D5A1",
                        name_of_park %in% "Odzala" & Run %in% "Completed" ~ "#002790",
                        name_of_park %in% "Odzala" & Run %in% "Zero reads" ~ "#99CCFF",
                        name_of_park %in% "Zakouma" & Run %in% "Completed" ~ "#ec7627",
                        name_of_park %in% "Zakouma" & Run %in% "Zero reads" ~ "#fbcfaf",
                        TRUE ~ "gray90"
                      ),
                      color = factor(color, levels = sort(unique(color), decreasing = TRUE)))
    })

    # Render metadata plot
    output$metadata_plot <- ggiraph::renderGirafe({
      req(metadata_plot_data())

      p <- ggplot2::ggplot(metadata_plot_data(),
                           ggplot2::aes(x = Assay, y = value, fill = color, tooltip = paste0("Status: ", Run, "\nCount: ", value))) +
        ggiraph::geom_col_interactive(position = "stack") +
        ggplot2::facet_wrap(~name_of_park, nrow = 1) +
        ggplot2::labs(y = "Samples") +
        ggplot2::scale_fill_identity() +
        ggplot2::scale_y_continuous(n.breaks = 8, expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.background = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(0, "lines")
        )

      ggiraph::girafe(ggobj = p,
                      options = list(
                        ggiraph::opts_hover(css = "fill:red;"),  # Change bar color on hover
                        ggiraph::opts_toolbar(saveaspng = TRUE)  # Enable toolbar with save button
                      ))
    })

    # Get vertebrate plot data ----
    vertebrates_plot_data <- reactive({
      req(rv$meta)

      data <- rv$meta |>
        dplyr::filter(!is.na(species_id_dna_common), !is.na(name_of_park)) |>
        dplyr::group_by(species_id_dna_common) |>
        dplyr::mutate(total_count = sum(dplyr::n())) |>  # Compute total count across parks
        dplyr::ungroup() |>
        dplyr::mutate(species_id_dna_common = ifelse(total_count < 5, "Other", species_id_dna_common)) |>
        dplyr::count(species_id_dna_common, name_of_park, name = "count") |>
        dplyr::ungroup()

      # Apply ordering based on the toggle selection
      if (is.null(input$order_toggle) || !input$order_toggle) {
        data <- data |>
          dplyr::mutate(species_id_dna_common = factor(species_id_dna_common,
                                                       levels = names(sort(tapply(count, species_id_dna_common, sum), decreasing = TRUE))))
      } else {
        data <- data |>
          dplyr::mutate(species_id_dna_common = factor(species_id_dna_common, levels = sort(unique(species_id_dna_common))))
      }

      return(data)
    })

    # Render vertebrate plot
    output$vertebrates_plot <- ggiraph::renderGirafe({
      req(vertebrates_plot_data())

      p <- ggplot2::ggplot(vertebrates_plot_data(),
                           ggplot2::aes(x = species_id_dna_common,
                                        y = count,
                                        # y = factor(species_id_dna_common, levels = rev(sort(unique(species_id_dna_common)))),
                                        fill = name_of_park, tooltip = paste0("Species: ", species_id_dna_common, "\nCount: ", count, "\nPark: ", name_of_park))) +
        ggiraph::geom_col_interactive(position = "stack") +
        ggplot2::scale_fill_manual(values = c("#b52727", "#28a745", "#002790", "#ec7627", "#901e7c"), guide = "none") +
        ggplot2::labs(y = "Samples", fill = "Park") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 5),
          strip.background = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(0, "lines")
        )

      ggiraph::girafe(ggobj = p,
                      options = list(
                        ggiraph::opts_hover(css = "fill:red;"),  # Change bar color on hover
                        ggiraph::opts_toolbar(saveaspng = TRUE)  # Enable toolbar with save button
                      ))
    })

    # Download buttons
    output$runs_download <- downloadHandler(
      filename = function() {
        paste0("AfricanParks_SamplesOverTime_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(runs_plot_data())
        write.csv(runs_plot_data(), file, na = "", row.names = FALSE)
      }
    )

    output$metadata_download <- downloadHandler(
      filename = function() {
        paste0("AfricanParks_SamplesByPark_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(metadata_plot_data())
        write.csv(metadata_plot_data() |> dplyr::select(-color) |> dplyr::rename(Park = name_of_park, Samples = value), file, na = "", row.names = FALSE)
      }
    )

    output$vertebrates_download <- downloadHandler(
      filename = function() {
        paste0("AfricanParks_SamplesBySpecies_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(vertebrates_plot_data())
        write.csv(vertebrates_plot_data() |> dplyr::rename(Species = species_id_dna_common, Park = name_of_park, Samples = count), file, na = "", row.names = FALSE)
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
