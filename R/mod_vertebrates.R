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

  bs4Dash::tabItem(
    tabName = "vertebrates",
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
    output$runs <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Runs"),
        subtitle = renderText(
            length(unique(rv$runs$JVRunId))
            ),
        icon = icon("hard-drive"),
        footer = dash_open(target = "runs_box", session = session)
      )
    })

    # Render the Metadata ValueBox dynamically
    output$metadata <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Samples Processed"),
        subtitle = renderText(
          rv$meta |>
            dplyr::filter(!is.na(RunId_12SVert)) |>
            nrow()
        ),
        icon = icon("clipboard"),
        footer = dash_open(target = "metadata_box", session = session)
      )
    })

    # Render the Vertebrates ValueBox dynamically
    output$vertebrates <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Host Species"),
        subtitle = renderText(
          length(unique(rv$meta$species_id_dna))
        ),
        icon = icon("cat"),
        footer = dash_open(target = "vertebrates_box", session = session)
      )
    })

    # List of ValueBoxes and corresponding Boxes
    boxes <- list(
      runs_box = list(
        title = "Number of Samples by Run",
        outputId = ns("runs_plot"),
        content = ggiraph::girafeOutput(ns("runs_plot"))
      ),
      metadata_box = list(
        title = "Number of Samples Processed by Park",
        outputId = ns("metadata_plot"),
        content = ggiraph::girafeOutput(ns("metadata_plot"))
      ),
      vertebrates_box = list(
        title = "Host Species",
        outputId = ns("vertebrates_plot"),
        content = plotOutput(ns("vertebrates_plot"))
      )
    )

    # Generalized Click Event Handling
    observeEvent(input$toggle_box, {
      box_id <- input$toggle_box  # Get clicked box ID

      if (!is.null(boxes[[box_id]])) {
        if (is.null(show_boxes[[box_id]]) || !show_boxes[[box_id]]) {
          # Insert the box dynamically inside the flex container
          insertUI(
            selector = paste0("#", ns("dynamic_boxes")),
            where = "beforeEnd",
            ui = div(
              id = ns(box_id),
              style = "flex: 1 1 40%; max-width: 40%; min-width: 400px; box-sizing: border-box;",  # Each box takes 50% width
              bs4Dash::box(
                title = boxes[[box_id]]$title,
                solidHeader = FALSE,
                status = "success",
                collapsible = FALSE,
                width = NULL,  # Let flexbox control width
                boxes[[box_id]]$content
              )
            )
          )
          show_boxes[[box_id]] <- TRUE
        } else {
          # Remove the Box dynamically
          removeUI(selector = paste0("#", ns(box_id)))
          show_boxes[[box_id]] <- FALSE
        }
      }
    })

    # Render runs plot
    output$runs_plot <- ggiraph::renderGirafe({
      req(rv$runs)

      print(rv$runs)
      p <- ggplot2::ggplot(rv$runs, ggplot2::aes(x = DateRun, y = Samples, fill = Amplicon)) +
        ggiraph::geom_bar_interactive(
          ggplot2::aes(tooltip = paste0("Run: ", JVRunId, "\nAmplicon: ", Amplicon, "\nSamples: ", Samples),
              data_id = as.factor(JVRunId)),
          stat = "identity",
          position = ggplot2::position_dodge2(preserve = "single", padding = 0),
          width = 20
        ) +
        ggplot2::scale_x_date(date_labels = "%b %d", date_breaks = "1 month", expand = ggplot2::expansion(mult = 0.1)) +
        ggplot2::scale_fill_manual(values = c("gray50", "#28a745")) +
        ggplot2::labs(x = "Run date", y = "Samples", fill = "Amplicon") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
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
        )
    })

    # Render metadata plot
    output$metadata_plot <- ggiraph::renderGirafe({
      req(metadata_plot_data())

      p <- ggplot2::ggplot(metadata_plot_data(),
                           ggplot2::aes(x = Assay, y = value, fill = Run, tooltip = paste("Count:", value))) +
        ggiraph::geom_col_interactive(position = "stack") +
        ggplot2::facet_wrap(~name_of_park, nrow = 1) +
        ggplot2::scale_fill_manual(values = c("gray90", "#A8D5A1", "#28a745"), guide = "none") +
        ggplot2::labs(y = "Number of Samples") +
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
