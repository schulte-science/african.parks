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
    # Metadata table box
    bs4Dash::box(
      title = "Metadata Table",
      width = 12,
      solidHeader = FALSE,
      status = "success",
      collapsible = TRUE,
      div(
        style = "margin-bottom: 10px;"
      ),
      DT::dataTableOutput(ns("metadata_table"))
    )

    # Metadata plot box
    # bs4Dash::box(
    #   title = "Metadata Summary",
    #   width = 12,
    #   solidHeader = FALSE,
    #   status = "success",
    #   collapsible = TRUE,
    #   div(
    #     style = "margin-bottom: 10px;"
    #   ),
    #   ggiraph::girafeOutput(ns("metadata_plot"))
    # )
  )
}

#' metadata Server Functions
#'
#' @importFrom shiny NS tagList
#' @noRd
mod_metadata_server <- function(id, rv, parentSession) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

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

    # Get metadata plot data
    metadata_plot_data <- reactive({
      req(rv$meta)

      rv$meta |>
        dplyr::mutate(name_of_park = stringr::str_to_title(ifelse(name_of_park %in% "odzala_okoua", "odzala", name_of_park))) |>
        dplyr::group_by(name_of_park) |>
        dplyr::summarize(
          `12SVert Run` = sum(!is.na(RunId_12SVert), na.rm = TRUE),
          `12SVert Missing` = sum(is.na(RunId_12SVert), na.rm = TRUE),
          `trnL Run` = sum(!is.na(RunId_trnL), na.rm = TRUE),
          `trnL Missing` = sum(is.na(RunId_trnL), na.rm = TRUE)
        ) |>
        tidyr::pivot_longer(
          cols = -name_of_park,
          names_to = c("Assay", "Run"),
          names_sep = " "
        )
    })

    # Render stacked bar chart with facets
    output$metadata_plot <- ggiraph::renderGirafe({
      req(metadata_plot_data())

      p <- ggplot2::ggplot(metadata_plot_data(),
                           ggplot2::aes(x = Assay, y = value, fill = Run, tooltip = paste("Count:", value))) +
        ggiraph::geom_col_interactive(position = "stack") +
        ggplot2::facet_wrap(~name_of_park, nrow = 1) +
        ggplot2::scale_fill_manual(values = c("gray90", "#28a745"), guide = "none") +
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
