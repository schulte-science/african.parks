#' plants UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_plants_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    bs4Dash::tabItem(
      tabName = "plants",
      fluidRow(
        bs4Dash::valueBoxOutput(ns("diet_ordination")),
        # bs4Dash::valueBoxOutput(ns("diet_ternary"))
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

    # Get filtered data
    trnl_filtered <- reactive({
      df <- rv$trnl

      # Apply filters only if something is selected
      if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
        df <- df[df$name_of_park %in% input$filter_park, ]
      }

      if (!is.null(input$filter_species) && length(input$filter_species) > 0) {
        df <- df[df$DNA_Common_Host %in% input$filter_species, ]
      }

      df
    })

    # Track which boxes are open (dynamically)
    show_boxes <- reactiveValues()

    # Render the Ordination ValueBox
    output$diet_ordination <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$b("Plant composition"),
        subtitle = "Composition of herbivore diet",
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
                inputId = ns("filter_park"),
                label = "Filter by Park",
                choices = NULL,
                selected = NULL,
                multiple = FALSE,
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
                inputId = ns("filter_species"),
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
            # div(
            #   style = "flex: 1;",
            #   shinyWidgets::pickerInput(
            #     inputId = ns("filter_family"),
            #     label = "Filter by Plant Family",
            #     choices = NULL,
            #     selected = NULL,
            #     multiple = TRUE,
            #     options = list(
            #       `actions-box` = TRUE,
            #       liveSearch = TRUE,
            #       title = "Nothing selected"
            #     ),
            #     width = "100%"
            #   )
            # ),

            # Run button
            div(
              style = "flex: 0 0 100px;",
              tags$div(
                class = "form-group shiny-input-container",
                tags$label(" "),  # blank label with correct Bootstrap spacing
                actionButton(
                  inputId = ns("button_run_ordination"),
                  label = "Run",
                  class = "btn btn-primary",
                  style = "width: 100%; height: 38px;"
                )
              )
            )
          ),

          # Ordination plot
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput(ns("diet_ordination_plot")),
          color = "#8175AABF", type = 8),

          # PERMANOVA result text
          # div(
          #   style = "margin-top: 10px;",
          #   verbatimTextOutput(ns("diet_ordination_permanova"))
          # ),

          # Pairwise PERMANOVA result table
          uiOutput(ns("diet_ordination_pairwise_ui"))
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
                multiple = FALSE,
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

    # Observe box for updating picker input choices
    observeEvent(rv$active_box, {
      shinyjs::delay(100, {
        req(rv$trnl)

        shinyWidgets::updatePickerInput(
          session, "filter_park",
          choices = sort(unique(rv$trnl$name_of_park))
        )

        shinyWidgets::updatePickerInput(
          session, "filter_species",
          choices = sort(unique(rv$trnl |> dplyr::pull(DNA_Common_Host)))
        )
      })
    })

    # Update park choices
    observe({
      req(rv$trnl)

      df <- rv$trnl
      if (!is.null(input$filter_species) && length(input$filter_species) > 0) {
        df <- df[df$DNA_Common_Host %in% input$filter_species, ]
      }

      updatePickerInput(session, "filter_park",
                        choices = sort(unique(df$name_of_park)),
                        selected = input$filter_park[input$filter_park %in% df$name_of_park]
                        )
    })

    # Update species choices
    observe({
      req(rv$trnl)

      df <- rv$trnl

      if (!is.null(input$filter_park) && length(input$filter_park) > 0) {
        df <- df[df$name_of_park %in% input$filter_park, ]
      }

      df <- df |>
        dplyr::distinct(Barcode, DNA_Common_Host) |>
        dplyr::add_count(DNA_Common_Host) |>
        dplyr::filter(n > 2) |>
        dplyr::distinct(DNA_Common_Host) |>
        dplyr::arrange(DNA_Common_Host)

      updatePickerInput(session, "filter_species",
                        choices = sort(unique(df$DNA_Common_Host)),
                        selected = input$filter_species[input$filter_species %in% df$DNA_Common_Host]
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

    # Get pairwise PERMANOVA function
    pairwise_permanova <- function(mat, group_vector, method = "bray", permutations = 999) {
      group_vector <- as.factor(group_vector)
      lvls <- levels(group_vector)
      combs <- combn(lvls, 2, simplify = FALSE)

      results <- lapply(combs, function(pair) {
        inds <- which(group_vector %in% pair)
        sub_mat <- mat[inds, ]
        sub_group <- droplevels(group_vector[inds])

        ad <- vegan::adonis2(sub_mat ~ sub_group, permutations = permutations, method = method)

        data.frame(
          Group1 = pair[1],
          Group2 = pair[2],
          Df = ad[1, "Df"],
          R2 = round(ad[1, "R2"], 3),
          F = round(ad[1, "F"], 3),
          p = round(ad[1, "Pr(>F)"], 3),
          stringsAsFactors = FALSE
        )
      })

      results_df <- do.call(rbind, results)
      results_df$p_Bonferroni <- round(p.adjust(results_df$p, method = "bonferroni"), 3)
      results_df$p_Holm <- round(p.adjust(results_df$p, method = "holm"), 3)
      results_df$p_BH <- round(p.adjust(results_df$p, method = "BH"), 3)

      results_df
    }

    # permanova_text <- reactiveVal(NULL)
    permanova_table <- reactiveVal(NULL)

    observeEvent(input$button_run_ordination, {
      permanova_table(NULL)
    })

    diet_ordination_plot_obj <- eventReactive(input$button_run_ordination, {
      req(rv$trnl)
      req(!is.null(input$filter_park) && length(input$filter_park) > 0)

      # Create tmp_wide
      if (!is.null(input$filter_species)) {
        tmp_wide <- trnl_filtered() |>
          dplyr::filter(name_of_park %in% input$filter_park,
                        DNA_Common_Host %in% input$filter_species) |>
          tidyr::pivot_wider(names_from = ESVId, id_cols = Barcode, values_from = Reads) |>
          tibble::column_to_rownames(var = "Barcode") |>
          dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(., 0))) |>
          (\(x) x[rowSums(x) > 0, colSums(x) > 0])()
      } else {
        tmp_wide <- trnl_filtered() |>
          dplyr::filter(name_of_park %in% input$filter_park) |>
          tidyr::pivot_wider(names_from = ESVId, id_cols = Barcode, values_from = Reads) |>
          tibble::column_to_rownames(var = "Barcode") |>
          dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(., 0))) |>
          (\(x) x[rowSums(x) > 0, colSums(x) > 0])()
      }

      if (nrow(tmp_wide) <= 2) {
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
        dplyr::left_join(rv$trnl[,c("Barcode", "DNA_Common_Host", "name_of_park")] |> dplyr::distinct(Barcode, .keep_all = TRUE), by = "Barcode")

      xrange <- range(tmp_sites$NMDS1, na.rm = TRUE)
      yrange <- range(tmp_sites$NMDS2, na.rm = TRUE)

      tmp_spp <- vegan::scores(tmp_nmds, "species") |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "ESVId") |>
        dplyr::filter(dplyr::between(NMDS1, xrange[1], xrange[2]),
                      dplyr::between(NMDS2, yrange[1], yrange[2])) |>
        dplyr::left_join((rv$trnl[,c("ESVId", "Class", "Order", "Family", "Genus", "Species")] |> dplyr::distinct(ESVId, .keep_all = T)), by = "ESVId") |>
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

      # Run PERMANOVA
      if(length(unique(tmp_sites$DNA_Common_Host)) > 1) {
        permanova <- vegan::adonis2(tmp_wide ~ rv$trnl$DNA_Common_Host[match(rownames(tmp_wide), rv$trnl$Barcode)],
                                           permutations = 99, method = "bray")
        perm_stats <- permanova[1, ]
        # perm_summary <- sprintf(
        #   "PERMANOVA results (99 permutations):\nDF = %d\nSST = %.3f\nR² = %.3f\nF = %.3f\np = %.3f",
        #   perm_stats$Df, perm_stats$SumOfSqs, perm_stats$R2, perm_stats$F, perm_stats$`Pr(>F)`
        # )
        # permanova_text(perm_summary)

        # Run pairwise PERMANOVA
        group_vector <- rv$trnl$DNA_Common_Host[match(rownames(tmp_wide), rv$trnl$Barcode)]
        pairwise_df <- rbind(
          data.frame(Group1 = "All", Group2 = "All", Df = perm_stats$Df, R2 = round(perm_stats$R2, 3), `F` = round(perm_stats$`F`, 3), p = round(perm_stats$`Pr(>F)`, 3), p_Bonferroni = "", p_Holm = "", p_BH = ""),
          pairwise_permanova(tmp_wide, group_vector) |>
            dplyr::filter(!is.na(p)) |>
            dplyr::arrange(-p) |>
            dplyr::select(Group1, Group2, Df, R2, `F`, p, p_Bonferroni, p_Holm, p_BH)
        )
        permanova_table(pairwise_df)
      }

      # Plot
      tmp_p <- ggplot2::ggplot(tmp_sites, ggplot2::aes(x = NMDS1, y = NMDS2, fill = DNA_Common_Host)) +
        ggiraph::geom_point_interactive(
          aes(tooltip = paste0("Sample ID: ", Barcode, "\nPark: ", name_of_park, "\nConsumer: ", DNA_Common_Host)),
          color = "black", size = 3, shape = 21, alpha = 0.5
        ) +
        ggrepel::geom_text_repel(data = tmp_spp,
                                 ggplot2::aes(x = NMDS1, y = NMDS2, label = Consensus),
                                 color = "black", size = 3,
                                 inherit.aes = FALSE) +
        ggplot2::labs(fill = "DNA-Inferred Host") +
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

    # output$diet_ordination_permanova <- renderPrint({
    #   req(permanova_text())
    #   cat(permanova_text(), sep = "\n")
    # })

    output$diet_ordination_pairwise <- DT::renderDT({
      req(permanova_table())

      DT::datatable(
        permanova_table(),
        options = list(
          scrollX = TRUE,
          scrollY = "400px",
          paging = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip'
        ),
        rownames = FALSE,
        editable = FALSE,
        extensions = c("Buttons"),
        selection = "none"
      )

    })

    output$diet_ordination_pairwise_ui <- renderUI({
      req(permanova_table())
      df <- permanova_table()
      if (nrow(df) == 0) return(NULL)

      tagList(
        tags$h6("PERMANOVA by host (99 permutations):"),
        DT::dataTableOutput(ns("diet_ordination_pairwise"))
      )
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
