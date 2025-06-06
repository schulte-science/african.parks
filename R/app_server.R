#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Initialize reactive values ----
  rv <- reactiveValues(
    runs = read.csv(app_sys("Overview_20250519.csv"), na.strings = c("", "NA")) |>
      dplyr::mutate(DateRun = as.Date(DateRun)),

    meta = read.csv(app_sys("Metadata_20250519.csv"), na.strings = c("", "NA")) |>
      dplyr::mutate(latitude = ifelse(latitude < -1000405, NA, latitude)),

    vert = read.csv(app_sys("12S_Summary_20250519.csv"), na.strings = c("", "NA")),

    trnl = read.csv(app_sys("trnL_Summary_20250519.csv"), na.strings = c("", "NA")) |>
      dplyr::filter(DNA_Diet_Host %in% "Herbivore"),

    focus = NULL
  )

  # Modules ----
  mod_dashboard_server("dashboard", rv, session)
  mod_metadata_server("metadata", rv, session)
  mod_vertebrates_server("vertebrates", rv, session)
  mod_plants_server("plants", rv, session)
  # mod_footer_server("footer")
}
