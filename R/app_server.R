#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Initialize reactive values ----
  rv <- reactiveValues(
    runs = read.csv("JordanaSamples - Overview.csv", na = "") |>
      dplyr::mutate(DateRun = as.Date(DateRun, format = "%m/%d/%Y")) |>
      dplyr::filter(!Amplicon %in% "16S"),

    meta = read.csv("JordanaSamples - Metadata_20250225.csv", na = "") |>
      dplyr::mutate(latitude = ifelse(latitude < -1000405, NA, latitude)),

    vert = read.csv("JordanaSamples - 12S_Summary_20250225.csv", na = ""),

    focus = NULL

  )

  # Modules ----
  mod_dashboard_server("dashboard", rv, session)
  mod_metadata_server("metadata", rv, session)
  # mod_vertebrates_server("vertebrates")
  # mod_plants_server("plants")
  # mod_footer_server("footer")
}
