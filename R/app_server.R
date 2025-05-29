#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Initialize reactive values ----
  rv <- reactiveValues(
    runs = read.csv("Overview_20250519.csv", na.strings = c("", "NA")) |>
      dplyr::mutate(DateRun = as.Date(DateRun)),

    meta = read.csv("Metadata_20250519.csv", na.strings = c("", "NA")) |>
      dplyr::mutate(latitude = ifelse(latitude < -1000405, NA, latitude),
                    name_of_park = ifelse(is.na(name_of_park), "kafue", name_of_park)),

    vert = read.csv("12S_Summary_20250519.csv", na.strings = c("", "NA")) |>
      dplyr::mutate(name_of_park = ifelse(is.na(name_of_park), "kafue", name_of_park)),

    trnl = read.csv("trnL_Summary_20250519.csv", na.strings = c("", "NA")) |>
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
