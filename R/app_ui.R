#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    bs4Dash::dashboardPage(
      freshTheme = fresh::create_theme(
        fresh::bs4dash_vars(
          main_footer_height = "1.1rem",
          main_footer_padding = "0.3rem"
        )
      ),
      dark = NULL,
      help = NULL,
      preloader = list(html = tagList(waiter::spin_5(), h3("Preparing African Parks Diet DNA Database")), color = "#8175AABF"),
      header = bs4Dash::dashboardHeader(
        img(
          src = "www/AfricanParksDNA_logo.png",
          style = "height: 40px;"
        ),
        controlbarIcon = NULL,
        compact = T,
        fixed = TRUE
      ),
      sidebar = bs4Dash::dashboardSidebar(
        id = "sidebar",
        skin = "dark",
        status = "secondary",
        elevation = 0,
        opacity = 0.8,
        expandOnHover = FALSE,
        collapsed = TRUE,
        minified = TRUE,
        sidebarMenu(
          compact = F,
          flat = T,
          id = "sidebarMenu",
          menuItem(
            "Dashboard",
            tabName = "dashboard",
            icon = icon("chart-bar")
          ),
          menuItem(
            "Metadata",
            tabName = "metadata",
            icon = icon("clipboard")
          ),
          menuItem(
            "Vertebrates",
            tabName = "vertebrates",
            icon = icon("cat")
          ),
          menuItem(
            "Plants",
            tabName = "plants",
            icon = icon("seedling")
          )
        )
      ),
      body = bs4Dash::dashboardBody(
        # mod_globals_ui("globals"),
        bs4TabItems(
          bs4TabItem(
            tabName = "dashboard",
            mod_dashboard_ui("dashboard")
          ),
          bs4TabItem(
            tabName = "metadata",
            mod_metadata_ui("metadata")
          )
          # bs4TabItem(
          #   tabName = "vertebrates",
          #   mod_vertebrates_ui("vertebrates")
          # ),
          # bs4TabItem(
          #   tabName = "plants",
          #   mod_plants_ui("plants")
          # )
        )
      )
      # footer = dashboardFooter(
      #   mod_footer_ui("footer"),
      #   fixed = TRUE
      # )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyalert useShinyalert shinyalert
#' @importFrom waiter useWaiter
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "african.parks"
    ),
    useShinyjs(),
    useWaiter()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
