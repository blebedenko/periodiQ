#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard shinydashboardPlus shinyWidgets
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    #  UI logic ----
    shinydashboard::dashboardPage(
      ## header ----
      header = dashboardHeader(),
      ## sidebar -----
      sidebar = dashboardSidebar(
        sidebarMenu(
          menuItem(
            text = "Home",
            tabName = "home",
            icon = icon("home")
          ),
          menuItem(
            text = "Introduction",
            tabName = "introduction",
            icon = icon("graduation-cap")
          ),
          menuItem("Simulation", tabName = "simulation",
                   icon = icon("dice")),
          menuItem(
            "Results",
            tabName = "results",
            icon = icon("chart-line")
          ),
          menuItem(
            "Conclusions",
            tabName = "conclusions",
            icon = icon("balance-scale")
          )
        )
      ),
      ## body ----
      body = shinydashboard::dashboardBody(tabItems(
        ## welcome ----
        tabItem("home",
                mod_welcome_ui("welcome_1")),
        ## introduction ----
        tabItem("introduction",
                # htmlTemplate("template.html",document_ = T)
                mod_introduction_ui("introduction_1")),
        ## simulation -----
        tabItem(
          "simulation",
          fluidRow(
            box(
              width = 6,
              collapsible = T,
              mod_simulation_custom_ui("simulation_custom_1")
            ),
            box(
              width = 6,
              collapsible = T,
              mod_simulation_pre_ui("simulation_pre_1")
            )
          ),
          fluidRow(
            box(

              sliderInput(
                "from_to",
                label = "Slide to select timeframe:",
                min = 0,
                max = 5,
                step = 0.1,
                value = c(0, 1),
                width = '100%'
              ),
              title = "Queue Dynamics",
              collapsible = TRUE,
              collapsed = FALSE
            ),
            box(
              sliderInput(
                "n_segments",
                label = "Choose how many segments per day:",
                min = 3,
                max = 120,
                value = 24L,
                step = 1L,
                round = T
              ),
              title = "Arrivals by time segments",
              collapsible = TRUE,
              collapsed = FALSE
            )
          ),
          fluidRow(
            box(
              title = "Patience",
              collapsible = TRUE,
              collapsed = FALSE
            ),
            box(
              title = "Waiting",
              collapsible = TRUE,
              collapsed = FALSE
            )
          )
        ),
        ## results ----
        tabItem(tabName = "results",
                h2("theoretical and empirical results")),
        ## conclusions ----
        tabItem(tabName = "conclusions",
                h2("summary and conclusions here"))
      ))
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
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www",
                    app_sys("app/www"))



  tags$head(favicon(),
            bundle_resources(path = app_sys("app/www"),
                             app_title = "periodiQ")
            # Add here other external resources
            # for example, you can add shinyalert::useShinyalert())))
  )
}
