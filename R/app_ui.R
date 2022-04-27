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
          menuItem("Simulation",startExpanded = TRUE,
                   icon = icon("dice"),
                   menuSubItem(tabName = "simulation_custom",text = "Custom",icon = icon("wrench", lib = "glyphicon")),
                   menuSubItem(tabName = "simulation_pregenerated",text = "Pre-generated",icon = icon("paperclip",lib = "glyphicon"))

                   ),
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
        ## simulation menu -----
        tabItem( "simulation_custom",

                 shinydashboardPlus::box(title = "Instructions",width = 12,collapsible = TRUE,
                   mod_simulation_custom_ui("simulation_custom_1")),

          tabBox(width = 12,
                 tabPanel(title = "Arrivals per time segment",
                          mod_plot_res_ui("plot_res_segments")),
                 tabPanel(title = "Patience",mod_plot_res_ui("plot_res_patience")),

                 tabPanel(title = "Queue during time interval",
                          mod_plot_res_ui("plot_res_interval")),

                 tabPanel(title = "Queue  & average arrivals",
                          mod_plot_res_ui("plot_res_hourly")),

                 tabPanel(title = "Hourly breakdown of the queue",
                          mod_plot_res_ui("plot_res_breakdown"))
          )),

          # ## visualizations ----
          #
          #   box(
          #     title = "Arrivals per time segment",
          #     collapsible = TRUE,
          #     collapsed = FALSE,
          #     #width = 6,
          #     mod_plot_res_ui("plot_res_segments")
          #   ),
          #   box(
          #     title = "Patience",
          #     collapsible = TRUE,
          #     collapsed = FALSE,
          #     #width = 6,
          #     mod_plot_res_ui("plot_res_patience")
          #   ),
          # box(
          #   title = "Queue during time interval",
          #   collapsible = TRUE,
          #   collapsed = FALSE,
          #   #width = 6,
          #   mod_plot_res_ui("plot_res_interval"),
          #   uiOutput("res_interval_from_to_ui")
          # ),
          # box(
          #   title = "Queue length & averages arrivals",
          #   collapsible = TRUE,
          #   collapsed = FALSE,
          #   #width = 6,
          #   mod_plot_res_ui("plot_res_hourly")
          # ),
          # box(
          #   title = "Hourly breakdown of the queue",
          #   collapsible = TRUE,
          #   collapsed = FALSE,
          #   #width = 6,
          #   mod_plot_res_ui("plot_res_breakdown")
          # ),

          ## plot in tab box

          # ),
        ## pre-generated ---
        tabItem(tabName = "simulation_pregenerated",
                mod_simulation_pregenerated_ui("simulation_pregenerated_1"),
                tabBox(width = 12,
                       tabPanel(title = "Arrivals per time segment",
                                mod_plot_res_ui("plot_pre_segments")),
                       tabPanel(title = "Patience",
                                mod_plot_res_ui("plot_pre_patience")),

                       tabPanel(title = "Queue during time interval",
                                mod_plot_res_ui("plot_pre_interval")),

                       tabPanel(title = "Queue  & average arrivals",
                                mod_plot_res_ui("plot_pre_hourly")),

                       tabPanel(title = "Hourly breakdown of the queue",
                                mod_plot_res_ui("plot_pre_breakdown"))
                )),


        ## results ----
        tabItem(tabName = "results",
                h2("theoretical and empirical results")),
        ## conclusions ----
        tabItem(tabName = "conclusions",
                h2("summary and conclusions here"))
      )

  ))
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
