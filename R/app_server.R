#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny magrittr dplyr
#' @importFrom magrittr %>%
#' @noRd
app_server <- function(input, output, session) {
  # server logic ----
  library(tidyverse)
  mod_welcome_server("welcome_1")
  mod_introduction_server("introduction_1")
  params <- mod_simulation_custom_server("simulation_custom_1")
  mod_simulation_pre_ui("simulation_pre_1")
  res <- eventReactive(eventExpr = input$simulation_go,
                       valueExpr = {
                         ans <-
                           full_simulation(n = 10000, params = params())
                         ans
                       })
  mod_plot_res_server("plot_res_segments",res = res, "segments")
  mod_plot_res_server("plot_res_patience",res = res, "patience")
  mod_plot_res_server("plot_res_hourly",res = res, "hourly")

  observe(input$from_to)
  #mod_plot_res_server("plot_res_interval",res = res, "interval",from_to = reactive(input$from_to))

  # create slider for the dynamics plot
  # output$res_interval_from_to_ui <- renderUI({
  #
  #   simulator_data <- res()[["simulator"]]
  #   last_arrival <- 1#max(simulator_data$arrival_time)
  #   sliderInput(
  #     inputId = "plot_res_interval_from_to",
  #     label = "Time interval",
  #     min = 0,
  #     max = last_arrival,
  #     value = c(0, 1),
  #     step = 0.1,ticks = FALSE,round = -1 # round by 0.1
  #
  #   )
  # })

  # update the maximum time when new realization is created:

  #  observeEvent(res(),{
  #   simulator <- res()[["simulator"]]
  #   last_arrival <- max(simulator$arrival)
  #   updateSliderInput(inputId = "plot_res_interval_from_to",max = last_arrival)
  # })





}
