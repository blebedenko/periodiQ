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
  #library(shinyWidgets)
  mod_welcome_server("welcome_1")
  mod_introduction_server("introduction_1")
  res <- mod_simulation_progress_server("simulation_custom_1")
  pre <- mod_simulation_pregenerated_server("simulation_pregenerated_1")
  observe(reactive(input$from_to))




   mod_plot_res_server("plot_res_segments",res = res, "segments")
  mod_plot_res_server("plot_res_patience",res = res, "patience")
  mod_plot_res_server("plot_res_hourly",res = res, "hourly")
  mod_plot_res_server("plot_res_interval",res = res, "interval", from_to = reactiveVal(value = c(0,1)))
  mod_plot_res_server("plot_res_breakdown",res = res, "breakdown")

  # pregenerated
  # mod_plot_res_server("plot_pre_segments",res = pre, "segments")
  # mod_plot_res_server("plot_pre_patience",res = pre, "patience")
  # mod_plot_res_server("plot_pre_hourly",res = pre, "hourly")
  # mod_plot_res_server("plot_pre_interval",res = pre, "interval", from_to = reactiveVal(value = c(0,1)))
  # mod_plot_res_server("plot_pre_breakdown",res = pre, "breakdown")





}
