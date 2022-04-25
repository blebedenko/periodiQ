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
                           full_simulation(n = 1000, params = params())
                         ans
                       })
  observe(print(res()))
  # PROBLEM IS HERE
  mod_plot_res_server("plot_res_segments",res = res, "segments", from_to = c(0, 1))
  mod_plot_res_server("plot_res_patience",res = res, "patience")


}
