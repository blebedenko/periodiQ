#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # server logic ----
  mod_welcome_server("welcome_1")
  mod_introduction_server("introduction_1")
  res <- mod_simulation_custom_server("simulation_custom_1")
  mod_simulation_pre_ui("simulation_pre_1")
  mod_plot_res_server("plot")


}
