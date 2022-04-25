#' plot_res UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_res_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot")),
    uiOutput(ns("ui"))
  )
}

#' plot_res Server Functions
#'
#' @noRd
mod_plot_res_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_plot_res_ui("plot_res_1")

## To be copied in the server
# mod_plot_res_server("plot_res_1")
