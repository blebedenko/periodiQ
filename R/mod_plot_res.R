#' plot_res UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom magrittr %>%
#' @importFrom shiny NS tagList
mod_plot_res_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot")),
    #uiOutput(ns("ui"))
  )
}

#' plot_res Server Functions
#' @importFrom magrittr %>%
#' #' @noRd
mod_plot_res_server <- function(id, res, plot_name, from_to = c(0, 1)){
  stopifnot(is.reactive(res))
  moduleServer(id,
               function(input, output, session) {



                 if (plot_name == "segments") {
                   output$plot <- renderPlot({
                     params <- res()[["params"]]
                     simulator_data <- res()[["simulator"]]
                     plot_segments(params, simulator_data, segment_n = 24)

                   })
                 }
                 # if (plot_name == "arrivals") {
                 #   output$plot <- renderPlot({
                 #     pltArrivalsByTimeSegment(RES = RES,
                 #
                 #                              params = params)
                 #   })
                 #
                 # }
                 if (plot_name == "patience") {
                   output$plot <-  renderPlot({
                     params <- res()[["params"]]
                     simulator_data <- res()[["simulator"]]
                     plot_patience(simulator_data, params)
                   })
                 }
                 # if (plot_name == "dynamics") {
                 #   hist(RES$AWX$W, main = "Waiting times of joining customers")
                 #
                 # }

               })

  }


## To be copied in the UI
# mod_plot_res_ui("plot_res_1")

## To be copied in the server
# mod_plot_res_server("plot_res_1")
