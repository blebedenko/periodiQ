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
mod_plot_res_server <- function(id, res, plot_name, from_to = NULL){
  stopifnot(is.reactive(res))
  stopifnot(is.null(from_to) || is.reactive(from_to))
  moduleServer(id,
               function(input, output, session) {



                 if (plot_name == "segments") {
                   output$plot <- renderPlot({
                     params <- res()[["params"]]
                     simulator_data <- res()[["simulator"]]
                     plot_segments(params, simulator_data, segment_n = 24)

                   })
                 }
                 if (plot_name == "hourly") {
                   output$plot <- renderPlot({
                     plot_hourly_queue_arrivals(simulator_data = res()[["simulator"]],
                                              params = res()[["params"]])
                   })

                 }
                 if (plot_name == "patience") {
                   output$plot <-  renderPlot({
                     params <- res()[["params"]]
                     simulator_data <- res()[["simulator"]]
                     plot_patience(simulator_data, params)
                   })
                 }
                 if (plot_name == "interval") {
                   output$plot <- renderPlot({
                     params <- res()[["params"]]
                     simulator_data <- res()[["simulator"]]
                     plot_interval(simulator_data=simulator_data,params=params,from_to = from_to())

                   })}

                   # if (plot_name == "interval") {
                   #   output$plot <- renderPlotly({
                   #     params <- res()[["params"]]
                   #     simulator_data <- res()[["simulator"]]
                   #     plot_interval(simulator_data=simulator_data,params=params,from_to = from_to())
                   #     dat <- res()[["simulator"]]
                   #     params <- res()[["params"]]
                   #
                   #     lambda <- rate_factory(params)
                   #     dat$rate <- lambda(dat$arrival)
                   #     dat %>%
                   #       mutate(queue = as.numeric(scale(queue)),
                   #              rate = as.numeric(scale(rate))) -> dat
                   #
                   #     fig <- plot_ly(data = dat)
                   #
                   #     fig %>%
                   #       add_trace(x = ~ arrival, y = ~queue, mode = "line") %>%
                   #       add_trace(x = ~ arrival, y = ~rate, mode = "line" )
                   #   })
                   # }


                 if (plot_name == "breakdown"){
                   output$plot <- renderPlot({
                     params <- res()[["params"]]
                     simulator_data <- res()[["simulator"]]
                     plot_hourly_join_prob_queue(simulator_data, params)
                   })
                 }

               })

  }


## To be copied in the UI
# mod_plot_res_ui("plot_res_1")

## To be copied in the server
# mod_plot_res_server("plot_res_1")
