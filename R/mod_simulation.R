#' #' simulation UI Function
#' #'
#' #' @description A shiny Module.
#' #'
#' #' @param id,input,output,session Internal parameters for {shiny}.
#' #'
#' #' @noRd
#' #'
#' #' @importFrom shiny NS tagList
#' mod_simulation_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'     uiOutput(ns("entire_ui"))
#'   )
#' }
#'
#' #' simulation Server Functions
#' #'
#' #' @noRd
#' mod_simulation_server <- function(id, simulation_config){
#'   stopifnot(is.reactive(res))
#'   moduleServer( id, function(input, output, session){
#'     ns <- session$ns
#'     output$entire_ui <- renderUI({
#'       fluidRow(
#'       model_info_html(res()[["params"]]),
#'       ## parameter values choices ----
#'       ### drop menu settings -----
#'       dropdownButton(
#'         inline = TRUE,
#'         width = 12,
#'         circle = FALSE,
#'         icon = icon("gear"),
#'         tooltip = tooltipOptions(title = "Click to see inputs !"),
#'         label = "Custom scenario",
#'         ### parameter input  controllers -----
#'         tags$h3("Choose parameters"),
#'         helpText("Job and service:"),
#'         numericInput(
#'           inputId = NS(id, "mu"),
#'           label = HTML("&mu; - shape"),
#'           value = 1,
#'           min = 0.1,
#'           max = 10,
#'           step = 0.1
#'         ),
#'         numericInput(
#'           inputId = NS(id, "eta"),
#'           label = HTML("&eta; - scale"),
#'           value = 1,
#'           min = 0.1,
#'           max = 10,
#'           step = 0.1
#'         ),
#'         numericInput(
#'           inputId = NS(id, "s"),
#'           label = "s - number of servers",
#'           value = 50,
#'           min = 1,
#'           max = 100,
#'           step = 1L
#'         ),
#'         # tags$hr(style = "border-color: black;"),
#'
#'         helpText("Exponential patience:"),
#'         numericInput(
#'           inputId = NS(id, "theta"),
#'           label = HTML("&theta;"),
#'           value = 1,
#'           min = 0.1,
#'           max = 10,
#'           step = 0.1
#'         ),
#'
#'         helpText("Intensity function:"),
#'         numericInput(
#'           inputId = NS(id, "gamma"),
#'           label = HTML("&gamma; - the periodical component"),
#'           value = 100,
#'           min = 0.1,
#'           max = 100,
#'           step = 0.1
#'         ),
#'         numericInput(
#'           inputId = NS(id, "lambda_0"),
#'           label = HTML("&lambda;<sub>0</sub> - the constant component"),
#'           value = 50,
#'           min = 0.1,
#'           max = 100,
#'           step = 0.1
#'         ),
#'         numericInput(
#'           inputId = NS(id, "sample_size"),
#'           label = HTML("n - the sample size"),
#'           value = 1000,
#'           min = 50,
#'           max = 1000000,
#'           step = 50,
#'         )))
#'         ###
#'
#'
#'
#'       ## generate button ----
#'       actionBttn(
#'         inputId = "simulation_go",
#'         label = "generate!",
#'         icon = icon("play"),
#'         color = "royal"
#'       )
#'
#'       )
#'
#'
#'
#'       reactive(param_list(
#'         gamma = input$gamma,
#'         lambda_0 = input$lambda_0,
#'         theta =input$theta,
#'         eta = input$eta,
#'         mu = input$mu,
#'         s = input$s
#'       ))
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'     })
#'
#'
#'   })
#' }
#'
#' ## To be copied in the UI
#' # mod_simulation_ui("simulation_1")
#'
#' ## To be copied in the server
#' # mod_simulation_server("simulation_1")
