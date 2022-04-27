#' simulation_pregenerated UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulation_pregenerated_ui <-  function(id){
  ns <- NS(id)
  html_sub <- list(
    HTML('<p align = "left"> Scenario C1: &gamma; = 10, &lambda;<sub>0</sub> = 10, &theta; = 1</p>'),
    HTML('<p align = "left"> Scenario C2: &gamma; = 40, &lambda;<sub>0</sub> = 10, &theta; = 1</p>'),
    HTML('<p align = "left"> Scenario C3: &gamma; = 1, &lambda;<sub>0</sub> = 2, &theta; = 1</p>'),
    HTML('<p align = "left"> Scenario C4: &gamma; = 100, &lambda;<sub>0</sub> = 50, &theta; = 1</p>')
  )
  tagList(
    fluidRow(
      shinyWidgets::pickerInput(inputId = ns("scenario"),
                                label = "Available datasets:",
                                choices = paste0("C",1:4),
                                choicesOpt = list(subtext = html_sub)
                                ),
    # shinyWidgets::radioGroupButtons(
    #
    #   status = "primary",
    #   direction = "vertical",
    #   selected = "C1",
    #   choiceValues = list("C1",
    #                       "C2",
    #                       "C3",
    #                       "C4"),
    #   choiceNames = html_sub,
    #   justified = T
    # )),
    actionBttn(inputId = ns("pre_go"),label = "show!",style = "jelly",color = "royal")
  ))
}

#' simulation_pregenerated Server Functions
#'
#' @noRd
mod_simulation_pregenerated_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ans <- eventReactive(input$pre_go,{
      simulate_queue(scenario_params())
    })

    ans
  })
}

## To be copied in the UI
# mod_simulation_pregenerated_ui("simulation_pregenerated_1")

## To be copied in the server
# mod_simulation_pregenerated_server("simulation_pregenerated_1")
