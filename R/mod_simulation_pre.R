#' simulation_pre UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulation_pre_ui <- function(id){
  ns <- NS(id)
  html_sub <- list(
    HTML('<p align = "left"> Scenario C1: &gamma; = 10, &lambda;<sub>0</sub> = 10, &theta; = 1</p>'),
    HTML('<p align = "left"> Scenario C1: &gamma; = 40, &lambda;<sub>0</sub> = 10, &theta; = 1</p>'),
    HTML('<p align = "left"> Scenario C1: &gamma; = 1, &lambda;<sub>0</sub> = 2, &theta; = 1</p>'),
    HTML('<p align = "left"> Scenario C1: &gamma; = 100, &lambda;<sub>0</sub> = 50, &theta; = 1</p>')
  )
  tagList(
    shinyWidgets::radioGroupButtons(
      inputId = "dataset",
      label = "Available datasets:",
      status = "primary",
      direction = "vertical",
      selected = character(),
      choiceValues = list("C1",
                          "C2",
                          "C3",
                          "C4"),
      choiceNames = html_sub,
      justified = T
    )
  )
}

#' simulation_pre Server Functions
#'
#' @noRd
mod_simulation_pre_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_simulation_pre_ui("simulation_pre_1")

## To be copied in the server
# mod_simulation_pre_server("simulation_pre_1")
