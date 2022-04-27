#' simulation_config UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulation_config_ui <- function(id){

  # subtext for pre-generated data
  html_sub <- list(
    HTML('<p align = "left"> Scenario C1: &gamma; = 10, &lambda;<sub>0</sub> = 10, &theta; = 1</p>'),
    HTML('<p align = "left"> Scenario C2: &gamma; = 40, &lambda;<sub>0</sub> = 10, &theta; = 1</p>'),
    HTML('<p align = "left"> Scenario C3: &gamma; = 1, &lambda;<sub>0</sub> = 2, &theta; = 1</p>'),
    HTML('<p align = "left"> Scenario C4: &gamma; = 100, &lambda;<sub>0</sub> = 50, &theta; = 1</p>')
  )

  ns <- NS(id)
  tagList(
    dropdownButton(
      inline = TRUE,
      circle = FALSE,
      icon = icon("gear"),
      tooltip = tooltipOptions(title = "Click to see inputs !"),
      label = "Custom scenario",
      ### parameter input  controllers -----
      tags$h3("Choose parameters"),
      helpText("Job and service:"),
      numericInput(
        inputId = NS(id, "mu"),
        label = HTML("&mu; - shape"),
        value = 1,
        min = 0.1,
        max = 10,
        step = 0.1
      ),
      numericInput(
        inputId = NS(id, "eta"),
        label = HTML("&eta; - scale"),
        value = 1,
        min = 0.1,
        max = 10,
        step = 0.1
      ),
      numericInput(
        inputId = NS(id, "s"),
        label = "s - number of servers",
        value = 50,
        min = 1,
        max = 100,
        step = 1L
      ),
      # tags$hr(style = "border-color: black;"),

      helpText("Exponential patience:"),
      numericInput(
        inputId = NS(id, "theta"),
        label = HTML("&theta;"),
        value = 1,
        min = 0.1,
        max = 10,
        step = 0.1
      ),

      helpText("Intensity function:"),
      numericInput(
        inputId = NS(id, "gamma"),
        label = HTML("&gamma; - the periodical component"),
        value = 100,
        min = 0.1,
        max = 100,
        step = 0.1
      ),
      numericInput(
        inputId = NS(id, "lambda_0"),
        label = HTML("&lambda;<sub>0</sub> - the constant component"),
        value = 50,
        min = 0.1,
        max = 100,
        step = 0.1
      ),
      numericInput(
        inputId = NS(id, "sample_size"),
        label = HTML("n - the sample size"),
        value = 1000,
        min = 50,
        max = 1000000,
        step = 50,
      )

    ),
    ### pre-generated data selector -----
    dropdownButton(
      inline = TRUE,
      circle = FALSE,
      icon = icon("gear"),
      tooltip = tooltipOptions(title = "Click to see inputs !"),
      label = "Pre-generated",
      tags$h3("Choose dataset"),
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
    )


  )
}

#' simulation_config Server Functions
#'
#' @noRd
mod_simulation_config_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    reactive(param_list(
      gamma = input$gamma,
      lambda_0 = input$lambda_0,
      theta =input$theta,
      eta = input$eta,
      mu = input$mu,
      s = input$s,
      sample_size = input$sample_size
    ))


  })
}

## To be copied in the UI
# mod_simulation_config_ui("simulation_config_1")

## To be copied in the server
# mod_simulation_config_server("simulation_config_1")
