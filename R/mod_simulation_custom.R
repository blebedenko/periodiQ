#' simulation_custom UI Function
#'
#' @description The module concerning the interactive simulation
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return returns the results list of the simulation
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulation_custom_ui <- function(id) {
  ns <- NS(id)
  ## subtext html ----
  html_sub <- list(
    HTML(
      '<p align = "left"> Scenario C1: &gamma; = 10, &lambda;<sub>0</sub> = 10, &theta; = 1</p>'
    ),
    HTML(
      '<p align = "left"> Scenario C1: &gamma; = 40, &lambda;<sub>0</sub> = 10, &theta; = 1</p>'
    ),
    HTML(
      '<p align = "left"> Scenario C1: &gamma; = 1, &lambda;<sub>0</sub> = 2, &theta; = 1</p>'
    ),
    HTML(
      '<p align = "left"> Scenario C1: &gamma; = 100, &lambda;<sub>0</sub> = 50, &theta; = 1</p>'
    )
  )
  tagList(
    ## instructions -----
    HTML(
      '<p>This section produces simulations of the process with parameter settings of your choice.</p>

              <p>Change parameters by clicking <svg aria-hidden="true" role="img" viewBox="0 0 640 512" style="height:1em;width:1.25em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:currentColor;overflow:visible;position:relative;"><path d="M512.1 191l-8.2 14.3c-3 5.3-9.4 7.5-15.1 5.4-11.8-4.4-22.6-10.7-32.1-18.6-4.6-3.8-5.8-10.5-2.8-15.7l8.2-14.3c-6.9-8-12.3-17.3-15.9-27.4h-16.5c-6 0-11.2-4.3-12.2-10.3-2-12-2.1-24.6 0-37.1 1-6 6.2-10.4 12.2-10.4h16.5c3.6-10.1 9-19.4 15.9-27.4l-8.2-14.3c-3-5.2-1.9-11.9 2.8-15.7 9.5-7.9 20.4-14.2 32.1-18.6 5.7-2.1 12.1.1 15.1 5.4l8.2 14.3c10.5-1.9 21.2-1.9 31.7 0L552 6.3c3-5.3 9.4-7.5 15.1-5.4 11.8 4.4 22.6 10.7 32.1 18.6 4.6 3.8 5.8 10.5 2.8 15.7l-8.2 14.3c6.9 8 12.3 17.3 15.9 27.4h16.5c6 0 11.2 4.3 12.2 10.3 2 12 2.1 24.6 0 37.1-1 6-6.2 10.4-12.2 10.4h-16.5c-3.6 10.1-9 19.4-15.9 27.4l8.2 14.3c3 5.2 1.9 11.9-2.8 15.7-9.5 7.9-20.4 14.2-32.1 18.6-5.7 2.1-12.1-.1-15.1-5.4l-8.2-14.3c-10.4 1.9-21.2 1.9-31.7 0zm-10.5-58.8c38.5 29.6 82.4-14.3 52.8-52.8-38.5-29.7-82.4 14.3-52.8 52.8zM386.3 286.1l33.7 16.8c10.1 5.8 14.5 18.1 10.5 29.1-8.9 24.2-26.4 46.4-42.6 65.8-7.4 8.9-20.2 11.1-30.3 5.3l-29.1-16.8c-16 13.7-34.6 24.6-54.9 31.7v33.6c0 11.6-8.3 21.6-19.7 23.6-24.6 4.2-50.4 4.4-75.9 0-11.5-2-20-11.9-20-23.6V418c-20.3-7.2-38.9-18-54.9-31.7L74 403c-10 5.8-22.9 3.6-30.3-5.3-16.2-19.4-33.3-41.6-42.2-65.7-4-10.9.4-23.2 10.5-29.1l33.3-16.8c-3.9-20.9-3.9-42.4 0-63.4L12 205.8c-10.1-5.8-14.6-18.1-10.5-29 8.9-24.2 26-46.4 42.2-65.8 7.4-8.9 20.2-11.1 30.3-5.3l29.1 16.8c16-13.7 34.6-24.6 54.9-31.7V57.1c0-11.5 8.2-21.5 19.6-23.5 24.6-4.2 50.5-4.4 76-.1 11.5 2 20 11.9 20 23.6v33.6c20.3 7.2 38.9 18 54.9 31.7l29.1-16.8c10-5.8 22.9-3.6 30.3 5.3 16.2 19.4 33.2 41.6 42.1 65.8 4 10.9.1 23.2-10 29.1l-33.7 16.8c3.9 21 3.9 42.5 0 63.5zm-117.6 21.1c59.2-77-28.7-164.9-105.7-105.7-59.2 77 28.7 164.9 105.7 105.7zm243.4 182.7l-8.2 14.3c-3 5.3-9.4 7.5-15.1 5.4-11.8-4.4-22.6-10.7-32.1-18.6-4.6-3.8-5.8-10.5-2.8-15.7l8.2-14.3c-6.9-8-12.3-17.3-15.9-27.4h-16.5c-6 0-11.2-4.3-12.2-10.3-2-12-2.1-24.6 0-37.1 1-6 6.2-10.4 12.2-10.4h16.5c3.6-10.1 9-19.4 15.9-27.4l-8.2-14.3c-3-5.2-1.9-11.9 2.8-15.7 9.5-7.9 20.4-14.2 32.1-18.6 5.7-2.1 12.1.1 15.1 5.4l8.2 14.3c10.5-1.9 21.2-1.9 31.7 0l8.2-14.3c3-5.3 9.4-7.5 15.1-5.4 11.8 4.4 22.6 10.7 32.1 18.6 4.6 3.8 5.8 10.5 2.8 15.7l-8.2 14.3c6.9 8 12.3 17.3 15.9 27.4h16.5c6 0 11.2 4.3 12.2 10.3 2 12 2.1 24.6 0 37.1-1 6-6.2 10.4-12.2 10.4h-16.5c-3.6 10.1-9 19.4-15.9 27.4l8.2 14.3c3 5.2 1.9 11.9-2.8 15.7-9.5 7.9-20.4 14.2-32.1 18.6-5.7 2.1-12.1-.1-15.1-5.4l-8.2-14.3c-10.4 1.9-21.2 1.9-31.7 0zM501.6 431c38.5 29.6 82.4-14.3 52.8-52.8-38.5-29.6-82.4 14.3-52.8 52.8z"/></svg> in the top right corner.</p>

              <p>Just hit the &quot;simulate&quot; button to produce a new realization.</p>

              <p>Alternatively, you can use one of the large pre-generated datasets by hitting one of the scenario buttons.</p>

              <p>Descriptive analyses of the realization will be produced below after the simulation is over.</p>'
    ),
    fluidRow(
      ## parameter values choices ----
      column(width = 8,
      dropdownButton(
        # inline = TRUE,
        # width = 12,
        circle = FALSE,
        icon = icon("gear"),

        tooltip = tooltipOptions(title = "Click to see inputs !"),
        label = "Change simulation & queue parameters:",
        tags$h4("sample size:"),
        ## sample size ----
        sliderInput(
          inputId = NS(id, "sample_size"),
          label = HTML(" "),# "N<sub>eff</sub> - number of arrivals"),
          value = 2000,
          min = 1000,
          max = 100000,
          step = 1000
        ),

        ## patience selector ----
        tags$h4("patience model"),
        shinyWidgets::radioGroupButtons(
          inputId = "patience_model",
          label = " ",
          choiceNames = list( "determistic",
                              "exponential"),

          choiceValues = list("deterministic", "exponential"),
          justified = F,
        ),
        tags$h4("system parameters"),
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

        helpText("Exponential: rate; Determinstic: mean"),
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
        )

      )
    ),

    column(width = 4,
           actionBttn(
             inputId = ns("simulation_go"),
             label = "simulate!",
             style = "unite",
             color = "primary"
           ))
    )




  )


}

#' simulation_custom Server Functions
#'
#' @noRd
mod_simulation_custom_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    res <- eventReactive(
      eventExpr = input$simulation_go,valueExpr = {
        params <-
          param_list(
            gamma = input$gamma,
            lambda_0 = input$lambda_0,
            theta = input$theta,
            eta = input$eta,
            mu = input$mu,
            s = input$s,
            sample_size = input$sample_size
          )

        simulate_queue(params = params,patience_model = input$patience_model)
      })

    res # return the results


  })
}


## To be copied in the UI
# mod_simulation_custom_ui("simulation_custom_1")

## To be copied in the server
# mod_simulation_custom_server("simulation_custom_1")
