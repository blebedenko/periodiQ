#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import fontawesome purrr dplyr
#' @importFrom shiny NS tagList
mod_welcome_ui <- function(id){
  ns <- NS(id)
  icon_list <- lapply(c("home","graduation-cap","dice","chart-line","balance-scale") ,fontawesome::fa)
  description_list <- list(
    home = "This current landing page",
    introduction = "Exposition of the model and the problem",
    simulation = "Simulation engine for making your own results",
    results = "Estimation results from  large and small datasets",
    conclusion = "Final remarks"
  )

  instruction_list <- mapply(paste ,icon_list," - ",description_list,USE.NAMES = F,SIMPLIFY = F)
  instructions <- unlist(instruction_list)
  tagz <- lapply(instruction_list, htmltools::p)
  code <- purrr::reduce(tagz, paste0)
  tagList(
    includeMarkdown(
      system.file("app/www/welcome.md", package = "periodiQ")),
    div(HTML(instructions[1])),
    br(),
    div(HTML(instructions[2])),
    br(),
    div(HTML(instructions[3])),
    br(),
    div(HTML(instructions[4])),
    br(),
    div(HTML(instructions[5])),
    br()
  )

}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
