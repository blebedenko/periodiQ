#' introduction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_introduction_ui <- function(id){
  ns <- NS(id)
  tagList(
    #htmlTemplate("template.html",document_ = T)
    box(width = 12,collapsible = TRUE,title = "Periodic arrivals",
        h4("nohomo Poisson"),
        h4("periodic poisson"))
    ,
    box(width = 12,collapsible = TRUE, title = "Impatient customers",

        h4("Customer impatience models"),
        uiOutput("introduction")
    ),
    box(width = 12,collapsible = TRUE, title = "Study goal",
        h4('objectives'))
  )
}

#' introduction Server Functions
#'
#' @noRd
mod_introduction_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_introduction_ui("introduction_1")

## To be copied in the server
# mod_introduction_server("introduction_1")
