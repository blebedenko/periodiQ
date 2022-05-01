#' simulation_custom UI Function
#'
#' @description The module concerning the interactive simulation
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return returns the results list of the simulation
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulation_progress_ui <- function(id) {
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
      '<p>This section produces simulations with parameter settings of your choice.</p>

              <p>Change parameters in the menu below, then click the "simulate" button to produce a new realization.</p>

              <p>Alternatively, you can use one of the large pre-generated datasets by hitting one of the scenario buttons.</p>

              <p>Descriptive analyses of the realization will be produced below after the simulation is over.</p>'
    ),
    shiny::fixedRow(
      ## parameter values choices ----

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
               shinyWidgets::radioGroupButtons(
                 inputId = NS(id,"patience_model"),
                 label = "Patience model:",selected = "deterministic",
                 choices = c("deterministic", "exponential"),
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

             ),

              shinyWidgets::actionBttn(
               inputId = ns("simulation_go"),
               label = "simulate!",
               style = "unite",
               color = "primary")

    )




  )


}

#' simulation_progress Server Functions
#'
#' @noRd
mod_simulation_progress_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe(input$patience_model)
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
            sample_size = input$sample_size,
            patience_model = input$patience_model)#input$patience_model)
        # copy the simulation function here

        withProgress(message = "Simulating queue, sit tight...",value = 0,expr = {
          n <- params$sample_size
          m <- 10 * n
          gamma <- params$gamma
          lambda_0 <- params$lambda_0
          theta <- params$theta
          eta <- params$eta
          mu <- params$mu
          s <- params$s
          message("pre-generating ", m ," arrival times...")
          T_tilde <- make_arrivals(m = m,params = params) # arrival times of ALL customers
          message("Done! Now starting the queue...")

          T_inter <- diff(T_tilde) # inter-arrival times


          lambda <-   rate_factory(params)


          #Running simulation variables
          klok <- 0
          effective_counter <- 0 #Observation counter
          Q.length <- 0 #Queue length (including service)
          virtual.wait <- 0 #Virtual waiting time process
          effective_counter <- 0


          #Output vectors:
          Wj <- rep(NA, n) #Vector of workloads before jumps
          Xj <- rep(NA, n) #Vector of workload jumps
          Aj <- rep(NA, n) #Vector of effective inter-arrival times
          Qj <- rep(NA, n) #Vector of queue lengths at arrival times
          Yj <-  rep(NA, n) # Vector of patience values of customers that join
          Dj <- rep(NA,n) # did the customer join?
          Bj <- rep(NA, n) # workloads

          Q.trans <- 0 #Vector of queue lengths at transitions
          IT.times <- numeric(0) #Vector of inter-transition times
          Pl <- 1 #Proportion of lost customers
          Nl <- 0 # number of lost customers
          total_arrived <- 0
          trans.last <- 0 #Counter of time since last transition
          time.last <- 0 #Counter of time since last admission event
          Res.service <- numeric(0) #Vector of residual service times

          # for (arr in 1:(length(T_tilde) - 1)){
          arr <- 1
          while (effective_counter < n + 1) {
            if ((arr %% m) == 0){ # finished arrival times vec, make more
              message("arrival times reached last, generating ",m, " more...")
              T_tilde <- c(T_tilde,make_arrivals(m = m,params = params, start_time = T_tilde[arr]))
              message("Done. Back to the queue...")
            }



            customer <- summon_customer(params = params)
            B <- customer$jobsize
            Y <- customer$patience
            total_arrived <- total_arrived + 1

            if (virtual.wait <= Y){

              #######
              # unique to shiny #
              shiny::incProgress(amount = 1/n,
                                 message = Y)


              ########


              effective_counter <- effective_counter + 1 #Count observation
              Res.service <- c(Res.service, B) # add  to residual service
              Q.length <- length(Res.service) # determine queue length
              # Q.trans <- c(Q.trans, Q.length) # add current queue length %%%%% SLOW



              Dj[arr] <- 1 # mark customer joined
              #IT.times <- c(IT.times, trans.last) # update transition time
              trans.last <- 0 # Reset transition time
              time.last <- 0 # Reset last arrival time
              Aj[arr] <- time.last # inter-effective-arrival time

            } else {

              Nl <- Nl + 1
              Dj[arr] <- 0 # customer balked

            }

            # add new observations:
            Wj[arr] <- virtual.wait # waiting time experienced
            Qj[arr] <- Q.length - 1 # queue length (excluding new arrival)
            Yj[arr] <- Y # patience of the customer arriving
            Bj[arr] <- B # jobsize of customer


            A <- diff(T_tilde[c(arr,arr+1)])
            klok <- klok + A
            time.last <-
              time.last + A #Add arrival time to effective arrival time

            #Departure and residual service times of customers in the system:
            Q.length <- length(Res.service) #Queue length
            D.times <- rep(NA, Q.length) #Departure times
            Vw <- rep(NA, Q.length) #Virtual waiting times
            for (i in 1:Q.length)
            {
              if (i <= s)
              {
                Vw[i] <- 0 #No virtual waiting time
                D.times[i] <-
                  Res.service[i] #Departure time is the residual service time
                Res.service[i] <-
                  max(Res.service[i] - A, 0) #Update residual service time
              } else
              {
                D.i <- sort(D.times[1:i]) #Sorted departures of customers ahead of i
                Vw[i] <- D.i[i - s] #Time of service start for customer i
                D.times[i] <- Res.service[i] + Vw[i] #Departure time
                serv.i <-
                  max(0, A - Vw[i]) #Service obtained before next arrival
                Res.service[i] <-
                  max(Res.service[i] - serv.i, 0) #New residual service
              }
            }

            # Update jump of virtual waiting time
            if (Q.length < s ||  Dj[arr] == 0)
            {
              Xj[arr] <- 0
            } else
            {
              Xj[arr] <- sort(D.times)[Q.length + 1 - s] - virtual.wait
            }

            #Update residual service times:
            Res.service <-
              Res.service[!(Res.service == 0)] #Remove completed services

            #Update transition times and queue lengths:
            D.before <-
              which(D.times <= A) #Departing customers before next arrival
            if (length(D.before) > 0)
            {
              T.d <- sort(D.times[D.before]) #Sorted departure times
              for (i in 1:length(D.before))
              {
                # Q.trans <-
                #   c(Q.trans, Q.length - i) #Update queue length at departures
                if (i == 1)
                {
                  trans.last <- trans.last + T.d[1] #Update time since last transition
                  # IT.times <-
                  #   c(IT.times, trans.last) #Departure transition time
                  trans.last <- 0 #Reset transition time
                } else
                {
                  trans.last <-
                    trans.last + T.d[i] - T.d[i - 1] #Update time since last transition
                  # IT.times <-
                  #   c(IT.times, trans.last) #Departure transition time
                  trans.last <- 0 #Reset transition time
                }

              }
              trans.last <-
                A - T.d[i] #Update remaining time until next arrival
            } else if (length(D.before) == 0)
            {
              trans.last <-
                trans.last + A #Update timer since least transition with new arrival
            }
            virtual.wait <- VW(Res.service, s) #Update virtual waiting time

            arr <- arr + 1 # next arrival time

            # progress bar
            if (effective_counter %% 1000 == 0)
              message(effective_counter %/% 1000,appendLF = TRUE)



          }



          # prepare two datasets for output:
          # 1. - fullest data, available only to a "simulator", for modeling flexibility
          # 2. - AWX, data only at effective arrivals

          arrival <- T_tilde[1:length(Dj)]  # subset pregenerated times

          simulator <-
            dplyr::bind_cols(
              arrival = arrival,
              inter = c(0, diff(arrival)), # the 1st interarrival is zero
              job = Bj,
              join = Dj,
              wait = Wj,
              jump = Xj,
              queue = Qj,
              patience = Yj) %>%
            dplyr::mutate(time_of_day = arrival - trunc(arrival),
                          hour = floor(24 * time_of_day),
                          day = trunc(arrival))



        })

        list(simulator = simulator, params = params)

      })

    res # return the results


  })
}


## To be copied in the UI
# mod_simulation_progress_ui("simulation_custom_1")

## To be copied in the server
# mod_simulation_progress_server("simulation_custom_1")
