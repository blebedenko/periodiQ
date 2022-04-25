#'  Make Parameter list
#'
#' @param gamma periodical component of the rate function.
#' @param lambda_0  constant component of the rate function.
#' @param theta  exponential patience rate parameter.
#' @param eta shape parameter of the job size.
#' @param mu rate parameter of the job size.
#' @param s number of servers
#' @param model (default to "cosine_exp") names of the model to use
#' @return named list with all the relevant parameters.
#' @export
#'
#' @examples
#' listParams(gamma=10,lambda_0=20,theta=2.5, eta = 1, mu = 1 , s = 3)
param_list <-
  function(gamma, lambda_0, theta, eta, mu, s,  model = "cosine_exp") {
    par_list <-
      list(
        gamma = gamma,
        lambda_0 = lambda_0,
        theta = theta,
        eta = eta,
        mu = mu,
        s = s
      )
    return(par_list)
  }


#' Rate function factory
#'
#' @param params  named list of parameters (output of 'listParams()').
#'
#' @return A function with argument t that computes the arrival rate.
#' @export
#'
#' @examples
#' params <- listParams(gamma=10,lambda_0=20,theta=2.5, eta = 1, mu = 1 , s = 3)
#' rate <- rate_factory(params)
#' rate(t = runif(3))
rate_factory <- function(params) {
  gamma <- params$gamma
  lambda_0 <- params$lambda_0
  return(function(t)
    lambda_0 + (gamma / 2) * (1 + cos(2 * pi * t)))
}

#' Next Arrival of inhomogeneous Poisson process
#'
#' Generates  next arrival time (not inter-arrival!) of a nonhomogeneous Poisson process.
#' @param current_time the time on the clock.
#' @param params  named list of parameters (output of 'listParams()').
#' @param model (currently inactive, do not use)
#' @return time of the next arrival.
#'
#' @export
#'
#' @examples
#' params <- listParams(gamma=10,lambda_0=20,theta=2.5, eta = 1, mu = 1 , s = 3)
#' next_arrival(1.2,params = params)

next_arrival <- function(current_time, params, model = "cosine") {
  gamma <- params$gamma
  lambda_0 <- params$lambda_0
  lambda_sup <- gamma + lambda_0 # highest rate in the cosine model
  rate <- rate_factory(params)
  arrived <- FALSE
  while (!arrived) {
    u1 <- stats::runif(1)
    current_time <-
      current_time - (1 / lambda_sup) * log(u1) # generate next arrival from Pois(sup_lambda)
    u2 <- stats::runif(1)
    arrived <- u2 <= rate(current_time) / lambda_sup
  }
  return(current_time)
}

#' Generate arrival times
#'
#' @param m number of samples to generate (default = 10000)
#' @param params parameters
#' @param model (currently inactive, do not use)
#' @return A vector of arrival times
#' @export
#'
#' @examples
#' Ar <- make_arrivals(params = exampleParams())
#' hist(Ar - trunc(Ar)) # note the cosine structure
make_arrivals <- function(m = 1000, params, model = "cosine"){
  arrival_times <- numeric(length = m)
  arrival_times[1] <- next_arrival(current_time = 0,params = params)
  for (i in 2:m){
    arrival_times[i] <- next_arrival(current_time = arrival_times[i - 1],params = params)
  }
  return(arrival_times)
}


#' Customer's job size and patience
#' Provides with a realization of Y~Exp(theta) and B~Gamma(eta,mu)
#' @param params  named list of parameters (output of 'listParams()').
#' @param patience_distr (currently inactive) name of the patience distribution.
#' @return list with elements 'patience' and 'jobsize'
#' @export
#' @examples
#' params <- listParams(gamma=10,lambda_0=20,theta=2.5, eta = 1, mu = 1 , s = 3)
#'  customer <- customer_exp(params)
#'  customer
customer_exp <- function(params, patience_distr = "exp") {
  theta <- params$theta
  eta <- params$eta
  mu <- params$mu
  B <-  stats::rgamma(1, shape = eta, rate = mu) #Job sizes
  Y <- stats::rexp(1, theta)
  return(list(patience = Y, jobsize = B))
}


#' Liron's Virtual Waiting function
#'
#' @param Res.service vector of residual service times
#' @param s the number of servers
#' @export
#' @examples
#' VW(c(1.2,1.5,0.3), s = 2)
#' VW(c(1.2,1.5,0.3), s = 3)
#' VW(c(1.2,1.5,0.3), s = 4)
VW <- function(Res.service, s) {
  Q.length <- length(Res.service) #Number in the system
  if (Q.length < s) {
    virtual.wait <- 0
  } else
  {
    D.times <- rep(NA, Q.length + 1)
    Vw <- rep(NA, Q.length + 1)
    D.times[1:s] <- Res.service[1:s]
    Vw[1:s] <- rep(0, s) #VW for customers in service
    for (i in (s + 1):(Q.length + 1))
    {
      D.i <- sort(D.times[1:i]) #Sorted departures of customers ahead of i
      Vw[i] <- D.i[i - s] #Virtual waiting time for position i
      if (i <= Q.length) {
        D.times[i] <- Res.service[i] + Vw[i]
      } #Departure times
    }
    virtual.wait <- Vw[Q.length + 1]
  }
  return(virtual.wait)
}


#' full simulation function
#' @description The full-observations simulation function.
#' @param n required number of effective arrivals
#' @param params a list of the model parameters
#' @return The return value, if any, from executing the function.
#'
#' @noRd
full_simulation <- function(n, params, m = 1e6){
  gamma <- params$gamma
  lambda_0 <- params$lambda_0
  theta <- params$theta
  eta <- params$eta
  mu <- params$mu
  s <- params$s
  message("pre-generating arrival times...")
  T_tilde <- make_arrivals(m = m,params = params) # arrival times of ALL customers
  message("Done! Now the queue...")

  T_inter <- diff(T_tilde) # inter-arrival times


  lambdaFunction <-   rate_factory(params)


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



    customer <- customer_exp(params = params)  # generate patience and job size:
    B <- customer$jobsize
    Y <- customer$patience
    total_arrived <- total_arrived + 1
    if (virtual.wait <= Y){

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


    A <- T_inter[arr]
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
      cat("* ")

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


  AWX <- simulator %>%
    dplyr::filter(join == 1) %>%
    dplyr:: select(inter, wait, jump)  %>% # inter-effective-arrival, waiting ,  jumps
    dplyr:: rename(A = inter, W = wait, X = jump )

  RES <- list(

    T_tilde = T_tilde, # all arrivals, for future expansion

    Aj = Aj, # inter-effective-arrivals

    Bj = Bj,
    # jobsizes
    Dj = Dj,
    # joining indicator
    Wj = Wj,
    # waiting time at arrival
    Xj = Xj,
    # waiting time jump
    Qj = Qj,
    # queue length at arrival
    # Q.trans = Q.trans,
    #queue @ transitions
    # IT.times = IT.times,
    #intertransition times
    Yj = Yj,
    # patiences
    Res.service = Res.service,
    # residual service
    virtual.wait = virtual.wait,# virtual waiting,

    simulator = simulator, # data available only to a "simulator" entity

    AWX = AWX, # data that are available to an observer

    params = params #  for convenience in shiny (saves module code)
  )

  return(RES)
}
