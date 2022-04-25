#' list_params
#'
#' @description Standardized parameter input throughout the entire app
#' @param gamma periodical component of the rate function.
#' @param lambda_0  constant component of the rate function.
#' @param theta  exponential patience rate parameter.
#' @param eta shape parameter of the job size.
#' @param mu rate parameter of the job size.
#' @param s number of servers
#' @param model (default to "cosine_exp") names of the model to use
#' @return named list with all the relevant parameters.
#'
#' @noRd
list_params <- function(gamma, lambda_0, theta, eta, mu, s,  model = "cosine_exp") {
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
