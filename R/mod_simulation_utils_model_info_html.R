#' make html code of model info for the chosen parameters
#'
#' @param params parameters
#' @param digits number of digits to round to (default = 3)
#' @param scenario if null - info about the model from params. if "C1" , "C2" etc, the value is provided by a helper function
#' @return
#' @export
#'
#' @examples
model_info_html <- function(params, digits = 3, scenario = NULL) {
  if(is.null(scenario)){
    eta <- params$eta
    s <- params$s
    mu <- params$mu
    gamma <- params$gamma
    lambda_0 <- params$lambda_0
    rates <- lambda_0 + c(0, gamma / 2, gamma)
    names(rates) <- c("min", "ave", "max")
    rhos <- rates * eta / (s * mu)
  } else {
    params <- scenario_params()
  }

  # round for nicer output:
  rhos <- round(rhos, digits = digits)
  sentence1 <-
    paste0("The average rate is ",
           rates[2],
           " and the range is ",
           rates[1],
           " - ",
           rates[3],
           ".")
  sentence2 <-
    paste0("Thus the offered load is on average ",
           rhos[2],
           " and ranges ",
           rhos[1],
           " - ",
           rhos[3],
           ".")
  rho_text <- paste(sentence1, sentence2, sep = "\n")


  params_round <- sapply(params, round, digits = digits)
  human_text <- with(params,  {
    sentence1 <-
      paste0(
        "The customers' patience is exponentially distributed with parameter: ",
        theta,
        " (mean = ",
        round(1 / theta, digits  = 3),
        ")"
      )

    jobsize_distr <- paste0("Gamma(", eta, ", ", mu, ")")
    sentence2 <-
      paste0(
        "The job sizes are independent of the patience and distributed ",
        jobsize_distr,
        " (mean = ",
        round(eta / mu, digits = digits),
        "). "
      ) # average jobsize

    sentence3 <-
      paste0(
        "This implies that customers are willing to wait ",
        100 * (1 / theta) / (eta / mu),
        "%",
        " of their job requirement, on average"
      )




    # the human text:
    paste(sentence1, sentence2, sentence3, sep = "\n")
  }
  )
  txt <- paste0('<div> ', c(rho_text,human_text), '</div>')
  return(txt)

}
