#' Compute log-likelihood of an \code{RprobitB_fit}.
#'
#' @description
#' This function computes the log-likelihood of an \code{RprobitB_fit}.
#'
#' @inheritParams choice_probabilities
#'
#' @return
#' The log-likelihood value.
#'
#' @examples
#' data <- simulate_choices(form = choice ~ covariate, N = 10, T = 10, J = 2)
#' x <- mcmc(data)
#' log_likelihood(x)
#' @export

log_likelihood <- function(x, par_set = mean) {
  probs <- choice_probabilities(x = x, par_set = par_set)
  choices <- as.character(unlist(sapply(x$data$data, `[[`, "y")))
  ll <- 0
  for (row in 1:nrow(probs)) ll <- ll + log(probs[row, choices[row]])
  return(as.numeric(ll))
}
