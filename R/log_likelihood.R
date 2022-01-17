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

log_likelihood <- function(x, at_true = FALSE) {
  probs <- choice_probabilities(x = x, at_true = at_true)
  ll <- 0
  for (row in 1:nrow(probs)) {
    y_nt <- x$data$data[[probs[row, "id"]]]$y[probs[row, "idc"]]
    ll <- ll + log(probs[row, 2 + y_nt])
  }
  return(as.numeric(ll))
}
