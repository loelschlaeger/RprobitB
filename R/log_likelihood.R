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
  ll <- 0
  for (row in 1:nrow(probs)) {
    ### extract observed choice
    y_nt <- x$data$data[[probs[row, "id"]]]$y[probs[row, "idc"]]
    ### add contribution
    ll <- ll + log(probs[row, y_nt])
  }
  return(as.numeric(ll))
}
