#' Compute log-likelihood of an \code{RprobitB_model}.
#' @description
#' This function computes the log-likelihood of an \code{RprobitB_model}.
#' @inheritParams choice_probabilities
#' @return
#' A numeric value.
#' @examples
#' p = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2, seed = 1)
#' m1 = fit(data = p, seed = 1)
#' compute_loglikelihood(m1)
#' @export

compute_loglikelihood = function(object, at_true = TRUE) {
  ### TODO: change to estimated parameters
  probs = choice_probabilities(object = object, at_true = at_true)
  ll = 0
  for(row in 1:nrow(probs)){
    y_nt = object$data$data[[probs[row,"N"]]]$y[probs[row,"T"]]
    ll = ll + probs[row,2+y_nt]
  }
  return(as.numeric(ll))
}
