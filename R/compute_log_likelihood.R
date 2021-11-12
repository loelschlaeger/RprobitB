#' Compute log-likelihood of an \code{RprobitB_model}.
#' @description
#' This function computes the log-likelihood of an \code{RprobitB_model}.
#' @inheritParams choice_probs
#' @return
#' The log-likelihood value.
#' @export

compute_log_likelihood = function(x, at_true = FALSE) {
  probs = choice_probs(x = x, at_true = at_true)
  ll = 0
  for(row in 1:nrow(probs)){
    y_nt = x$data$data[[probs[row,"N"]]]$y[probs[row,"T"]]
    ll = ll + log(probs[row,2+y_nt])
  }
  return(as.numeric(ll))
}
