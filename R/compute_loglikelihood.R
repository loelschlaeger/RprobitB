#' Compute log-likelihood of an \code{RprobitB_model}.
#' @description
#' This function computes the log-likelihood of an \code{RprobitB_model}.
#' @param object
#' An object of class \code{RprobitB_model}.
#' @return
#' A numeric value.
#' @examples
#' ### probit model
#' p = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2, seed = 1)
#' m1 = fit(data = p, seed = 1)
#' compute_loglikelihood(m1)
#' @export

compute_loglikelihood = function(object) {
  parameter = object$data$true_parameter ### TODO: change to estimated parameters
  ll = 0
  for(n in 1:object$data$N) for(t in 1:object$data$T[n]){
    P_nt = compute_choice_probabilities(X = object$data$data[[n]]$X[[t]],
                                        parameter = parameter)
    ll = ll + P_nt[object$data$data[[n]]$y[t]]
  }
}
