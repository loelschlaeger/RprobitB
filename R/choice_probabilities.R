#' Compute choice probabilities of an \code{RprobitB_model}.
#' @description
#' This function computes the choice probabilities of an \code{RprobitB_model}.
#' @param object
#' An object of class \code{RprobitB_model}.
#' @param at_true
#' If \code{TRUE}, choice probabilities are computed for the true parameter
#' values.
#' @return
#' A data frame, one row per choice situation and one column per alternative.
#' @examples
#' \dontrun{
#' model = mcmc(data = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2))
#' choice_probabilities(model)
#' }
#' @export

choice_probabilities = function(object, at_true = FALSE) {
  if(at_true){
    parameter = object$data$true_parameter
  } else {
    parameter = compute_point_estimates(object, FUN = mean)
  }
  probabilities = matrix(NA, nrow = 0, ncol = object$data$J+2)
  for(n in 1:object$data$N) for(t in 1:object$data$T[n]){
    P_nt = compute_choice_probabilities(
      X = object$data$data[[n]]$X[[t]], parameter = parameter)
    probabilities = rbind(probabilities, c(n,t,P_nt))
  }
  colnames(probabilities) = c("N","T",object$data$alternatives)
  return(probabilities)
}
