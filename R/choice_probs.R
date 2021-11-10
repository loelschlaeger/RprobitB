#' Compute choice probabilities of an \code{RprobitB_model}.
#' @description
#' This function computes the choice probabilities of an \code{RprobitB_model}.
#' @param object
#' An object of class \code{RprobitB_model}.
#' @param at_true
#' If \code{TRUE}, choice probabilities are computed at the true parameters.
#' @return
#' A data frame, choice situations in rows and alternatives in columns.
#' @export

choice_probs = function(object, at_true = FALSE) {
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
  probabilities = as.data.frame(probabilities)
  probabilities = cbind(object$data$choice_data$id, probabilities)
  colnames(probabilities) = c("id", "N","T",object$data$alternatives)
  return(probabilities)
}
