#' Compute point estimates of an \code{RprobitB_model}.
#' @description
#' This function computes the point estimates of an \code{RprobitB_model} based
#' on the means of the Gibbs samples.
#' @param object
#' An object of class \code{RprobitB_model}.
#' @return
#' An object of class \code{RprobitB_parameter}.

compute_point_estimates = function(object) {

  ### check input
  if(!class(object) == "RprobitB_model")
    stop("'object' is not of class 'RprobitB_model'.")

  ### extract meta parameters
  P_f = object$data$P_f
  P_r = object$data$P_r
  J = object$data$J
  C = NA

  ### compute parameter statistics
  parameter_statistics = summary.RprobitB_model(object)$parameter_statistics

  ### compute point estimates
  Sigma = as.matrix(parameter_statistics$Sigma[,"mean"], nrow = J, ncol = J)
  if(P_f>0){
    alpha = parameter_statistics$alpha[,"mean"]
  } else {
    alpha = NULL
  }
  if(P_r>0){

  } else {
    s = NULL
    b = NULL
    Omega = NULL
  }

  ### build an return an object of class 'RprobitB_parameter'
  out = RprobitB_parameter(P_f = P_f, P_r = P_r, J = J,
                           alpha = alpha, C = C, s = s, b = b, Omega = Omega,
                           Sigma = Sigma, sample = FALSE)
  return(out)
}
