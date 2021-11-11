#' Compute point estimates of an \code{RprobitB_model}.
#' @description
#' This function computes the point estimates of an \code{\link{RprobitB_model}}.
#' Per default, the \code{mean} of the Gibbs samples is used as a point estimate.
#' However, any statistic that computes a single numeric value out of a vector of
#' Gibbs samples can be specified for \code{FUN}.
#' @param x
#' An object of class \code{\link{RprobitB_model}}.
#' @param FUN
#' A function that computes a single numeric value out of a vector of numeric
#' values.
#' @return
#' An object of class \code{\link{RprobitB_parameter}}.
#' @keywords
#' internal

compute_point_estimates = function(x, FUN = mean) {

  ### check input
  if(!class(x) == "RprobitB_model")
    stop("'x' is not of class 'RprobitB_model'.")
  if(!is.list(FUN))
    FUN = list(FUN)
  if(length(FUN) != 1 || class(FUN[[1]]) != "function")
    stop("'FUN' must be a function.")

  ### extract meta parameters
  P_f = x$data$P_f
  P_r = x$data$P_r
  J = x$data$J
  C = x$latent_classes$C
  point_estimates = RprobitB_gibbs_samples_statistics(
    gibbs_samples = x$gibbs_samples, FUN = FUN)

  ### compute point estimates
  if(P_f>0){
    alpha = as.numeric(point_estimates$alpha)
  } else {
    alpha = NULL
  }
  if(P_r>0){
    s = as.numeric(point_estimates$s)[1:C]
    b = matrix(point_estimates$b, nrow = P_r, ncol = C)
    Omega = matrix(point_estimates$Omega, nrow = P_r^2, ncol = C)
  } else {
    s = NULL
    b = NULL
    Omega = NULL
  }
  Sigma = matrix(point_estimates$Sigma, nrow = J-1, ncol = J-1)

  ### build an return an object of class 'RprobitB_parameter'
  out = RprobitB_parameter(P_f = P_f, P_r = P_r, J = J,
                           alpha = alpha, C = C, s = s, b = b, Omega = Omega,
                           Sigma = Sigma, sample = FALSE)
  return(out)
}
