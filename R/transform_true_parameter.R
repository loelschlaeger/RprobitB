#' Transformation of true parameter values.
#' @description
#' This function transforms the true parameter values 'true_parameter' based
#' on 'normalization$scale'.
#' @param true_parameter
#' An object of class \code{RprobitB_true_parameter}.
#' @param normalization
#' An object of class \code{RprobitB_normalization}.
#' @examples
#' true_parameter = RprobitB_parameter(P_f = 2, P_r = 2, J = 3, N = 100)
#' normalization = RprobitB_normalization(J = 3, P_f = 2)
#' transform_true_parameter(true_parameter = true_parameter,
#'                          normalization = normalization)
#' @return
#' An object of class \code{RprobitB_parameter}.

transform_true_parameter = function(true_parameter, normalization) {

  ### check inputs
  if(!inherits(true_parameter, "RprobitB_true_parameter"))
    stop("'true_parameter' must be of class 'RprobitB_true_parameter'.")
  if(!inherits(normalization, "RprobitB_normalization"))
    stop("'normalization' must be of class 'RprobitB_normalization'.")

  ### function to scale the parameters
  scaling = function(par, factor) if(any(is.na(par))) NA else par * factor

  ### scale elements of 'true_parameter'
  scale = normalization$scale
  if(scale$parameter=="a"){
    factor = scale$value / true_parameter$alpha[scale$index]
    true_parameter$alpha = scaling(true_parameter$alpha, factor)
    true_parameter$b = scaling(true_parameter$b, factor)
    true_parameter$Omega = scaling(true_parameter$Omega, factor^2)
    true_parameter$Sigma = scaling(true_parameter$Sigma, factor^2)
    true_parameter$beta = scaling(true_parameter$beta, factor)
  }
  if(scale$parameter=="s"){
    factor = scale$value / true_parameter$Sigma[scale$index,scale$index]
    true_parameter$alpha = scaling(true_parameter$alpha, sqrt(factor))
    true_parameter$b = scaling(true_parameter$b, sqrt(factor))
    true_parameter$Omega = scaling(true_parameter$Omega, factor)
    true_parameter$Sigma = scaling(true_parameter$Sigma, factor)
    true_parameter$beta = scaling(true_parameter$beta, sqrt(factor))
  }

  ### return 'true_parameter'
  return(true_parameter)
}
