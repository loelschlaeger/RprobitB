#' Transformation of parameter values.
#' @description
#' This function transforms parameter values based on \code{normalization}.
#' @param parameter
#' An object of class \code{RprobitB_parameter}.
#' @param normalization
#' An object of class \code{RprobitB_normalization}.
#' @examples
#' parameter = RprobitB_parameter(P_f = 2, P_r = 2, J = 3, N = 100)
#' normalization = RprobitB_normalization(J = 3, P_f = 2)
#' transform_parameter(parameter = parameter, normalization = normalization)
#' @return
#' An object of class \code{RprobitB_parameter}.

transform_parameter = function(parameter, normalization) {

  ### check inputs
  if(!inherits(parameter, "RprobitB_parameter"))
    stop("'parameter' must be of class 'RprobitB_parameter'.")
  if(!inherits(normalization, "RprobitB_normalization"))
    stop("'normalization' must be of class 'RprobitB_normalization'.")

  ### function to scale the parameters
  scaling = function(par, factor){
    if(any(is.na(par))){
      NA
    } else {
      out = par * factor
      ### preserve names
      names(out) = names(par)
      return(out)
    }
  }

  ### scale elements of 'parameter'
  scale = normalization$scale
  if(scale$parameter == "a"){
    factor = scale$value / parameter$alpha[scale$index]
    parameter$alpha = scaling(parameter$alpha, factor)
    parameter$b = scaling(parameter$b, factor)
    parameter$Omega = scaling(parameter$Omega, factor^2)
    parameter$Sigma = scaling(parameter$Sigma, factor^2)
    parameter$beta = scaling(parameter$beta, factor)
  }
  if(scale$parameter == "s"){
    factor = scale$value / parameter$Sigma[scale$index,scale$index]
    parameter$alpha = scaling(parameter$alpha, sqrt(factor))
    parameter$b = scaling(parameter$b, sqrt(factor))
    parameter$Omega = scaling(parameter$Omega, factor)
    parameter$Sigma = scaling(parameter$Sigma, factor)
    parameter$beta = scaling(parameter$beta, sqrt(factor))
    parameter$Sigma_full = undiff_Sigma(parameter$Sigma, normalization$level)
  }

  ### return 'parameter'
  return(parameter)
}
