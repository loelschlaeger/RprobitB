#' Transformation of true parameter values
#' @description
#' Function that transforms \code{parm} based on scale.
#' @inheritParams check_parm
#' @inheritParams check_scale
#' @examples
#' parm = check_parm(parm = NULL, P_f = 2, P_r = 2, J = 3)
#' scale = check_scale(scale = NULL, P_f = 2, J = 3)
#' transform_parm(parm = parm, scale = scale)
#' @return
#' The transformed version of \code{parm}

transform_parm = function(parm = parm, scale = scale) {

  ### check inputs
  if(!inherits(parm, "RprobitB_parm"))
    stop("'parm' must be of class 'RprobitB_parm'.")
  if(!inherits(scale, "RprobitB_scale"))
    stop("'scale' must be of class 'RprobitB_scale'.")

  ### function to normalize the parameters
  normalize = function(par, factor) if(any(is.na(par))) NA else par * factor

  ### scale elements of 'parm'
  if(scale$parameter=="a"){
    factor = scale$value / parm$alpha[scale$index]
    parm$alpha = normalize(parm$alpha, factor)
    parm$b = normalize(parm$b, factor)
    parm$Omega = normalize(parm$Omega, factor^2)
    parm$Sigma = normalize(parm$Sigma, factor^2)
    if(!is.null(parm$beta))
      parm$beta = normalize(parm$beta, factor)
    if(!is.null(parm$U))
      parm$U = normalize(parm$U, factor)
  }
  if(scale$parameter=="s"){
    factor = scale$value / parm$Sigma[scale$index,scale$index]
    parm$alpha = normalize(parm$alpha, sqrt(factor))
    parm$b = normalize(parm$b, sqrt(factor))
    parm$Omega = normalize(parm$Omega, factor)
    parm$Sigma = normalize(parm$Sigma, factor)
    if(!is.null(parm$beta))
      parm$beta = normalize(parm$beta, sqrt(factor))
    if(!is.null(parm$U))
      parm$U = normalize(parm$U, sqrt(factor))
  }

  ### return 'parm'
  return(parm)
}
