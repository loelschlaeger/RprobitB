#' Provide initial values for the Gibbs sampler
#' @description
#' Function that provides initial values for the Gibbs sampler.
#' @inheritParams RprobitB_data
#' @param C
#' The number (greater or equal 1) of latent classes.
#' @examples
#' set_init(N = 10, T = 10, J = 3, P_f = 1, P_r = 1, C = 2)
#' @return
#' A list of initial values for the Gibbs sampler

set_init = function(N, T, J, P_f, P_r, C){

  ### check inputs
  stopifnot(is.numeric(N), N%%1 == 0, N>0)
  stopifnot(is.numeric(T), T%%1 == 0, T>0)
  stopifnot(is.numeric(P_f), P_f%%1 == 0, P_f>=0)
  stopifnot(is.numeric(P_r), P_r%%1 == 0, P_r>=0)
  stopifnot(is.numeric(C), C%%1 == 0, C>0)

  ### define initial values
  alpha0 = if(P_f>0) numeric(P_f) else NA
  m0 = if(P_r>0) round(rep(N,C)*2^(1:C-1)/sum(2^(1:C-1))) else NA
  b0 = if(P_r>0) matrix(0,nrow=P_r,ncol=C) else NA
  Omega0 =
    if(P_r>0) matrix(rep(as.vector(diag(P_r)),C),nrow=P_r*P_r,ncol=C) else NA
  beta0 = if(P_r>0) matrix(0,nrow=P_r,ncol=N) else NA
  U0 = matrix(0,nrow=J-1,ncol=N*max(T))
  Sigma0 = diag(J-1)

  ### define 'init'
  init = list("alpha0" = alpha0,
              "m0"     = m0,
              "b0"     = b0,
              "Omega0" = Omega0,
              "beta0"  = beta0,
              "U0"     = U0,
              "Sigma0" = Sigma0)

  ### return 'init'
  return(init)
}
