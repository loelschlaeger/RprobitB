#' Simulate choice data
#' @description
#' Function that simulates choice data for RprobitB.
#' @details
#' Please see the vignette "How to simulate choice data?" for more details.
#' @inheritParams check_model
#' @inheritParams check_parm
#' @inheritParams prepare
#' @param distr
#' See https://stackoverflow.com/questions/68452845/how-to-submit-a-vector-of-distributions-to-a-function-in-r
#' @return
#' A list of
#' \itemie{
#'   \item \code{data}
#'   \item \code{parm}
#' }
#' @examples
#' form = choice ~ cost | income | travel_time
#' re = c("cost","ASC")
#' model = c("N" = 100, "T" = 10, "J" = 3, "P_f" = 2, "P_r" = 2, "C" = 1)
#' distr = c()
#' standardize = FALSE
#' @export

simulate = function(form, model, parm, distr, standardize = FALSE) {





  N = model$N
  T = if(length(model$T)==1) rep(model$T,N) else model$T
  J = model$J
  P_f = model$P_f
  P_r = model$P_r
  C = model$C
  alpha = parm$alpha
  s = parm$s
  b = parm$b
  Omega = parm$Omega
  Sigma = parm$Sigma

  if(P_r>0){
    ### draw allocation variable (N x 1)
    z = sample(1:C,N,prob=s,replace=TRUE)

    ### compute class sizes
    m = as.numeric(table(z))

    ### draw beta
    beta = matrix(0,nrow=P_r,ncol=N)
    for(n in seq_len(N)) beta[,n] = b[,z[n]] + t(chol(matrix(Omega[,z[n]],nrow=P_r,ncol=P_r))) %*% rnorm(P_r)

  } else{
    beta = NA
    z = NA
    m = NA
  }

  ### compute lower-triangular Cholesky root of Sigma
  L = t(chol(Sigma))

  ### allocate space for output
  data = list()
  U = matrix(0,nrow=J-1,ncol=sum(T))

  ### difference operator
  Delta_J = diag(J)[-J,,drop=FALSE]; Delta_J[,J] = -1

  for(n in seq_len(N)){
    data[[n]] = list()
    data[[n]][["X"]] = list()
    y_n = numeric(T[n])

    for(t in seq_len(T[n])){
      ### draw covariates
      if(P_f==0 & P_r==0){
        W_nt_X_nt = NA
      } else {
        W_nt_X_nt = matrix(rnorm(J*(P_f+P_r),sd=sd),nrow=J,ncol=(P_f+P_r))
      }
      data[[n]][["X"]][[t]] = W_nt_X_nt

      ### build coefficient vector
      if(P_f>0 & P_r>0) coeff = c(alpha,beta[,n])
      if(P_f>0 & P_r==0) coeff = alpha
      if(P_f==0 & P_r>0) coeff = beta[,n]
      if(P_f==0 & P_r==0) coeff = NA

      ### compute utility and choice decision
      eps  = L %*% rnorm(J-1)
      if(P_f==0 & P_r==0){
        U_nt = eps
      } else {
        V_nt = Delta_J %*% W_nt_X_nt %*% coeff
        U_nt = V_nt + eps
      }
      U[,sum(T[seq_len(n-1)])+t] = U_nt
      y_n[t] = ifelse(max(U_nt) < 0, J, which.max(U_nt))
    }

    data[[n]][["y"]] = y_n
  }

  add_parm = list("U"    = U,
                  "beta" = beta,
                  "z"    = z,
                  "m"    = m)

  return(list("data"     = data,
              "add_parm" = add_parm))
}
