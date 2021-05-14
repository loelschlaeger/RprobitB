#' Sufficient statistics
#' @description Function that computes sufficient statistics for estimation.
#' @param model A list of model identifications.
#' @param data A list of data information.
#' @return A list of sufficient statistics.

compute_suff_statistics = function(model,data){
  N = model$N
  T = if(length(model$T)==1) rep(model$T,N) else model$T
  J = model$J
  P_r = model$P_r
  P_f = model$P_f
  
  ### utility differences wrt last alternative
  if(model$P_f > 0 || model$P_r > 0){
    Delta_J = diag(J)[-J,,drop=FALSE]; Delta_J[,J] = -1
    for(n in 1:N) data[[n]][["X"]] = lapply(data[[n]][["X"]], function(X_nt) Delta_J %*% X_nt)
  }
    
  y = matrix(0,nrow=N,ncol=max(T))
  for(n in 1:N){
    y_n = data[[n]][[2]]
    y[n,] = c(y_n,rep(NA,max(T)-length(y_n)))
  }
  W = list()
  X = list()
  if(P_f>0 & P_r>0){
    for(n in seq_len(N)){
      for(t in seq_len(T[n])){
        W[[sum(T[seq_len(n-1)])+t]] = data[[n]][[1]][[t]][,seq_len(P_f),drop=FALSE]
        X[[sum(T[seq_len(n-1)])+t]] = data[[n]][[1]][[t]][,-seq_len(P_f),drop=FALSE]
      }
    }
  }
  if(P_f>0 & P_r==0){
    X = NA
    for(n in seq_len(N)){
      for(t in seq_len(T[n])){
        W[[sum(T[seq_len(n-1)])+t]] = data[[n]][[1]][[t]]
      }
    }
  }
  if(P_f==0 & P_r>0){
    W = NA
    for(n in seq_len(N)){
      for(t in seq_len(T[n])){
        X[[sum(T[seq_len(n-1)])+t]] = data[[n]][[1]][[t]]
      }
    }
  }
  
  XkX = NA
  if(P_r>0){
    XkX = list()
    for(n in seq_len(N)){
      XnkXn = 0
      for(t in seq_len(T[n])){
        XnkXn = XnkXn + kronecker(t(X[[sum(T[seq_len(n-1)])+t]]),t(X[[sum(T[seq_len(n-1)])+t]]))
      }
      XkX[[n]] = XnkXn
    }
  }
  
  WkW = NA
  if(P_f>0){
    WkW = matrix(0,nrow=P_f^2,ncol=(J-1)^2)
    for(n in seq_len(N)){
      for(t in seq_len(T[n])){
        WkW = WkW + kronecker(t(W[[sum(T[seq_len(n-1)])+t]]),t(W[[sum(T[seq_len(n-1)])+t]]))
      }
    }
  }
  
  suff_statistics = list("Tvec"   = T,
                         "csTvec" = cumsum(T)-T, 
                         "W"      = W,
                         "X"      = X, 
                         "y"      = y, 
                         "XkX"    = XkX, 
                         "WkW"    = WkW)
  return(suff_statistics)
}