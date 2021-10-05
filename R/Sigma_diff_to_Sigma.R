#' Transform differenced to non-differenced error term covariance matrix.
#' @description
#' This function transforms the differenced error term covariance matrix
#' \code{Sigma_diff} back to a non-differenced error term covariance matrix.
#' @param Sigma_diff
#' The error term covariance matrix of dimension \code{J-1} x \code{J-1} which
#' was differenced with respect to alternative \code{i}.
#' @param i
#' An integer, the alternative number with respect to which \code{Sigma_diff}
#' got differenced.
#' @return
#' A covariance matrix of dimension \code{J} x \code{J}. If this covariance
#' matrix gets differenced with respect to alternative \code{i}, the results is
#' \code{Sigma_diff}.

Sigma_diff_to_Sigma = function(Sigma_diff, i = nrow(Sigma_diff) + 1){

  ### check inputs
  if(!is_covariance_matrix(Sigma_diff))
    stop("'Sigma_diff' is no covariance matrix.")
  J = nrow(Sigma_diff) + 1
  if(!(length(i)==1 && is.numeric(i) && i%%1==0 && i<=J && i>=1))
     stop("'i' must an alternative number.")

  ### difference operator
  Delta = function(i){
    Delta = diag(J)[-J,,drop=FALSE]; Delta[,i] = -1
    return(Delta)
  }

  ### Moore-Penrose generalized inverse
  mpgi = function(x){
    xsvd = svd(x)
    Positive = xsvd$d > max(sqrt(.Machine$double.eps) * xsvd$d[1L], 0)
    if(all(Positive)){
      xsvd$v %*% (1/xsvd$d * t(xsvd$u))
    } else if(!any(Positive)){
      array(0, dim(x)[2:1])
    } else {
      xsvd$v[, Positive, drop = FALSE] %*%
        ((1/xsvd$d[Positive]) * t(xsvd$u[, Positive, drop = FALSE]))
    }
  }

  ### transform to non-differenced error-term matrix
  Sigma = mpgi(t(Delta(i)) %*% Delta(i)) %*% t(Delta(i)) %*%
    Sigma_diff %*%
    Delta(i) %*% mpgi(t(Delta(i))%*%Delta(i))

  ### check if Sigma is a covariance matrix
  if(!is_covariance_matrix(Sigma))
    stop("Back-transformed matrix is no covariance matrix.")

  ### check if back-differencing yields differenced matrix
  Sigma_diff_back = Delta(i) %*% Sigma %*% t(Delta(i))
  if(any(abs(Sigma_diff_back - Sigma_diff) > sqrt(.Machine$double.eps)))
    stop("Back-differencing failed.")

  ### return transformed matrix
  return(Sigma)

}
