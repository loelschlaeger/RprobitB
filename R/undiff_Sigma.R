#' Transform differenced to non-differenced error term covariance matrix.
#' @description
#' This function transforms the differenced error term covariance matrix
#' \code{Sigma} back to a non-differenced error term covariance matrix.
#' @param Sigma
#' The error term covariance matrix of dimension \code{J-1} x \code{J-1} which
#' was differenced with respect to alternative \code{i}.
#' @param i
#' An integer, the alternative number with respect to which \code{Sigma}
#' got differenced.
#' @return
#' A covariance matrix of dimension \code{J} x \code{J}. If this covariance
#' matrix gets differenced with respect to alternative \code{i}, the results is
#' \code{Sigma}.

undiff_Sigma = function(Sigma, i = nrow(Sigma) + 1){

  ### check inputs
  Sigma = as.matrix(Sigma)
  if(!is_covariance_matrix(Sigma))
    stop("'Sigma' is no covariance matrix.")
  J = nrow(Sigma) + 1
  if(!(length(i)==1 && is.numeric(i) && i%%1==0 && i<=J && i>=1))
     stop("'i' must an alternative number.")

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
  Sigma_full = mpgi(t(delta(J,i)) %*% delta(J,i)) %*% t(delta(J,i)) %*%
    Sigma %*% delta(J,i) %*% mpgi(t(delta(J,i)) %*% delta(J,i))

  ### check if Sigma is a covariance matrix
  if(!is_covariance_matrix(Sigma_full))
    stop("Back-transformed matrix is no covariance matrix.")

  ### check if back-differencing yields differenced matrix
  Sigma_back = delta(J,i) %*% Sigma_full %*% t(delta(J,i))
  if(any(abs(Sigma_back - Sigma) > sqrt(.Machine$double.eps)))
    stop("Back-differencing failed.")

  ### return undifferenced covariance matrix
  return(Sigma_full)
}
