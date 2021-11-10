#' Transform differenced to non-differenced error term covariance matrix.
#' @description
#' This function transforms the differenced error term covariance matrix
#' \code{Sigma} back to a non-differenced error term covariance matrix.
#' @param Sigma
#' An error term covariance matrix of dimension \code{J-1} x \code{J-1} which
#' was differenced with respect to alternative \code{i}.
#' @param i
#' An integer, the alternative number with respect to which \code{Sigma}
#' was differenced.
#' @return
#' A covariance matrix of dimension \code{J} x \code{J}. If this covariance
#' matrix gets differenced with respect to alternative \code{i}, the results is
#' again \code{Sigma}.
#' @keywords
#' helper

undiff_Sigma = function(Sigma, i){

  ### check inputs
  Sigma = as.matrix(Sigma)
  if(!is_covariance_matrix(Sigma))
    stop("'Sigma' is no covariance matrix.")
  J = nrow(Sigma) + 1
  if(!(length(i)==1 && is.numeric(i) && i%%1==0 && i<=J && i>=1))
    stop("'i' must an alternative number.")

  ### add zero row and column to Sigma at row and column i
  if(i == 1){
    Sigma_full = rbind(0, cbind(0, Sigma))
  } else if(i == J){
    Sigma_full = rbind(cbind(Sigma, 0), 0)
  } else {
    Sigma_full = cbind(Sigma[,1:(i-1)], 0, Sigma[,i:(J-1)])
    Sigma_full = rbind(Sigma_full[1:(i-1),], 0, Sigma_full[i:(J-1),])
  }

  ### add kernel element to make all elements non-zero
  Sigma_full = Sigma_full + 1

  ### check if 'Sigma_full' is a covariance matrix
  if(!is_covariance_matrix(Sigma_full))
    stop("Back-transformed matrix is no covariance matrix.")

  ### check if back-differencing yields differenced matrix
  Sigma_back = delta(J,i) %*% Sigma_full %*% t(delta(J,i))
  if(any(abs(Sigma_back - Sigma) > sqrt(.Machine$double.eps)))
    stop("Back-differencing failed.")

  ### return undifferenced covariance matrix
  names(Sigma_full) = create_labels_Sigma(J+1, cov_sym = TRUE)
  return(Sigma_full)
}
