#' Check for covariance matrix
#' @description
#' Function that checks if the input is a covariance matrix.
#' @param x
#' A matrix.
#' @return
#' \code{TRUE} if \code{x} is a covariance matrix, \code{FALSE} otherwise

is.covariance.matrix = function(x){
  stopifnot(is.matrix(x))
  return(ncol(x)==nrow(x) && isSymmetric(x) && all(eigen(x)$value>=0))
}
