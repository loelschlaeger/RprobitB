#' Check covariance matrix properties.
#' @description
#' This function checks if the input is a proper covariance matrix, i.e.
#' a symmetric, numeric matrix with non-negative eigenvalues.
#' @param x
#' A matrix.
#' @return
#' A boolean, \code{TRUE} if \code{x} is a proper covariance matrix.

is_covariance_matrix = function(x){
  is.matrix(x) &&
    is.numeric(x) &&
    ncol(x)==nrow(x) &&
    isSymmetric(x) &&
    all(eigen(x)$value > -sqrt(.Machine$double.eps))
}
