#' Difference operator.
#' @description
#' This function computes the difference operator matrix for computing utility
#' differences.
#' @details
#' Given a \code{J} x \code{P} matrix \code{X} of choice characteristics,
#' then \code{delta(i,J)%*%X} computes differences with respect to alternative
#' \code{i}.
#' @param J
#' The total number of alternatives.
#' @param i
#' The alternative number to which respect utility differences are computed.
#' @return
#' A matrix of dimension \code{J-1} x \code{J}.
#' @examples
#' X = matrix(1:9,3,3, byrow = TRUE)
#' delta(3,3) %*% X

delta = function(J,i){
  stopifnot(is.numeric(J), J%%1 == 0, J>=2)
  stopifnot(is.numeric(i), i%%1 == 0, i>=1)
  stopifnot(J>=2)
  stopifnot(J>=i)
  if(i == 1){
    Delta = cbind(-1, diag(J-1))
  } else if (i == J) {
    Delta = cbind(diag(J-1), -1)
  } else {
    Delta = cbind(diag(J-1)[,1:(i-1)],-1,diag(J-1)[,i:(J-1)])
  }
  return(Delta)
}
