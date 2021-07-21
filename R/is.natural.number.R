#' Check for natural number
#' @description
#' Function that checks if the input is a natural number, i.e. a non-negative
#' integer.
#' @param x
#' A numeric value.
#' @return
#' \code{TRUE} if \code{x} is non-negative and has no fractional component,
#' \code{FALSE} otherwise

is.natural.number = function(x){
  stopifnot(is.numeric(x))
  return(x >= 0 && x%%1 == 0)
}
