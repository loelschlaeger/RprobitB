#' Print method for \code{RprobitB_model}.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @param ...
#' Ignored.
#' @export

print.RprobitB_model = function(x, ...){
  cat("Fitted probit model via Bayesian estimation.\n")
  return(invisible(x))
}
