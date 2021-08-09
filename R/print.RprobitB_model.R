#' Print method for \code{RprobitB_model}
#' @param x
#' An object of class \code{RprobitB_model}
#' @param ...
#' ignored
#' @export

print.RprobitB_model = function(x, ...){
  cat("Fitted probit model\n")
  return(invisible(x))
}
