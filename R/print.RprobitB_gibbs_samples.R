#' Print method for \code{RprobitB_gibbs_samples}.
#' @param x
#' An object of class \code{RprobitB_gibbs_samples}.
#' @param ...
#' Ignored.
#' @export

print.RprobitB_gibbs_samples = function(x, ...){
  cat("Gibbs samples.\n")
  return(invisible(x))
}
