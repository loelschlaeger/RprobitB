#' Print method for \code{RprobitB_parameter}.
#' @param x
#' An object of class \code{RprobitB_parameter}.
#' @param ...
#' Ignored.
#' @export
#' x = RprobitB_parameter(P_f = 1, P_r = 2, J = 3, N = 100, C = 2)

print.RprobitB_parameter = function(x) {
  if(!class(x) == "RprobitB_parameter")
    stop("'x' is not of class 'RprobitB_parameter'.")
  cat("RprobitB_parameter, contains values for:\n")
  for(par_name in names(x)){
    par = x[[par_name]]
    if(!is.null(par) && !anyNA(par)){
      cat("*",par_name,"\n")
    }
  }
}
