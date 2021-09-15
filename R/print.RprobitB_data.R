#' Print method for \code{RprobitB_data}
#' @param x
#' An object of class \code{RprobitB_data}
#' @param ...
#' ignored
#' @export

print.RprobitB_data = function(x, ...){
  cat(ifelse(x$simulated,"Simulated","Empirical"),
      "data of",sum(x$T),"choices.\n")
  return(invisible(x))
}
