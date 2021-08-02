#' @export

print.RprobitB_data = function(x, ...){

  if(!is.RprobitB_data(x))
    stop("Not of class 'RprobitB_data'.")

  cat("RprobitB_data")

  invisible(x)

}
