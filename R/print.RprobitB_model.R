#' @export

print.RprobitB_model = function(x, ...){

  if(!is.RprobitB_model(x))
    stop("Not of class 'RprobitB_model'.")

  cat("RprobitB_model\n")

  invisible(x)
}
