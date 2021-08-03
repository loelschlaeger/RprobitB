#' @export

print.RprobitB_data = function(x, ...){
  cat(ifelse(x$simulated,"Simulated","Empirical"),"choice data\n")
  return(invisible(x))
}
