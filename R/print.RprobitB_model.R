#' @export

print.RprobitB_model = function(x, ...){
  if(!is.RprobitB_model(x)) stop("wrong class")
  summary(x)
  return(invisible(x))
}
