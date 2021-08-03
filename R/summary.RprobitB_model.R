#' @export

summary.RprobitB_model = function(object, ...) {

  if(!is.RprobitB_model(object))
    stop("Not of class 'RprobitB_model'.")

  ### build 'summary.RprobitB_model' object
  out = list()
  class(out) = "summary.RprobitB_model"

  ### return 'summary.RprobitB_model' object
  return(out)
}
