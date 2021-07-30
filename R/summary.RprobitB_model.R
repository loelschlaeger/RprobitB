#' @export

summary.RprobitB_model = function(object, ...) {

  if(!is.RprobitB_model(object)) stop("wrong class")

  cat("R =",object$R)

  return(invisible(object))
}
