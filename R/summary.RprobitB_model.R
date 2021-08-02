#' For more details see the vignette "Model results":
#' \code{vignette("model_results", package = "RprobitB")}
#' @export

summary.RprobitB_model = function(object, ...) {

  if(!is.RprobitB_model(object))
    stop("Not of class 'RprobitB_model'.")

  ans = object[c("R","B","Q")]

  class(ans) = "summary.RprobitB_model"

  ans
}
