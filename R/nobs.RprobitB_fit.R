#' @exportS3Method

nobs.RprobitB_fit <- function(object, ...) {
  sum(object$data$T)
}
