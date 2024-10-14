#' Compute prediction accuracy
#'
#' @description
#' This function computes the prediction accuracy of an \code{RprobitB_fit}
#' object. Prediction accuracy means the share of choices that are correctly
#' predicted by the model, where prediction is based on the maximum choice
#' probability.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#' @param ...
#' Optionally specify more \code{RprobitB_fit} objects.
#'
#' @return
#' A numeric.
#'
#' @export

pred_acc <- function(x, ...) {
  models <- list(...)
  if (length(models) == 0) {
    models <- list(x)
  } else {
    models <- c(list(x), models)
  }
  pa <- sapply(models, function(x) {
    conf <- predict.RprobitB_fit(x, data = NULL, overview = TRUE)
    sum(diag(conf)) / sum(conf)
  })
  return(pa)
}
