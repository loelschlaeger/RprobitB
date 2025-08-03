#' Extract number of model parameters
#'
#' @description
#' This function extracts the number of model parameters of an
#' \code{RprobitB_fit} object.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#'
#' @param ...
#' Optionally more objects of class \code{RprobitB_fit}.
#'
#' @return
#' Either a numeric value (if just one object is provided) or a numeric vector.
#'
#' @export

npar <- function(object, ...) {
  UseMethod("npar")
}

#' @exportS3Method
#' @rdname npar

npar.RprobitB_fit <- function(object, ...) {
  models <- list(...)
  if (length(models) == 0) {
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  npar <- sapply(models, function(mod) {
    mod$data$P_f +
      (mod$data$P_r + (mod$data$P_r * (mod$data$P_r + 1) / 2)) * mod$latent_classes$C +
      mod$data$J * (mod$data$J - 1) / 2 - 1
  })
  return(npar)
}
