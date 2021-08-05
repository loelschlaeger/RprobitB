#' Transform fitted \code{RprobitB_model}
#' @description
#' Function that can transform a fitted \code{RprobitB_model} in three ways:
#' \itemize{
#'   \item Change the length \code{B} of the burn-in period
#'   \item Change the thinning factor \code{Q}
#'   \item Change the utility normalization \code{scale}
#' }
#' @details
#' For more details see the vignette "Model fitting":
#' \code{vignette("model_fitting", package = "RprobitB")}
#' @inheritParams fit
#' @param RprobitB_model
#' The output of \code{\link{fit}}.
#' @export

transform = function(RprobitB_model, B = NULL, Q = NULL, scale = NULL) {



}
