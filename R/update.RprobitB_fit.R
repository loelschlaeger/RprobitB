#' Update and re-fit probit model
#'
#' @description
#' This function estimates a nested probit model based on a given
#' \code{RprobitB_fit} object.
#'
#' @details
#' All parameters (except for \code{object}) are optional and if not specified
#' retrieved from the specification for \code{object}.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#'
#' @param ...
#' Currently not used.
#'
#' @inheritParams prepare_data
#' @inheritParams fit_model
#'
#' @return
#' An object of class \code{RprobitB_fit}.
#'
#' @export

update.RprobitB_fit <- function(
    object, form, re, alternatives, id, idc, standardize, impute, scale, R, B,
    Q, print_progress, prior, latent_classes, ...
  ) {

  data <- prepare_data(
    form = if (missing(form)) object$data$form else form,
    choice_data = object$data$choice_data,
    re = if (missing(re)) object$data$re else re,
    alternatives = if (missing(alternatives)) object$data$alternatives else alternatives,
    id = if (missing(id)) object$data$res_var_names$id else id,
    idc = if (missing(idc)) object$data$res_var_names$idc else idc,
    standardize = if (missing(standardize)) object$data$standardize else standardize,
    impute = if (missing(impute)) "complete_cases" else impute
  )

  model <- fit_model(
    data = data,
    scale = if (missing(scale)) object$scale else scale,
    R = if (missing(R)) object$R else R,
    B = if (missing(B)) object$B else B,
    Q = if (missing(Q)) object$Q else Q,
    print_progress = if (missing(print_progress)) getOption("RprobitB_progress") else print_progress,
    prior = if (missing(prior)) NULL else prior,
    latent_classes = if (missing(latent_classes)) object$latent_classes else latent_classes
  )

  return(model)
}
