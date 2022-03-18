#' Probit model for binary choice between Train trip alternatives
#'
#' @description
#' This object is a fitted probit model to the Train dataset of the {mlogit}
#' package with the model formula `choice ~ price + time + change + comfort | 0`.
#'
#' @usage data(model_train)
#'
#' @details
#' The model was derived via
#' \preformatted{
#' data("Train", package = "mlogit")
#' Train$price_A <- Train$price_A / 100 * 2.20371
#' Train$price_B <- Train$price_B / 100 * 2.20371
#' Train$time_A <- Train$time_A / 60
#' Train$time_B <- Train$time_B / 60
#' form <- choice ~ price + time + change + comfort | 0
#' data <- prepare_data(
#'   form = form,
#'   choice_data = Train,
#'   idc = "choiceid"
#'   )
#' model_train <- mcmc(
#'   data = data,
#'   scale = list("parameter" = "a", index = 1, value = -1)
#'   )
#' }
#'
#' @format An object of class \code{RprobitB_fit}.
#'
#' @keywords
#' model
"model_train"

#' Mixed probit model for multivariate choice between electricity suppliers
#'
#' @description
#' This object is a fitted mixed probit model to the Electricity dataset of the
#' {mlogit} package with the model formula `choice ~ pf + cl + loc + wk + tod + seas | 0`.
#'
#' @usage data(model_elec)
#'
#' @details
#' The model was derived via
#' \preformatted{
#' data("Electricity", package = "mlogit")
#' Electricity <- as_cov_names(Electricity, c("pf","cl","loc","wk","tod","seas"), 1:4)
#' data <- prepare_data(
#'   form = choice ~ pf + cl + loc + wk + tod + seas | 0,
#'   choice_data = Electricity,
#'   re = c("cl","loc","wk","tod","seas")
#' )
#' model <- mcmc(data, R = 1000, scale = list(parameter = "a", index = 1, value = -1))
#' }
#'
#' The Gibbs samples for `beta` were removed to reduce the required memory space.
#'
#' @format An object of class \code{RprobitB_fit}.
#'
#' @keywords
#' model
"model_elec"


#' An example output of `model_selection()`.
#'
#' @keywords
#' internal
"mod_sel"
