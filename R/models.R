#' Probit model for binary choice between Train trip alternatives
#'
#' @description
#' This object is a fitted probit model to the Train data set of the {mlogit}
#' package with the model formula `choice ~ price + time + change + comfort | 0`.
#'
#' @usage data(model_train)
#'
#' @details
#' The model was estimated via
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
#' )
#' model_train <- fit_model(
#'   data = data,
#'   R = 10000,
#'   Q = 10,
#'   scale = price ~ -1)
#' )
#' }
#'
#' @format An object of class \code{RprobitB_fit}.
#'
#' @keywords
#' model
"model_train"

#' Probit model for binary choice between Train trip alternatives with the price
#' as the only explanatory variable
#'
#' @description
#' This object is a fitted probit model to the Train data set of the {mlogit}
#' package with the model formula `choice ~ price | 0`.
#'
#' @usage data(model_train_sparse)
#'
#' @details
#' The model was derived from the \code{\link{model_train}} object via
#' \preformatted{
#' data("model_train", package = "RprobitB")
#' model_train_sparse <- update(
#'   model_train,
#'   form = choice ~ price | 0
#' )
#' }
#'
#' @format An object of class \code{RprobitB_fit}.
#'
#' @keywords
#' model
"model_train_sparse"

#' Mixed probit model for multivariate choice between electricity suppliers
#'
#' @description
#' This object is a fitted mixed probit model to the Electricity dataset of the
#' {mlogit} package with the model formula `choice ~ pf + cl + loc + wk + tod + seas | 0`.
#'
#' @usage data(model_elec)
#'
#' @details
#' The model was estimated via
#' \preformatted{
#' data("Electricity", package = "mlogit")
#' Electricity <- as_cov_names(Electricity, c("pf","cl","loc","wk","tod","seas"), 1:4)
#' data <- prepare_data(
#'   form = choice ~ pf + cl + loc + wk + tod + seas | 0,
#'   choice_data = Electricity,
#'   re = c("cl","loc","wk","tod","seas")
#' )
#' model_elec <- fit_model(
#'   data = data,
#'   R = 5000,
#'   Q = 10,
#'   scale = pf ~ -1
#' )
#' }
#'
#' @format An object of class \code{RprobitB_fit}.
#'
#' @keywords
#' model
"model_elec"
