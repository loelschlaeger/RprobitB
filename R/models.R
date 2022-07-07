#' Model for binary choice between Train trip alternatives
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
#'   scale = "price := -1")
#' )
#' }
#'
#' @format An object of class \code{RprobitB_fit}.
#'
#' @keywords
#' model
"model_train"

#' Model for for binary choice between Train trip alternatives with the
#' price as the only explanatory variable
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

#' Model for multivariate choice between electricity suppliers
#'
#' @description
#' This object is a fitted mixed probit model to the Electricity dataset of the
#' {mlogit} package with the model formula
#' `choice ~ pf + cl + loc + wk + tod + seas | 0`.
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
#'   scale = "pf := -1"
#' )
#' }
#'
#' @format An object of class \code{RprobitB_fit}.
#'
#' @keywords
#' model
"model_elec"

#' Model for binary berserking choice
#'
#' @description
#' This object is a fitted mixed probit model to the \code{choice_berserk}
#' dataset of the {RprobitB} package with the formula
#' `berserk ~ 0 | white + rating + rating_diff + min_rem + streak + berserk.1 + lost.1 + 1`.
#'
#' @usage data(model_berserk)
#'
#' @details
#' The model was estimated via
#' \preformatted{
#' choice_berserk <- create_lagged_cov(
#'   choice_data = RprobitB::choice_berserk,
#'   column = c("berserk","lost"),
#'   k = 1,
#'   id = "player_id"
#' )
#'
#' data_berserk <- prepare_data(
#'   form = berserk ~ 0 | white + rating + rating_diff + min_rem + streak + berserk.1 + lost.1 + 1,
#'   re = c("rating_diff","lost.1"),
#'   choice_data = choice_berserk,
#'   id = "player_id",
#'   idc = "game_id",
#'   standardize = c("rating","rating_diff","min_rem"),
#'   impute = "zero"
#' )
#' model_berserk <- fit_model(
#'   data_berserk,
#'   latent_classes = list("dp_update" = TRUE, "C" = 10),
#'   R = 5000
#' )
#' }
#'
#' @format An object of class \code{RprobitB_fit}.
#'
#' @keywords
#' model
"model_berserk"

#' Model for chess opening choice
#'
#' @description
#' This object is a fitted mixed probit model to the \code{choice_chess_opening}
#' dataset of the {RprobitB} package with the formula
#' `w1 ~ w1.1 | sex_w + byear_w + rating_w + rating_diff + 1`.
#'
#' @usage data(model_chessop)
#'
#' @details
#' The model was estimated via
#' \preformatted{
#' choice_chess_opening <- create_lagged_cov(
#'   choice_data = RprobitB::choice_chess_opening,
#'   column = "w1",
#'   k = 1,
#'   id = "fideid_w")
#' choice_chess_opening <- fastDummies::dummy_cols(choice_chess_opening, "w1.1")
#' choice_chess_opening$rating_diff <- choice_chess_opening$rating_w - choice_chess_opening$rating_b
#' data <- prepare_data(
#'   form = w1 ~ w1.1 | sex_w + byear_w + rating_w + rating_diff + 1,
#'   choice_data = choice_chess_opening,
#'   id = "fideid_w",
#'   idc = "fideid_b",
#'   alternatives = c("e4","d4","b3"),
#'   standardize = c("byear_w","rating_w","rating_diff"),
#'   impute = "complete_cases"
#' )
#' model_chessop <- fit_model(
#'   data = data,
#'   re = "w1.1",
#'   latent_classes = list("dp_update" = TRUE, "C" = 2)
#' )
#' }
#'
#' @format An object of class \code{RprobitB_fit}.
#'
#' @keywords
#' model
"model_chessop"
