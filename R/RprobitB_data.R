#' Construct object of class "RprobitB_data"
#' @description
#' Function that constructs an object of class "RprobitB_data".
#' @param data
#' A list with the choice data.
#' The list has \code{N} elements.
#' Each element is a list with two elements, \code{X} and \code{y}, which are
#' the covariates and decisions for a decision maker. More precisely,
#' \code{X} is a list of \code{T} elements, where each element is a matrix of
#' dimension \code{J}x(\code{P_f}+\code{P_r}) and contains the characteristics
#' for one choice occasion.
#' \code{y} is a vector of length \code{T} and contains the labels for the
#' chosen alternatives.
#' @param N
#' The non-negative number of decision makers.
#' @param T
#' The non-negative number of choice occasions.
#' @param J
#' The non-negative number of choice alternatives.
#' @param P_f
#' The non-negative number of covariates connected to a fixed coefficient.
#' @param P_r
#' The non-negative number of covariates connected to a random coefficient.
#' @param cov_fix
#' A character vector with the names of covariates that are connected to a
#' fixed coefficient.
#' @param cov_random
#' A character vector with the names of covariates that are connected to a
#' random coefficient.
#' @param form
#' A formula object that is used to specify the probit model.
#' The structure is \code{choice ~ A | B | C}, where
#' \code{A} are alternative and choice situation specific covariates with a
#' generic coefficient,
#' \code{B} are choice situation specific covariates with alternative specific
#' coefficients,
#' and \code{C} are alternative and choice situation specific covariates with
#' alternative specific coefficients.
#' By default, alternative specific constants are added to the model (for all
#' except for the last alternative).
#' They can be removed by adding \code{+0} in the second spot.
#' @param standardize
#' A character vector of variable names of \code{form} that get standardized.
#' @inheritParams check_parm
#' @param distr
#' A character vector of number generation functions from which the covariates
#' are drawn. Possible inputs are
#' \itemize{
#'   \item functions of the type \code{r*} from base R (e.g. \code{rnorm})
#'         where all required inputs (except for \code{n}) must be specified,
#'   \item the function \code{sample}, where all required inputs
#'         (except for \code{size}) must be specified.
#' }
#' The order is the same as in \code{form}, i.e. the first element of
#' \code{distr} draws the first covariate of \code{form}.
#' If \code{distr} is not specified, all covariates are drawn from a
#' standard normal distribution.
#' If \code{distr} is specified, the length of \code{distr} must equal the
#' number of covariates.
#' @return
#' An object of class "RprobitB_data", i.e. a list with the arguments of this
#' function as elements.
#' In case of empirical data, the elements \code{parm} and \code{distr} equal
#' \code{NULL}.

RprobitB_data = function(data, N, T, J, P_f, P_r, cov_fix, cov_random, form,
                         standardize, parm, distr){

  ### check input types
  stopifnot(is.list(data))
  stopifnot(is.list(parm) || is.null(parm))
  stopifnot(is.list(distr) || is.null(distr))
  stopifnot(Vectorize(is.natural.number)(c(N,T,J,P_f,P_r)))
  stopifnot(Vectorize(is.character)(c(cov_fix,cov_random,standardize)))
  stopifnot(inherits(form,"formula"))

  ### create object of class "RprobitB_data"
  out = list("data"        = data,
             "N"           = N,
             "T"           = T,
             "J"           = J,
             "P_f"         = P_f,
             "P_r"         = P_r,
             "cov_fix"     = cov_fix,
             "cov_random"  = cov_random,
             "form"        = form,
             "standardize" = standardize,
             "parm"        = parm,
             "distr"       = distr)
  class(out) = "RprobitB_data"

  ### return object
  return(out)
}
