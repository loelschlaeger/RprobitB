#' Construct object of class \code{RprobitB_data}
#' @description
#' Function that constructs an object of class \code{RprobitB_data}.
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
#' The number (greater or equal 1) of decision makers.
#' @param T
#' The number (greater or equal 1) of choice occasions or a vector of choice
#' occasions of length \code{N} (i.e. a decision maker specific number).
#' @param J
#' The number (greater or equal 2) of choice alternatives.
#' @param P_f
#' The number of covariates connected to a fixed coefficient (can be 0).
#' @param P_r
#' The number of covariates connected to a random coefficient (can be 0).
#' @param alternatives
#' A character vector with the names of the choice alternatives.
#' @param cov_fix
#' A character vector with the names of the covariates that are connected to a
#' fixed coefficient.
#' @param cov_random
#' A character vector with the names of the covariates that are connected to a
#' random coefficient.
#' @param form
#' A formula object that is used to specify the probit model.
#' The structure is \code{choice ~ A | B | C}, where
#' \itemize{
#'   \item \code{A} are alternative and choice situation specific covariates
#'   with a generic coefficient,
#'   \item \code{B} are choice situation specific covariates with alternative
#'   specific coefficients,
#'   \item and \code{C} are alternative and choice situation specific covariates
#'   with alternative specific coefficients.
#' }
#' By default, alternative specific constants are added to the model (for all
#' except for the last alternative).
#' They can be removed by adding \code{+0} in the second spot.
#' @param re
#' A character vector of variable names of \code{form} with random effects.
#' To have random effects for the alternative specific constants, include
#' \code{"ASC"} in \code{re}.
#' @param vars
#' A list of length three, containing the three different covariate types of
#' \code{form}.
#' @param ASC
#' A boolean, determining whether alternative specific constants are included.
#' @param standardize
#' A character vector of names of covariates that get standardized.
#' Covariates of type 1 or 3 have to be addressed by
#' \code{covariate_alternative}.
#' If \code{standardize = "all"}, all covariates get standardized.
#' @param simulated
#' A boolean, if \code{TRUE} then \code{data} is simulated, otherwise
#' \code{data} is empirical.
#' @inheritParams prepare
#' @inheritParams check_parm
#' @inheritParams simulate
#' @return
#' An object of class \code{RprobitB_data}, i.e. a list with the arguments of
#' this function as elements.
#' In case of empirical data, the elements \code{parm} and \code{distr} equal
#' \code{NULL}.

RprobitB_data = function(data, choice_data, N, T, J, P_f, P_r, alternatives,
                         cov_fix, cov_random, form, re, vars, ASC, standardize,
                         simulated, parm, distr){

  ### check inputs
  stopifnot(is.list(data))
  stopifnot(is.list(vars), length(vars)==3)
  stopifnot(is.numeric(N), N%%1 == 0)
  stopifnot(is.numeric(T), T%%1 == 0)
  stopifnot(is.numeric(J), J%%1 == 0)
  stopifnot(is.numeric(P_f), P_f%%1 == 0)
  stopifnot(is.numeric(P_r), P_r%%1 == 0)
  stopifnot(Vectorize(is.character)(c(cov_fix,cov_random,standardize)))
  stopifnot(is.character(alternatives) || J != length(alternatives))
  stopifnot(inherits(form,"formula"))
  stopifnot(is.null(re) || is.character(re))
  stopifnot(is.logical(simulated))
  stopifnot(is.logical(ASC))

  ### create object of class "RprobitB_data"
  out = list("data"         = data,
             "choice_data"  = choice_data,
             "N"            = N,
             "T"            = T,
             "J"            = J,
             "P_f"          = P_f,
             "P_r"          = P_r,
             "alternatives" = alternatives,
             "cov_fix"      = cov_fix,
             "cov_random"   = cov_random,
             "form"         = form,
             "re"           = re,
             "vars"         = vars,
             "ASC"          = ASC,
             "standardize"  = standardize,
             "simulated"    = simulated,
             "parm"         = parm,
             "distr"        = distr)
  class(out) = "RprobitB_data"

  ### return object
  return(out)
}
