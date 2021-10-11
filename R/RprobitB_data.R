#' Create object of class \code{RprobitB_data}.
#' @description
#' This function creates an object of class \code{RprobitB_data}.
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
#' @param vars
#' The element \code{vars} in the output of \link{check_form}.
#' @param ASC
#' A boolean, determining whether the model has ASCs.
#' @param standardize
#' A character vector of names of covariates that get standardized.
#' Covariates of type 1 or 3 have to be addressed by
#' \code{covariate_alternative}.
#' If \code{standardize = "all"}, all covariates get standardized.
#' @param simulated
#' A boolean, if \code{TRUE} then \code{data} is simulated, otherwise
#' \code{data} is empirical.
#' @inheritParams check_form
#' @inheritParams prepare
#' @inheritParams simulate
#' @params true_parameter
#' An object of class \code{RprobitB_parameters}.
#' @return
#' An object of class \code{RprobitB_data}, which is a list of
#' \itemize{
#'   \item the arguments of this functions as elements, and
#'   \item the element \code{covs}, which is a data frame of the two
#'   columns \code{name} (the covariate names) and \code{random} (a boolean
#'   determining whether the covariates are random effects).
#' }

RprobitB_data = function(data, choice_data, N, T, J, P_f, P_r, alternatives,
                         form, re, vars, ASC, standardize, simulated, distr,
                         true_parameter){

  ### check inputs
  stopifnot(is.list(data))
  stopifnot(is.list(vars), length(vars)==3)
  stopifnot(is.numeric(N), N%%1 == 0)
  stopifnot(is.numeric(T), T%%1 == 0)
  stopifnot(is.numeric(J), J%%1 == 0)
  stopifnot(is.numeric(P_f), P_f%%1 == 0)
  stopifnot(is.numeric(P_r), P_r%%1 == 0)
  stopifnot(is.character(alternatives) || J != length(alternatives))
  stopifnot(inherits(form,"formula"))
  stopifnot(is.null(re) || is.character(re))
  stopifnot(is.logical(simulated))
  stopifnot(is.logical(ASC))
  stopifnot(is.null(true_parameter) || inherits(true_parameter,"RprobitB_parameter"))

  ### create data frame of covariate names
  cov_names = colnames(data[[1]][["X"]][[1]])
  covs = data.frame("names" = colnames(data[[1]][["X"]][[1]]),
                    "random" = FALSE)
  covs[which(gsub("_.*$","",cov_names) %in% re),"random"] = TRUE

  ### create and return object of class "RprobitB_data"
  out = list("data"           = data,
             "choice_data"    = choice_data,
             "N"              = N,
             "T"              = T,
             "J"              = J,
             "P_f"            = P_f,
             "P_r"            = P_r,
             "alternatives"   = alternatives,
             "covs"           = covs,
             "form"           = form,
             "re"             = re,
             "vars"           = vars,
             "ASC"            = ASC,
             "standardize"    = standardize,
             "simulated"      = simulated,
             "distr"          = distr,
             "true_parameter" = true_parameter)
  class(out) = "RprobitB_data"
  return(out)
}
