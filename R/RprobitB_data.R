#' Create object of class \code{RprobitB_data}.
#'
#' @description
#' This function creates an object of class \code{RprobitB_data}.
#'
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
#' If not specified, the choice set is defined by the observed choices.
#' @param ASC
#' A boolean, determining whether the model has ASCs.
#' @param linear_coeffs
#' A data frame with the coefficient names and booleans indicating whether
#' they are connected to random effects.
#' @param standardize
#' A character vector of names of covariates that get standardized.
#' Covariates of type 1 or 3 have to be addressed by
#' \code{<covariate>_<alternative>}.
#' If \code{standardize = "all"}, all covariates get standardized.
#' @param simulated
#' A boolean, if \code{TRUE} then \code{data} is simulated, otherwise
#' \code{data} is empirical.
#' @param choice_available
#' A boolean, if \code{TRUE} then \code{data} contains observed choices.
#' @inheritParams check_form
#' @inheritParams prepare_data
#' @param true_parameter
#' An object of class \code{RprobitB_parameters}.
#' @param res_var_names
#' A names list of reserved variable names in \code{choice_data}.
#'
#' @return
#' An object of class \code{RprobitB_data} with the arguments of this function
#' as elements.
#'
#' @keywords
#' s3

RprobitB_data <- function(data, choice_data, N, T, J, P_f, P_r, alternatives,
                          form, re, ASC, linear_coeffs, standardize, simulated,
                          choice_available, true_parameter, res_var_names) {

  ### check inputs
  stopifnot(is.list(data))
  stopifnot(is.numeric(N), N %% 1 == 0)
  stopifnot(is.numeric(T), T %% 1 == 0)
  stopifnot(is.numeric(J), J %% 1 == 0)
  stopifnot(is.numeric(P_f), P_f %% 1 == 0)
  stopifnot(is.numeric(P_r), P_r %% 1 == 0)
  stopifnot(is.character(alternatives) || J != length(alternatives))
  stopifnot(inherits(form, "formula"))
  stopifnot(is.logical(simulated))
  stopifnot(is.logical(choice_available))
  if (!is.null(true_parameter)) {
    stopifnot(class(true_parameter) == "RprobitB_parameter")
  }

  ### create and return object of class "RprobitB_data"
  out <- list(
    "data" = data,
    "choice_data" = choice_data,
    "N" = N,
    "T" = T,
    "J" = J,
    "P_f" = P_f,
    "P_r" = P_r,
    "alternatives" = alternatives,
    "form" = form,
    "re" = re,
    "ASC" = ASC,
    "linear_coeffs" = linear_coeffs,
    "standardize" = standardize,
    "choice_available" = choice_available,
    "simulated" = simulated,
    "true_parameter" = true_parameter,
    "res_var_names" = res_var_names
  )
  class(out) <- "RprobitB_data"
  return(out)
}

#' @noRd
#' @export

print.RprobitB_data <- function(x, ...) {
  cat(
    ifelse(x$simulated, "Simulated", "Empirical"),
    "data of", sum(x$T), "choices.\n"
  )
  return(invisible(x))
}

#' @noRd
#' @export

summary.RprobitB_data <- function(object, ...) {

  ### check class of 'object'
  if (!inherits(object, "RprobitB_data")) {
    stop("Not of class 'RprobitB_data'.")
  }

  ### summary of alternatives
  alt <- data.frame(matrix(NA, nrow = 0, ncol = 1))
  colnames(alt) <- "frequency"
  for (i in object$alternatives) {
    alt[nrow(alt) + 1, ] <-
      sum(unlist(lapply(object$data, function(x) x[["y"]])) == i)
    rownames(alt)[nrow(alt)] <- i
  }

  ### build 'summary.RprobitB_data' object
  out <- list(
    "simulated" = object$simulated,
    "N" = object$N,
    "T" = object$T,
    "linear_coeffs" = object$linear_coeffs,
    "alternatives" = alt
  )
  class(out) <- "summary.RprobitB_data"

  ### return 'summary.RprobitB_data' object
  return(out)
}


#' @export
#' @noRd

print.summary.RprobitB_data <- function(x, ...) {
  cat(
    "Summary of", ifelse(x$simulated, "simulated", "empirical"),
    "choice data\n\n"
  )

  ### summary of decision makers
  cat(x$N, paste0("decision maker", ifelse(x$N == 1, "", "s")), "\n")
  if (length(unique(x$T)) == 1) {
    cat(
      x$T[1], paste0("choice occasion", ifelse(unique(x$T) == 1, "", "s")),
      ifelse(x$N == 1, "", "each"), "\n"
    )
  }
  if (length(unique(x$T)) > 1) {
    cat(
      min(x$T), "to", max(x$T), "choice occasions",
      ifelse(x$N == 1, "", "each"), "\n"
    )
  }
  cat(sum(x$T), "choices in total\n")
  cat("\n")

  ### summary of alternatives
  cat("Alternatives\n")
  print(x$alternatives)
  cat("\n")

  ### summary of covariates
  cat("Linear coefficients\n")
  print(x$linear_coeffs)
  return(invisible(x))
}
