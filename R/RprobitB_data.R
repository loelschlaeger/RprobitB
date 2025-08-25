#' Create object of class \code{RprobitB_data}
#'
#' @description
#' This function constructs an object of class \code{RprobitB_data}.
#'
#' @param data \[`list`\]\cr
#' A list with the choice data.
#'
#' * The list has \code{N} elements.
#'
#' * Each element is a list with two elements, \code{X} and \code{y}, which are
#' the covariates and decisions for a decision maker. More precisely:
#'
#'    - \code{X} is a list of \code{T} elements, where each element is a matrix
#'    of dimension \code{J}x(\code{P_f}+\code{P_r}) and contains the
#'    characteristics for one choice occasion.
#'
#'    - \code{y} is a vector of length \code{T} and contains the labels for the
#'    chosen alternatives.
#'
#' @param N \[`integer(1)`\]\cr
#' The number of decision makers.
#'
#' @param T \[`integer(1)` | `integer(N)`\]\cr
#' The number of choice occasions or a vector of decider-specific choice
#' occasions of length \code{N}.
#'
#' @param J \[`integer(1)`\]\cr
#' The number >= 2 of choice alternatives.
#'
#' @param P_f \[`integer(1)`\]\cr
#' The number of covariates connected to a fixed coefficient.
#'
#' @param P_r \[`integer(2)`\]\cr
#' The number of covariates connected to a random coefficient.
#'
#' @param alternatives \[`character()`\]\cr
#' The names of the choice alternatives. If not specified, the choice set is
#' defined by the observed choices.
#'
#' If \code{ordered = TRUE}, \code{alternatives} is assumed to be specified with
#' the alternatives ordered from worst to best.
#'
#' @param ordered \[`logical(1)`\]\cr
#' If \code{TRUE}, the choice set \code{alternatives} is assumed to be ordered
#' from worst to best.
#'
#' @param ranked \[`logical(1)`\]\cr
#' Are the alternatives ranked?
#'
#' @param base \[`character(1)`\]\cr
#' The name of the base alternative for covariates that are not
#' alternative specific (i.e. type 2 covariates and ASCs).
#'
#' Ignored and set to \code{NULL} if the model has no alternative specific
#' covariates (e.g. in the ordered probit model).
#'
#' By default, \code{base} is the last element of \code{alternatives}.
#'
#' @param ASC \[`logical(1)`\]\cr
#' Does the model have ASCs?
#'
#' @param effects \[`data.frame`\]\cr
#' A data frame with the effect names and booleans indicating whether
#' they are connected to random effects.
#'
#' @param standardize \[`character()` | `"all"`\]\cr
#' Names of covariates that get standardized.
#'
#' Covariates of type 1 or 3 have to be addressed by
#' \code{<covariate>_<alternative>}.
#'
#' If \code{standardize = "all"}, all covariates get standardized.
#'
#' @param simulated \[`logical(1)`\]\cr
#' Is \code{data} simulated?
#'
#' @param choice_available \[`logical(1)`\]\cr
#' Does \code{data} contain observed choices?
#'
#' @param true_parameter \[`RprobitB_parameters`\]\cr
#' True parameters for the data generating process.
#'
#' @param res_var_names \[`list`\]\cr
#' Reserved variable names in \code{choice_data}.
#'
#' @inheritParams check_form
#' @inheritParams prepare_data
#'
#' @return
#' An object of class \code{RprobitB_data}.
#'
#' @keywords internal

RprobitB_data <- function(
    data, choice_data, N, T, J, P_f, P_r, alternatives, ordered, ranked, base,
    form, re, ASC, effects, standardize, simulated, choice_available,
    true_parameter, res_var_names
) {

  ### check inputs
  stopifnot(is.list(data))
  stopifnot(is.numeric(N), N %% 1 == 0)
  stopifnot(is.numeric(T), T %% 1 == 0)
  stopifnot(is.numeric(J), J %% 1 == 0)
  stopifnot(is.numeric(P_f), P_f %% 1 == 0)
  stopifnot(is.numeric(P_r), P_r %% 1 == 0)
  stopifnot(is.character(alternatives) || J != length(alternatives))
  stopifnot(is.character(alternatives), base %in% alternatives)
  stopifnot(is.logical(ordered))
  stopifnot(is.logical(ranked))
  stopifnot(is.null(base) || (is.character(base) && length(base) == 1))
  stopifnot(inherits(form, "formula"))
  stopifnot(is.logical(simulated))
  stopifnot(is.logical(choice_available))
  if (!is.null(true_parameter)) {
    stopifnot(inherits(true_parameter, "RprobitB_parameter"))
  }

  ### create and return object of class "RprobitB_data"
  structure(
    list(
      "data" = data,
      "choice_data" = choice_data,
      "N" = N,
      "T" = T,
      "J" = J,
      "P_f" = P_f,
      "P_r" = P_r,
      "alternatives" = alternatives,
      "ordered" = ordered,
      "ranked" = ranked,
      "base" = base,
      "form" = form,
      "re" = re,
      "ASC" = ASC,
      "effects" = effects,
      "standardize" = standardize,
      "choice_available" = choice_available,
      "simulated" = simulated,
      "true_parameter" = true_parameter,
      "res_var_names" = res_var_names
    ),
    class = "RprobitB_data"
  )
}

#' @rdname RprobitB_data
#' @export

print.RprobitB_data <- function(x, ...) {
  cat(
    ifelse(x$simulated, "Simulated", "Empirical"),
    "data of", sum(x$T),
    if (x$ordered) "(ordered)",
    if (x$ranked) "(ranked)",
    "choices.\n"
  )
  invisible(x)
}

#' @rdname RprobitB_data
#' @export

summary.RprobitB_data <- function(object, ...) {
  ### check class of 'object'
  if (!inherits(object, "RprobitB_data")) {
    stop("Not of class 'RprobitB_data'.", call. = FALSE
    )
  }

  ### alternative frequency
  alt_freq <- data.frame(matrix(NA_integer_, nrow = 0, ncol = 1))
  colnames(alt_freq) <- "frequency"
  if (object$ranked) {
    choice_set <- sapply(
      oeli::permutations(object$alternatives), paste, collapse = ","
    )
  } else {
    choice_set <- object$alternatives
  }

  for (i in choice_set) {
    alt_freq[nrow(alt_freq) + 1, ] <-
      sum(unlist(lapply(object$data, function(x) x[["y"]])) == i)
    rownames(alt_freq)[nrow(alt_freq)] <- i
  }

  ### build 'summary.RprobitB_data' object
  structure(
    list(
      "simulated" = object$simulated,
      "N" = object$N,
      "T" = object$T,
      "form" = object$form,
      "re" = object$re,
      "effects" = object$effects,
      "alt_freq" = alt_freq
    ),
    class = "summary.RprobitB_data"
  )
}


#' @rdname RprobitB_data
#' @exportS3Method

print.summary.RprobitB_data <- function(x, ...) {
  overview <- data.frame(
    c(
      x$N,
      ifelse(length(unique(x$T)) == 1, x$T[1], paste0(min(x$T), "-", max(x$T))),
      sum(x$T),
      nrow(x$alt_freq),
      x$alt_freq$frequency
    )
  )
  rownames(overview) <- c(
    "deciders", "choice occasions", "total choices",
    "alternatives", paste0("- '", rownames(x$alt_freq), "'")
  )
  colnames(overview) <- c("count")
  print(overview)
  return(invisible(x))
}
