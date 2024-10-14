#' Check model formula
#'
#' @description
#' This function checks the input \code{form}.
#'
#' @param form
#' A \code{formula} object that is used to specify the model equation.
#' The structure is \code{choice ~ A | B | C}, where
#' \itemize{
#'   \item \code{choice} is the name of the dependent variable (the choices),
#'   \item \code{A} are names of alternative and choice situation specific
#'   covariates with a coefficient that is constant across alternatives,
#'   \item \code{B} are names of choice situation specific covariates with
#'   alternative specific coefficients,
#'   \item and \code{C} are names of alternative and choice situation specific
#'   covariates with alternative specific coefficients.
#' }
#'
#' Multiple covariates (of one type) are separated by a \code{+} sign.
#' By default, alternative specific constants (ASCs) are added to the model.
#' They can be removed by adding \code{+0} in the second spot.
#'
#' In the ordered probit model (\code{ordered = TRUE}), the \code{formula}
#' object has the simple structure \code{choice ~ A}. ASCs are not estimated.
#' @param re
#' A character (vector) of covariates of \code{form} with random effects.
#' If \code{re = NULL} (the default), there are no random effects.
#' To have random effects for the ASCs, include \code{"ASC"} in \code{re}.
#' @inheritParams RprobitB_data
#'
#' @return
#' A list that contains the following elements:
#' \itemize{
#'   \item The input \code{form}.
#'   \item The name \code{choice} of the dependent variable in \code{form}.
#'   \item The input \code{re}.
#'   \item A list \code{vars} of three character vectors of covariate names of
#'   the three covariate types.
#'   \item A boolean \code{ASC}, determining whether the model has ASCs.
#' }
#'
#' @seealso
#' [overview_effects()] for an overview of the model effects

check_form <- function(form, re = NULL, ordered = FALSE) {

  ### check inputs
  if (!inherits(form, "formula")) {
    stop("'form' must be of class 'formula'.", call. = FALSE)
  }
  if (!is.null(re)) {
    if (!is.character(re)) {
      stop("'re' must be a character (vector).", call. = FALSE)
    }
  }
  if (!isTRUE(ordered) && !isFALSE(ordered)) {
    stop("'ordered' must be a boolean.", call. = FALSE)
  }

  ### extract name of dependent variable
  choice <- all.vars(form)[1]

  ### build 'vars'
  vars <- trimws(strsplit(as.character(form)[3],
                          split = "|",
                          fixed = TRUE
  )[[1]])
  while (length(vars) < 3) {
    vars <- c(vars, NA)
  }
  vars <- lapply(strsplit(vars, split = "+", fixed = TRUE), trimws)

  ### build 'ASC'
  ASC <- ifelse(any(vars[[2]] %in% 0), FALSE, TRUE)
  for (i in 1:3) {
    vars[[i]] <- vars[[i]][!vars[[i]] %in% c(0, 1, NA)]
  }

  ### check the ordered case
  if (ordered) {
    vars[[2]] <- c(vars[[1]], vars[[2]], vars[[3]])
    vars[[1]] <- character()
    vars[[3]] <- character()
    re <- re[!re == "ASC"]
    ASC <- FALSE
  }

  ### match 're' with 'form'
  if (!is.null(re)) {
    for (re_element in re) {
      if (!re_element %in% c("ASC", unlist(vars))) {
        re <- setdiff(re, re_element)
        warning(
          "The covariate '", re_element,
          "' in 're' is not part of 'form' and hence ignored.",
          call. = FALSE,
          immediate. = TRUE
        )
      }
    }
  }

  ### return
  out <- list(
    "form" = form,
    "choice" = choice,
    "re" = re,
    "vars" = vars,
    "ASC" = ASC
  )
  return(out)
}
