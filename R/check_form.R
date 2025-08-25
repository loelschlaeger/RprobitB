#' Check model formula
#'
#' @description
#' This function checks the input \code{form}.
#'
#' @param form \[`formula`\]\cr
#' A model description with the structure \code{choice ~ A | B | C}, where
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
#'
#' @param re \[`character()` | `NULL`\]\cr
#' Names of covariates with random effects.
#' If \code{re = NULL} (the default), there are no random effects.
#' To have random effects for the ASCs, include \code{"ASC"} in \code{re}.
#'
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
  oeli::input_check_response(
    check = oeli::check_missing(form),
    var_name = "form"
  )
  oeli::input_check_response(
    check = checkmate::check_formula(form),
    var_name = "form"
  )
  oeli::input_check_response(
    check = checkmate::check_character(re, null.ok = TRUE),
    var_name = "re"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(ordered),
    var_name = "ordered"
  )

  ### extract name of dependent variable
  choice <- all.vars(form)[1]

  ### build 'vars'
  vars <- strsplit(as.character(form)[3], split = "|", fixed = TRUE)[[1]] |>
    trimws()
  while (length(vars) < 3) vars <- c(vars, NA)
  vars <- lapply(strsplit(vars, split = "+", fixed = TRUE), trimws)

  ### build 'ASC'
  ASC <- ifelse(any(vars[[2]] %in% 0), FALSE, TRUE)
  for (i in 1:3) vars[[i]] <- vars[[i]][!vars[[i]] %in% c(0, 1, NA)]

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
  list("form" = form, "choice" = choice, "re" = re, "vars" = vars, "ASC" = ASC)
}
