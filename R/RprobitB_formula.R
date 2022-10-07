#' Constructor for \code{RprobitB_formula} object
#'
#' @description
#' This function constructs an S3 object of class \code{RprobitB_formula}.
#'
#' @param formula
#' An object of class \code{\link[stats]{formula}}:
#' a symbolic description of the model to be fitted.
#' The details of model specification are given under \sQuote{Details}.
#'
#' @param re
#' A character (vector): the names of covariates in \code{formula} that should
#' have a (log-) normal mixing distribution.
#' The details of random effects are given under \sQuote{Details}.
#'
#' @inheritParams RprobitB_data
#'
#' @return
#' An object of class \code{RprobitB_formula}.
#'
#' @details
#' # Details of model specification
#' The structure of \code{formula} should be
#' \code{choice ~ A | B | C}, where
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
#' Multiple covariates of one type are separated by a \code{+} sign.
#' By default, alternative specific constants (ASCs) are added to the model.
#' They can be removed by adding \code{+ 0} in the second spot.
#'
#' # Model specification in the ordered case
#' In the ordered probit model (\code{ordered = TRUE}), covariates cannot be
#' alternative specific. Therefore, the \code{formula} object does need any
#' separation via \code{|} anymore, and has the simple structure
#' \code{choice ~ A + B + C}. ASCs cannot be estimated in the ordered case.
#'
#' # Details of random effects
#' TODO
#'
#' @examples
#' formula <- choice ~ A | B + 0 | C + D
#' re <- c("A", "D+")
#' new_RprobitB_formula(formula = formula, re = re, ordered = FALSE)

new_RprobitB_formula <- function(
    formula = choice ~ covariate, re = character(), ordered = FALSE
  ) {
  stopifnot(inherits(formula, "formula"))
  stopifnot(is.character(re))
  stopifnot(isTRUE(ordered) || isFALSE(ordered))
  formula_parts <- as.character(formula)
  vars <- trimws(strsplit(formula_parts[3], split = "|", fixed = TRUE)[[1]])
  while (length(vars) < 3) vars <- c(vars, NA_character_)
  vars <- lapply(strsplit(vars, split = "+", fixed = TRUE), trimws)
  validate_RprobitB_formula(
    structure(
      list(
        formula = formula,
        re = re,
        ordered = ordered,
        choice = formula_parts[2],
        vars = vars,
        ASC = ifelse(0 %in% vars[[2]], FALSE, TRUE),
        md_n = re[!endsWith(re, "+")],
        md_ln = sub(".{1}$", "", re[endsWith(re, "+")])
      ),
      class = "RprobitB_formula"
    )
  )
}

#' Validator for \code{RprobitB_formula} object
#'
#' @description
#' This function validates an \code{RprobitB_formula} object.
#'
#' @param x
#' An object of class \code{RprobitB_formula}.
#'
#' @return
#' The input \code{x}.

validate_RprobitB_formula <- function(x) {
  if (length(as.character(x$formula)) != 3) {
    RprobitB_stop("'formula' is not in form '<choice> ~ <covariates>'.")
  }
  for (re in x[c("md_n","md_ln")]) {
    # TODO
  }
  return(x)
}

#' @noRd
#' @exportS3Method

print.RprobitB_formula <- function(x, ...) {
  stopifnot(inherits(x, "RprobitB_formula"))
  cat("Formula:", deparse1(x$formula))
  if (length(attr(x, "re")) > 0) {
    cat("\nRandom effects:", attr(x$re, "re"))
  }
}
