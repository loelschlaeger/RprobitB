#' Constructor for \code{RprobitB_formula} object
#'
#' @description
#' This function constructs an S3 object of class \code{RprobitB_formula}.
#'
#' @param formula
#' An object of class \code{\link[stats]{formula}}:
#' a symbolic description of the model to be fitted.
#' The details of model specification are given under \sQuote{Details}.
#' @param re
#' A character (vector): the names of covariates in \code{formula} that should
#' have a random effect.
#' Set \code{re = NULL} (default) for no random effects.
#' The details of random effects are given under \sQuote{Details}.
#' @inheritParams new_RprobitB_alternatives
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
#'   \item \code{A} are names of \strong{alternative specific covariates} with
#'   \strong{a coefficient that is constant across alternatives},
#'   \item \code{B} are names of \strong{covariates that are constant across
#'   alternatives},
#'   \item and \code{C} are names of \strong{alternative specific covariates}
#'   with \strong{alternative specific coefficients}.
#' }
#'
#' Multiple covariates of one type are separated by a \code{+} sign, i.e.,
#' \code{choice ~ A1 + A2}.
#'
#' By default, alternative specific constants (ASCs) are added to the model.
#' They can be removed by adding \code{+ 0} in the second spot, i.e.,
#' \code{choice ~ A | B + 0 | C}. To not include any covariates of
#' the second category but to estimate ASCs, add \code{1} in the second
#' spot, e.g., \code{choice ~ A | 1 | C}. The expression
#' \code{choice ~ A | 0 | C} is interpreted as no covariates of the second
#' category and no ASCs.
#'
#' In the ordered probit model (\code{ordered = TRUE}), covariates are not
#' alternative specific, i.e., there exists only one type of covariate.
#' Therefore, the \code{formula} object does not need any
#' separation via \code{|}, and hence has the simple structure
#' \code{choice ~ A + B + C}.
#' ASCs cannot be estimated in the ordered case.
#'
#' # Details of random effects
#' Covariates can have random effects, i.e., their coefficients can follow a
#' random distribution. Per default, the distribution is normal. The log-normal
#' distribution (for sign-restriction) can be specified via appending a \code{+}
#' to the corresponding name in \code{re}.
#' To have random effects for the ASCs, add \code{ASC} (or \code{ASC+}) to
#' \code{re}.
#'
#' @examples
#' formula <- choice ~ A | B + 0 | C + D
#' re <- c("A", "D+")
#' new_RprobitB_formula(formula, re, ordered = FALSE)

new_RprobitB_formula <- function(formula, re = NULL, ordered = FALSE) {
  if (missing(formula)) {
    RprobitB_stop("Please specify the input 'formula'.")
  }
  stopifnot(inherits(formula, "formula"))
  if (is.null(re)) re <- character()
  stopifnot(is.character(re))
  re <- unique(re)
  stopifnot(isTRUE(ordered) || isFALSE(ordered))
  formula_parts <- as.character(formula)
  vars <- trimws(strsplit(formula_parts[3], split = "|", fixed = TRUE)[[1]])
  while (length(vars) < 3) vars <- c(vars, NA_character_)
  vars <- lapply(strsplit(vars, split = "+", fixed = TRUE), trimws)
  ASC <- if (ordered) FALSE else ifelse(0 %in% vars[[2]], FALSE, TRUE)
  vars <- lapply(vars, function(x) x[!x %in% c(0, 1, NA)])
  if (ordered) vars <- list(character(), unlist(vars[1:3]), character())
  validate_RprobitB_formula(
    structure(
      list(
        formula = formula,
        re = re,
        ordered = ordered,
        choice = formula_parts[2],
        vars = vars,
        ASC = ASC,
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
#'
#' @importFrom glue glue

validate_RprobitB_formula <- function(x) {
  if (length(as.character(x$formula)) != 3) {
    RprobitB_stop(
      "Input 'formula' is misspecified.",
      glue::glue("'{deparse1(x$formula)}' is not in form '<choice> ~ <covariates>'.")
    )
  }
  if (x$ordered) {
    if (grepl("|", x$formula[3], fixed = TRUE)) {
      RprobitB_stop(
        "Input 'formula' is misspecified.",
        "The ordered probit model is specified via 'ordered' = TRUE.",
        glue::glue("Vertical bars in '{deparse1(x$formula)}' are not allowed in this case.")
      )
    }
  }
  if (length(intersect(x$md_n, x$md_ln)) != 0) {
    re_double <- intersect(x$md_n, x$md_ln)[1]
    RprobitB_stop(
      "Input 're' is misspecified.",
      glue::glue("'re' includes '{re_double}' and '{re_double}+'."),
      glue::glue("Either a normal or a log-normal mixing distribution can be specified.")
    )
  }
  for (re in unlist(x[c("md_n","md_ln")])) {
    if (!re %in% c(unlist(x$vars), if(x$ASC) "ASC")) {
      RprobitB_stop(
        "Input 're' is misspecified.",
        glue::glue("'re' includes '{re}'."),
        glue::glue("But '{re}' is not part of '{deparse1(x$formula)}'.")
      )
    }
  }
  return(x)
}

#' @noRd
#' @exportS3Method
#' @importFrom cli style_underline

print.RprobitB_formula <- function(x, ...) {
  stopifnot(inherits(x, "RprobitB_formula"))
  cat(cli::style_underline("Formula:"), deparse1(x$formula))
  if (length(x$re) > 0) {
    cat("\n")
    cat(cli::style_underline("Random effects:"), x$re)
  }
}
