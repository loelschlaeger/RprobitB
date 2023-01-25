#' Define probit model formula
#'
#' @description
#' This function constructs an object of class \code{\link{RprobitB_formula}},
#' which contains the formula for a probit model.
#'
#' @param formula
#' A \code{\link[stats]{formula}}, a symbolic description of the model to be
#' fitted, see details.
#' @param re
#' A \code{character}, the vector of names of covariates in \code{formula} that
#' should have a random effect, see details.
#' By default, \code{re = NULL}, i.e., no random effects.
#' @inheritParams RprobitB_alternatives
#'
#' @return
#' An \code{\link{RprobitB_formula}} object.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{formula}}{The model formula.}
#'   \item{\code{re}}{The names of covariates with random effects.}
#'   \item{\code{ordered}}{Are the choice alternatives ordered?}
#'   \item{\code{choice}}{The name of the dependent variable.}
#'   \item{\code{vars}}{The different types of covariates.}
#'   \item{\code{ASC}}{Does the model have ASCs?}
#'   \item{\code{md_n}}{The covariates with normal mixing distribution.}
#'   \item{\code{md_ln}}{The covariates with log-normal mixing distribution.}
#' }
#'
#' @details
#' # Model formula
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
#' Therefore, the \code{formula} object does not need the special
#' separation form via \code{|}, and hence has the simple structure
#' \code{choice ~ A + B + C}.
#' ASCs cannot be estimated in the ordered case.
#'
#' # Random effects
#' Covariates can have random effects, i.e., their coefficients can follow a
#' random distribution. Per default, the distribution is normal. The log-normal
#' distribution (for sign-restriction) can be specified via appending \code{+}
#' to the corresponding name in \code{re}.
#' To have random effects for the ASCs, add \code{ASC} (or \code{ASC+}) to
#' \code{re}.
#'
#' @examples
#' \dontrun{
#' formula <- choice ~ A | B + 0 | C + D
#' re <- c("A", "D+")
#' RprobitB_formula(formula, re, ordered = FALSE)
#' }
#'
#' @keywords internal object

RprobitB_formula <- function(formula, re = NULL, ordered = FALSE) {
  if (missing(formula)) {
    RprobitB_stop(
      "Please specify the input 'formula'.",
      "See the function documentation for details."
    )
  }
  if (!inherits(formula, "formula")) {
    RprobitB_stop(
      "Input 'formula' is misspecified.",
      "It should be a `formula` object."
    )
  }
  if (is.null(re)) {
    re <- character()
  } else {
    if (!is.character(re)) {
      RprobitB_stop(
        "Input 're' is misspecified.",
        "It should be a `character` vector."
      )
    } else {
      re <- unique(re)
    }
  }
  if (!isTRUE(ordered) && !isFALSE(ordered)) {
    RprobitB_stop(
      "Input 'ordered' must be `TRUE` or `FALSE`."
    )
  }
  formula_parts <- as.character(formula)
  vars <- trimws(strsplit(formula_parts[3], split = "|", fixed = TRUE)[[1]])
  if (length(vars) > 3) {
    RprobitB_stop(
      "Input 'formula' is misspecified.",
      "It should have no more than 2 of '|' separators."
    )
  }
  while (length(vars) < 3) vars <- c(vars, NA_character_)
  vars <- lapply(strsplit(vars, split = "+", fixed = TRUE), trimws)
  ASC <- if (ordered) FALSE else ifelse(0 %in% vars[[2]], FALSE, TRUE)
  vars <- lapply(vars, function(x) x[!x %in% c(0, 1, NA)])
  if (ordered) {
    ### in the ordered case, 'vars' has only variables in second position
    vars <- list(character(), unlist(vars[1:3]), character())
  }
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

#' @rdname RprobitB_formula
#' @param x
#' An \code{\link{RprobitB_formula}} object.

is.RprobitB_formula <- function(x) {
  inherits(x, "RprobitB_formula")
}

#' @rdname RprobitB_formula
#' @importFrom glue glue

validate_RprobitB_formula <- function(x) {
  if (length(as.character(x$formula)) != 3) {
    RprobitB_stop(
      "Input 'formula' is misspecified.",
      "It should be in the form '<choice> ~ <covariates>'."
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
        glue::glue("But '{re}' is not on the right side of the model formula '{deparse1(x$formula)}'.")
      )
    }
  }
  return(x)
}

#' @rdname RprobitB_formula
#' @exportS3Method
#' @importFrom cli style_underline
#' @param ...
#' Currently not used.

print.RprobitB_formula <- function(x, ...) {
  if (!is.RprobitB_formula(x)) {
    RprobitB_stop(
      "Input 'x' is not of class `RprobitB_formula`."
    )
  }
  cat(cli::style_underline("Formula:"), deparse1(x$formula))
  if (length(x$re) > 0) {
    cat("\n")
    cat(cli::style_underline("Random effects:"), x$re)
  }
}
