#' Constructor for \code{RprobitB_alternatives} object
#'
#' @description
#' This function constructs an S3 object of class \code{RprobitB_alternatives}.
#'
#' @param alternatives
#' A character vector: the names of the choice alternatives. Must be at least of
#' length two.
#' @param base
#' A character: the name of the base alternative for covariates that are not
#' alternative specific.
#' Ignored if the model has no alternative specific covariates (e.g., in the
#' ordered probit case).
#' Per default, \code{base} is the last element of \code{alternatives}.
#' The details of the base alternative are given under \sQuote{Details}.
#' @param ordered
#' A boolean: \code{TRUE} for if the alternatives are ordered and \code{FALSE}
#' (default) else.
#'
#' @return
#' An object of class \code{RprobitB_alternatives}.
#'
#' @details
#' # Details of the base alternative
#' The full collection of coefficients for covariates that are constant across
#' alternatives (including ASCs) is not identified, one coefficient is a linear
#' coefficient of the others, respectively. To achieve identifiability, the
#' coefficient of one alternative \code{base} is fixed to zero. The other
#' coefficients then have to be interpreted with respect to \code{base}.
#'
#' @examples
#' new_RprobitB_alternatives(LETTERS[1:3])
#'
#' @keywords internal

new_RprobitB_alternatives <- function(
    alternatives = character(), base = tail(alternatives, 1), ordered = FALSE
  ) {
  stopifnot(is.character(alternatives))
  stopifnot(is.character(base), length(base) == 1)
  stopifnot(isTRUE(ordered) || isFALSE(ordered))
  if (!ordered) alternatives <- sort(alternatives)
  validate_RprobitB_alternatives(
    structure(
      list(
        alternatives = alternatives,
        base = base,
        ordered = ordered
      ),
      class = "RprobitB_alternatives"
    )
  )
}

#' Validator for \code{RprobitB_alternatives} object
#'
#' @description
#' This function validates an \code{RprobitB_alternatives} object.
#'
#' @param x
#' An object of class \code{RprobitB_alternatives}.
#'
#' @return
#' The input \code{x}.

validate_RprobitB_alternatives <- function(x) {
  if (length(x$alternatives) < 2) {
    RprobitB_stop(
      "At least two alternatives are required.",
      "Input 'alternatives' must have at least two elements."
    )
  }
  if (!identical(x$alternatives, unique(x$alternatives))) {
    RprobitB_stop(
      "Alternatives must be unqiue.",
      "Input 'alternatives' does not have unique elements."
    )
  }
  if (!x$base %in% x$alternatives) {
    RprobitB_stop(
      "Base alternative must be in alternative set.",
      "Input 'base' must be an element of 'alternatives'."
    )
  }
  if (x$ordered) {
    x$base <- NULL
  }
  return(x)
}

#' @noRd
#' @exportS3Method
#' @importFrom cli style_underline

print.RprobitB_alternatives <- function(x, ...) {
  stopifnot(inherits(x, "RprobitB_alternatives"))
  alt <- x$alternatives
  alt[alt == x$base] <- paste0(alt[alt == x$base], "*")
  cat(cli::style_underline("Alternatives:"), alt, if (x$ordered) "(ordered)")
}
