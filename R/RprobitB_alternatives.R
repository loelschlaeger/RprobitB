#' Define choice alternatives
#'
#' This function constructs an object of class \code{RprobitB_alternatives},
#' which contains the choice alternatives.
#'
#' @param alternatives
#' A \code{character}, the vector of names of the choice alternatives.
#' Must be at least of length two.
#' @param base
#' A \code{character}, the name of the base alternative for covariates that are
#' not alternative specific, see details.
#' Ignored if the model has no alternative specific covariates (e.g., in the
#' ordered probit case).
#' By default, \code{base} is the first element of \code{alternatives}.
#' @param ordered
#' A \code{logical}, \code{TRUE} if the choice alternatives are ordered and
#' \code{FALSE} (default) else.
#'
#' @return
#' An \code{RprobitB_alternatives} object.
#'
#' @details
#' # Base alternative
#' The full collection of coefficients for covariates that are constant across
#' alternatives (including ASCs) is not identified, one coefficient is a linear
#' coefficient of the others, respectively. To achieve identifiability, the
#' coefficient of one alternative \code{base} is fixed to zero. The other
#' coefficients then have to be interpreted with respect to \code{base}.
#'
#' @examples
#' RprobitB_alternatives(LETTERS[1:3])
#'
#' @keywords internal

RprobitB_alternatives <- function(
    alternatives, base = alternatives[1], ordered = FALSE
  ) {
  if (missing(alternatives)) {
    RprobitB_stop("Please specify the input 'alternatives'.")
  }
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

#' @rdname RprobitB_alternatives

is.RprobitB_alternatives <- function(x) {
  inherits(x, "RprobitB_alternatives")
}

#' @rdname RprobitB_alternatives
#' @param x
#' An \code{RprobitB_alternatives} object.

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

#' @rdname RprobitB_alternatives
#' @exportS3Method
#' @importFrom cli style_underline

print.RprobitB_alternatives <- function(x, ...) {
  stopifnot(is.RprobitB_alternatives(x))
  alt <- x$alternatives
  alt[alt == x$base] <- paste0(alt[alt == x$base], "*")
  cat(cli::style_underline("Alternatives:"), alt, if (x$ordered) "(ordered)")
}
