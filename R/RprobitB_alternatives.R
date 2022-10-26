#' Define choice alternatives
#'
#' This function constructs an object of class \code{RprobitB_alternatives},
#' which contains the choice alternatives.
#'
#' @param J
#' An \code{integer}, the number of choice alternatives.
#' Must be at least 2.
#' If \code{ordered = TRUE}, must be at least 3.
#' @param alternatives
#' A \code{character}, the vector of names of the choice alternatives.
#' Its length must be \code{J}.
#' By default, \code{alternatives = LETTERS[1:J]}.
#' @param base
#' A \code{character}, the name of the base alternative for covariates that are
#' not alternative specific, see details.
#' \code{base} must be contained in \code{alternatives}.
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
#' RprobitB_alternatives(3)
#'
#' @keywords internal

RprobitB_alternatives <- function(
    J, alternatives = LETTERS[1:J], base = alternatives[1], ordered = FALSE
  ) {
  if (missing(J)) {
    RprobitB_stop("Please specify the input 'J'.")
  }
  stopifnot(
    is_pos_int(J), is.character(alternatives), is.character(base),
    length(base) == 1, isTRUE(ordered) || isFALSE(ordered)
  )
  if (!ordered) alternatives <- sort(alternatives)
  validate_RprobitB_alternatives(
    structure(
      list(
        J = length(alternatives),
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
      "Please make sure that input 'alternatives' has at least two elements."
    )
  }
  if (length(x$alternatives) != x$J) {
    RprobitB_stop(
      glue::glue("The input 'alternatives' must be of length {x$J}."),
      glue::glue("Instead, it is of length {length(x$alternatives}.")
    )
  }
  if (!identical(x$alternatives, unique(x$alternatives))) {
    RprobitB_stop(
      "Alternatives must be unqiue.",
      "Please make sure that input 'alternatives' has unique elements."
    )
  }
  if (x$ordered) {
    x$base <- NA
    if (length(x$alternatives) < 3) {
      RprobitB_stop(
        "At least three alternatives are required in the ordered case.",
        "Please make sure that input 'alternatives' has at least three elements."
      )
    }
  } else {
    if (!x$base %in% x$alternatives) {
      RprobitB_stop(
        "Base alternative must be in alternative set.",
        "Please make sure that input 'base' is contained in 'alternatives'."
      )
    }
  }
  return(x)
}

#' @rdname RprobitB_alternatives
#' @exportS3Method
#' @importFrom cli style_underline

print.RprobitB_alternatives <- function(x, ...) {
  stopifnot(is.RprobitB_alternatives(x))
  alt <- x$alternatives
  if (!x$ordered) alt[alt == x$base] <- paste0(alt[alt == x$base], "*")
  cat(cli::style_underline("Alternatives:"), alt, if (x$ordered) "(ordered)")
}
