#' Define choice alternatives
#'
#' @description
#' This function constructs an object of class
#' \code{\link{RprobitB_alternatives}}, which contains the choice alternatives.
#'
#' @param J
#' An \code{integer}, the number of choice alternatives.
#' Must be at least \code{2}.
#' If \code{ordered = TRUE}, must be at least \code{3}.
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
#' An \code{\link{RprobitB_alternatives}} object.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{J}}{The number of choice alternatives.}
#'   \item{\code{alternatives}}{The names of the choice alternatives}
#'   \item{\code{base}}{The name of the base alternative.}
#'   \item{\code{ordered}}{Are the choice alternatives ordered?}
#' }
#'
#' @details
#' # Base alternative
#' The full collection of coefficients for covariates that are constant across
#' alternatives (including ASCs) is not identified.
#' To achieve identifiability, the coefficient of one alternative \code{base}
#' is fixed to \code{0}.
#' The other coefficients then have to be interpreted with respect to
#' \code{base}.
#'
#' @examples
#' \dontrun{
#' RprobitB_alternatives(3)
#' }
#'
#' @keywords internal object

RprobitB_alternatives <- function(
    J, alternatives = LETTERS[1:J], base = alternatives[1], ordered = FALSE
  ) {
  if (missing(J)) {
    RprobitB_stop(
      "Please specify the input 'J'.",
      "It should be an `integer`, the number of choice alternatives."
    )
  }
  if (!is.vector(alternatives)) {
    RprobitB_stop(
      "Input 'alternatives' should be a `vector`.",
      "It containes the names of the choice alternatives."
    )
  }
  if (!isTRUE(ordered) && !isFALSE(ordered)) {
    RprobitB_stop(
      "Input 'ordered' must be `TRUE` or `FALSE`."
    )
  }
  if (!ordered) {
    alternatives <- sort(alternatives)
  }
  validate_RprobitB_alternatives(
    structure(
      list(
        J = J,
        alternatives = alternatives,
        base = base,
        ordered = ordered
      ),
      class = "RprobitB_alternatives"
    )
  )
}

#' @rdname RprobitB_alternatives
#' @param x
#' An \code{\link{RprobitB_alternatives}} object.

is.RprobitB_alternatives <- function(x) {
  inherits(x, "RprobitB_alternatives")
}

#' @rdname RprobitB_alternatives
#' @importFrom glue glue

validate_RprobitB_alternatives <- function(x) {
  if (!is_pos_int(x$J)) {
    RprobitB_stop(
      "Input 'J' is misspecified.",
      "It should be an `integer`, the number of choice alternatives."
    )
  } else {
    x$J <- as.integer(x$J)
  }
  if (!is.character(x$alternatives)) {
    RprobitB_stop(
      "Input 'alternatives' is misspecified.",
      "It should be a `character` vector, the names of the choice alternatives."
    )
  }
  if (length(x$alternatives) != x$J) {
    RprobitB_stop(
      glue::glue("Input 'alternatives' must be of length 'J = {x$J}'."),
      glue::glue("Instead, it is of length {length(x$alternatives)}.")
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
        "At least 3 alternatives are required in the ordered case.",
        "Please make sure that input 'alternatives' has at least 3 elements."
      )
    }
  } else {
    if (length(x$alternatives) < 2) {
      RprobitB_stop(
        "Input 'alternatives' is misspecified.",
        "Please make sure that input 'alternatives' has at least 2 elements."
      )
    }
    if (!(is.character(x$base) && length(x$base) == 1)) {
      RprobitB_stop(
        "Input 'base' is misspecified.",
        "It should be a single `character` vector, the base alternative name."
      )
    }
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
#' @param ...
#' Currently not used.

print.RprobitB_alternatives <- function(x, ...) {
  if (!is.RprobitB_alternatives(x)) {
    RprobitB_stop(
      "Input 'x' is not of class `RprobitB_alternatives`."
    )
  }
  alt <- x$alternatives
  if (!x$ordered) alt[alt == x$base] <- paste0(alt[alt == x$base], "*")
  cat(cli::style_underline("Alternatives:"), alt, if (x$ordered) "(ordered)")
}

