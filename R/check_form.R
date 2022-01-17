#' Check \code{form}.
#' @description
#' This function checks the input \code{form}.
#' @param form
#' A formula object that is used to specify the probit model.
#' The structure is \code{choice ~ A | B | C}, where
#' \itemize{
#'   \item \code{A} are names of alternative and choice situation specific
#'   covariates with a generic coefficient,
#'   \item \code{B} are names of choice situation specific covariates with
#'   alternative specific coefficients,
#'   \item and \code{C} are names of alternative and choice situation specific
#'   covariates with alternative specific coefficients.
#' }
#' Separate multiple covariates of one type by a \code{+} sign.
#' By default, alternative specific constants (ASCs) are added to the model
#' (for all except for the last alternative).
#' They can be removed by adding \code{+0} in the second spot.
#' See the vignette \code{vignette("data_management", package = "RprobitB")}
#' for more details.
#' @param re
#' A character (vector) of covariates of \code{form} with random effects.
#' If \code{re = NULL} (the default), there are no random effects.
#' To have random effects for the alternative specific constants, include
#' \code{"ASC"} in \code{re}.
#' @return
#' An object of class \code{RprobitB_formula}, which is a list that contains the
#' following elements:
#' \itemize{
#'   \item \code{form}:
#'   The input \code{form}.
#'   \item \code{choice}:
#'   The dependent variable in \code{form}.
#'   \item \code{re}:
#'   The input \code{re}, where covariates that are not part of \code{form}
#'   are removed.
#'   \item \code{vars}:
#'   A list of three character vectors of covariate names of the three
#'   covariate types.
#'   \item \code{ASC}:
#'   A boolean, determining whether the model has ASCs.
#' }
#' @examples
#' form <- choice ~ price + time + comfort + change | 1
#' re <- re <- c("price", "time")
#' check_form(form = form, re = re)
#' @export

check_form <- function(form, re = NULL) {

  ### check inputs
  if (!inherits(form, "formula")) {
    stop("'form' must be of class 'formula'.")
  }
  if (!is.null(re)) {
    if (!is.character(re)) {
      stop("'re' must be a character (vector).")
    }
  }

  ### extract name of depentend variable
  choice <- all.vars(form)[1]

  ### build 'vars'
  vars <- trimws(strsplit(as.character(form)[3], split = "|", fixed = TRUE)[[1]])
  while (length(vars) < 3) {
    vars <- c(vars, NA)
  }
  vars <- lapply(strsplit(vars, split = "+", fixed = TRUE), trimws)

  ### build 'ASC'
  ASC <- ifelse(any(vars[[2]] %in% 0), FALSE, TRUE)
  for (i in 1:3) {
    vars[[i]] <- vars[[i]][!vars[[i]] %in% c(0, 1, NA)]
  }

  ### match 're' with 'form'
  if (!is.null(re)) {
    for (re_element in re) {
      if (!re_element %in% c("ASC", unlist(vars))) {
        re <- setdiff(re, re_element)
        warning(
          "The covariate '", re_element,
          "' in 're' is not part of 'form' and hence ignored."
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
  class(out) <- "RprobitB_formula"
  return(out)
}

#' Print method for \code{RprobitB_formula}.
#' @description
#' This function is the print method for an object of class
#' \code{RprobitB_formula}.
#' @param x
#' An object of class \code{RprobitB_formula}.
#' @param ...
#' Ignored.
#' @return
#' Invisibly \code{x}.
#' @noRd

print.RprobitB_formula <- function(x, ...) {
  print(x$form)
  cat("- dependent variable:", x$choice, "\n")
  for (i in 1:3) {
    cat("- type", i, "covariate(s):", paste(x$vars[[i]], collapse = ", "), "\n")
  }
  cat("- random effects:", paste(x$re, collapse = ", "), "\n")
  cat("- ASC:", x$ASC, "\n")
  return(invisible(x))
}
