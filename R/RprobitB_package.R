#' fHMM: A package for Bayes estimation of multinomial Probit models
#'
#' This package provides tools for Bayes estimation of multinomial Probit models.
#'
#' @docType package
#' @name RprobitB
#' @importFrom Rcpp sourceCpp
#' @useDynLib RprobitB, .registration=TRUE
#' @keywords
#' internal
NULL

#' @noRd

rpb <- function() {
  .Deprecated()
}

#' @noRd

RprobitB_progress <- function(title, total) {
  progress::progress_bar$new(
    format = title,
    total = total,
    clear = FALSE
  )
}

#' @noRd
#' @importFrom utils packageVersion

.onAttach <- function(lib, pkg) {
  ### startup message
  msg <- paste0(
    "Thanks for using {RprobitB} ", utils::packageVersion("RprobitB"),
    ", happy choice modeling!\n",
    "See https://loelschlaeger.de/RprobitB for help.\n",
    "Type 'citation(\"RprobitB\")' for citing this R package."
  )
  packageStartupMessage(msg)
  invisible()
}
