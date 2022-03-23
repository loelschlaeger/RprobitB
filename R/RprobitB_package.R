#' RprobitB: A package for Bayes estimation of multinomial Probit models
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
  .Defunct(msg = "This function was removed from {RprobitB}.")
}

#' @noRd
#' @importFrom progress progress_bar

RprobitB_progress <- function(title, total) {
  progress::progress_bar$new(
    format = paste(":spin", title, ":percent [ETA: :eta]"),
    total = total,
    clear = TRUE
  )
}

#' @noRd

RprobitB_pp <- function(pb) {
  if (identical(getOption("RprobitB_progress"), TRUE)) {
    pb$tick()
  }
}

#' @noRd

.onLoad <- function(lib, pkg) {
  options("RprobitB_progress" = TRUE)
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
