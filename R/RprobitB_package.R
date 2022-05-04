#' RprobitB: A package for Bayes estimation of probit models
#'
#' This package provides tools for Bayes estimation of probit models.
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

RprobitB_pp <- function(title, i = NULL, total = NULL) {
  if (identical(getOption("RprobitB_progress"), TRUE)) {
    if(is.null(i) || is.null(total)) {
      message(title)
    } else {
      message(paste(title, i, "of", total, "\r"), appendLF = (i == total))
    }
  }
}

#' @noRd
#' @importFrom progress progress_bar

RprobitB_pb <- function(title, total) {
  progress::progress_bar$new(
    format = paste(title, ":current of :total"),
    total = total,
    clear = FALSE
  )
}

#' @noRd

RprobitB_pb_tick <- function(pb) {
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
