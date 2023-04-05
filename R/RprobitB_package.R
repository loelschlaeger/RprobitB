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

RprobitB_pp <- function(title, i = NULL, total = NULL, tail = NULL) {
  if (identical(getOption("RprobitB_progress"), TRUE)) {
    if (is.null(i) || is.null(total)) {
      message(title)
    } else {
      message(paste(title, "-", i, "of", total, tail, "\r"),
        appendLF = (i == total)
      )
    }
  }
}

#' @noRd
#' @importFrom progress progress_bar

RprobitB_pb <- function(title, total, tail = NULL) {
  progress::progress_bar$new(
    format = paste(title, "-", ":current of :total", tail),
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
#' @importFrom cli style_hyperlink

.onAttach <- function(lib, pkg) {
  msg <- paste0(
    "This is {RprobitB} ", utils::packageVersion("RprobitB"), ", happy choice modeling!\n",
    "See ", cli::style_hyperlink("https://loelschlaeger.de/RprobitB", "https://loelschlaeger.de/RprobitB"), " for help."
  )
  packageStartupMessage(msg)
  invisible()
}
