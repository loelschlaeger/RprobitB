#' RprobitB: Bayes Estimation of Probit Models for Discrete Choice Analysis
#'
#' @description
#' This package provides tools for Bayes estimation of probit models for
#' discrete choice analysis.
#'
#' @docType package
#' @name RprobitB
#' @author Lennart Oelschläger, \email{oelschlaeger.lennart@gmail.com}
#' @importFrom Rcpp sourceCpp
#' @useDynLib RprobitB, .registration=TRUE
#' @keywords internal

"_PACKAGE"

#' @noRd
#' @keywords internal

.onLoad <- function(lib, pkg) {
  options("RprobitB_verbose" = TRUE)
}

#' @noRd
#' @importFrom glue glue
#' @importFrom utils packageVersion
#' @importFrom cli style_hyperlink

.onAttach <- function(lib, pkg) {
  msg <- glue::glue(
    "Thanks for using {{RprobitB}} {utils::packageVersion('RprobitB')}",
    ", happy choice modeling!\n",
    "Documentation: ",
    cli::style_hyperlink("https://loelschlaeger.de/RprobitB", "https://loelschlaeger.de/RprobitB"),
  )
  packageStartupMessage(msg)
  invisible()
}

#' @noRd
#' @keywords internal

rpb <- function() {
  .Defunct(msg = "This {RprobitB} function is currently not available.")
}

#' @noRd
#' @keywords internal

RprobitB_pp <- function(title, i = NULL, total = NULL, tail = NULL) {
  if (isTRUE(getOption("RprobitB_progress"))) {
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
#' @keywords internal

RprobitB_pb <- function(title, total, tail = NULL) {
  progress::progress_bar$new(
    format = paste(title, "-", ":current of :total", tail),
    total = total,
    clear = FALSE
  )
}

#' @noRd
#' @keywords internal

RprobitB_pb_tick <- function(pb) {
  if (isTRUE(getOption("RprobitB_progress"))) pb$tick()
}

#' @noRd
#' @importFrom cli cli_abort
#' @keywords internal

RprobitB_stop <- function(msg, ...) {
  msg <- c(msg, ...)
  names(msg)[1] <- "x"
  names(msg)[-1] <- ""
  cli::cli_abort(msg, call = NULL)
}

#' @noRd
#' @importFrom cli cli_warn
#' @keywords internal

RprobitB_warn <- function(msg, ...) {
  msg <- c(msg, ...)
  names(msg)[1] <- "!"
  names(msg)[-1] <- ""
  cli::cli_warn(unlist(msg))
}
