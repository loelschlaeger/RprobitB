## usethis namespace: start
#' @importFrom checkmate assert_int
#' @importFrom oeli assert_covariance_matrix
#' @importFrom oeli delta
#' @importFrom oeli permutations
#' @importFrom oeli print_matrix
#' @importFrom oeli test_covariance_matrix
#' @importFrom Rcpp sourceCpp
#' @importFrom Rdpack reprompt
#' @importFrom stats var
## usethis namespace: end
#' @useDynLib RprobitB, .registration=TRUE

"_PACKAGE"

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
#' @importFrom cli style_hyperlink
#' @importFrom utils packageVersion

.onAttach <- function(lib, pkg) {
  doc_link <- "https://loelschlaeger.de/RprobitB"
  msg <- c(
    paste0(
      "Thanks for using {RprobitB} version ", utils::packageVersion("RprobitB")
    ),
    ", happy choice modeling!\n",
    "Documentation: ",
    cli::style_hyperlink(doc_link, doc_link)
  )
  packageStartupMessage(msg)
  invisible()
}
