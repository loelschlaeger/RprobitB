#' @useDynLib RprobitB, .registration=TRUE
#' @keywords internal

"_PACKAGE"

(function() {
  .Call("run_testthat_tests", FALSE, PACKAGE = "RprobitB")
})

## usethis namespace: start
#' @importFrom checkmate assert_int
#' @importFrom crayon underline
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach %dopar%
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 expand_limits
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_contour
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme_minimal
#' @importFrom graphics legend
#' @importFrom graphics par
#' @importFrom graphics points
#' @importFrom graphics title
#' @importFrom gridExtra grid.arrange
#' @importFrom MASS ginv
#' @importFrom mixtools ellipse
#' @importFrom mvtnorm dmvnorm
#' @importFrom mvtnorm pmvnorm
#' @importFrom oeli assert_covariance_matrix
#' @importFrom oeli delta
#' @importFrom oeli permutations
#' @importFrom oeli print_matrix
#' @importFrom oeli test_covariance_matrix
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom plotROC geom_roc
#' @importFrom plotROC style_roc
#' @importFrom Rcpp sourceCpp
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
#' @importFrom stats AIC
#' @importFrom stats BIC
#' @importFrom stats complete.cases
#' @importFrom stats cov2cor
#' @importFrom stats density
#' @importFrom stats dnorm
#' @importFrom stats ecdf
#' @importFrom stats logLik
#' @importFrom stats na.omit
#' @importFrom stats nobs
#' @importFrom stats pnorm
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom stats sd
#' @importFrom stats spec.ar
#' @importFrom stats var
#' @importFrom utils tail
#' @importFrom viridis magma
## usethis namespace: end

#' @noRd

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
