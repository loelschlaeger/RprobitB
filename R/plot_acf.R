#' Autocorrelation plot of Gibbs samples
#'
#' @description
#' This function plots the autocorrelation of the Gibbs samples. The plots
#' include the total Gibbs sample size \code{TSS} and the effective sample size
#' \code{ESS}, see the details.
#'
#' @details
#' The effective sample size is the value
#' \deqn{TSS / \sqrt{1 + 2\sum_{k\geq 1} \rho_k}},
#' where \eqn{\rho_k} is the auto correlation between the chain offset by
#' \eqn{k} positions. The auto correlations are estimated via
#' \code{\link[stats]{spec.ar}}.
#'
#' @param gibbs_samples
#' A matrix of Gibbs samples.
#' @param par_labels
#' A character vector with labels for the Gibbs samples, of length equal to the
#' number of columns of \code{gibbs_samples}.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal

plot_acf <- function(gibbs_samples, par_labels) {
  for (c in 1:ncol(gibbs_samples)) {
    x <- gibbs_samples[, c]
    sum_rho <- (stats::spec.ar(x, plot = F)$spec[1] / var(x) - 1) / 2
    stats::acf(x, las = 1, main = "")
    graphics::title(par_labels[c], line = 1)
    TSS <- length(x)
    ESS <- min(TSS / (1 + 2 * sum_rho), TSS)
    graphics::legend("topright",
                     x.intersp = -0.5, bg = "white",
                     legend = sprintf("%s %.0f", paste0(c("TSS", "ESS"), ":"), c(TSS, ESS))
    )
  }
}
