#' Visualizing the trace of Gibbs samples.
#'
#' @description
#' This function plots traces of the Gibbs samples.
#'
#' @param gibbs_samples
#' A matrix of Gibbs samples.
#' @param par_labels
#' A character vector of length equal to the number of columns of
#' \code{gibbs_samples}, containing labels for the Gibbs samples.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal

plot_trace <- function(gibbs_samples, par_labels) {
  ### define colors
  col <- viridis::magma(n = ncol(gibbs_samples), begin = 0.1, end = 0.9, alpha = 0.6)

  ### plot trace
  stats::plot.ts(gibbs_samples,
                 plot.type = "single",
                 ylim = c(min(gibbs_samples), max(gibbs_samples)),
                 col = col, xlab = "Iteration", ylab = "", xaxt = "n", main = "", las = 1
  )

  ### add info
  graphics::axis(
    side = 1, at = c(1, nrow(gibbs_samples)),
    labels = c("B+1", "R")
  )
  graphics::legend("topright",
                   legend = par_labels, lty = 1, col = col,
                   cex = 0.75, bg = "white"
  )
}
