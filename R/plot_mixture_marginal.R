#' Plot marginal mixing distributions
#'
#' @description
#' This function plots an estimated marginal mixing distributions.
#'
#' @param mean
#' The class means.
#' @param cov
#' The class covariances.
#' @param weights
#' The class weights.
#' @param name
#' The covariate name.
#' @return
#' An object of class \code{ggplot}.
#'
#' @keywords
#' internal

plot_mixture_marginal <- function(mean, cov, weights, name) {
  C <- length(weights)
  x_min <- min(mapply(function(x, y) x - 3 * y, mean, cov))
  x_max <- max(mapply(function(x, y) x + 3 * y, mean, cov))
  x <- seq(x_min, x_max, length.out = 200)
  y <- Reduce("+", sapply(1:C, function(c) {
    weights[c] *
      stats::dnorm(x, mean[[c]], sd = cov[[c]])
  },
  simplify = FALSE
  ))

  xint <- grp <- NULL
  out <- ggplot2::ggplot(data = data.frame(x = x, y = y), ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = bquote(beta[.(name)]), y = "")

  if (C > 1) {
    class_means <- data.frame(xint = unlist(mean), grp = factor(1:C))
    out <- out +
      ggplot2::geom_text(
        data = class_means,
        mapping = ggplot2::aes(x = xint, y = 0, label = grp, color = grp),
        size = 5,
        show.legend = FALSE
      )
  }

  return(out)
}
