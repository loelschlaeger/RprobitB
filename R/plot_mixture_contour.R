#' Plot bivariate contour of mixing distributions
#'
#' @description
#' This function plots an estimated bivariate contour mixing distributions.
#'
#' @param means
#' The class means.
#' @param covs
#' The class covariances.
#' @param weights
#' The class weights.
#' @param names
#' The covariate names.
#' @return
#' An object of class \code{ggplot}.
#'
#' @keywords
#' internal

plot_mixture_contour <- function(means, covs, weights, names) {
  C <- length(weights)
  x_min <- min(mapply(function(x, y) x[1] - 5 * y[1, 1], means, covs))
  x_max <- max(mapply(function(x, y) x[1] + 5 * y[1, 1], means, covs))
  y_min <- min(mapply(function(x, y) x[2] - 5 * y[2, 2], means, covs))
  y_max <- max(mapply(function(x, y) x[2] + 5 * y[2, 2], means, covs))
  data.grid <- expand.grid(
    x = seq(x_min, x_max, length.out = 200),
    y = seq(y_min, y_max, length.out = 200)
  )
  z <- Reduce("+", sapply(1:C, function(c) {
    mvtnorm::dmvnorm(data.grid, means[[c]], covs[[c]])
  }, simplify = FALSE))

  x <- y <- grp <- NULL
  out <- ggplot2::ggplot(
    data = cbind(data.grid, z),
    ggplot2::aes(x = .data$x, y = .data$y, z = .data$z)
  ) +
    ggplot2::geom_contour() +
    ggplot2::labs(x = bquote(beta[.(names[1])]), y = bquote(beta[.(names[2])]))

  if (C > 1) {
    class_means <- data.frame(
      x = sapply(means, "[[", 1), y = sapply(means, "[[", 2), z = 0,
      grp = factor(1:C)
    )
    out <- out +
      ggplot2::geom_text(
        data = class_means,
        mapping = ggplot2::aes(x = x, y = y, label = grp, color = grp),
        size = 5,
        show.legend = FALSE
      )
  }

  return(out)
}
