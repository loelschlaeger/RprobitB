#' Plot class allocation (for \code{P_r = 2} only)
#'
#' @description
#' This function plots the allocation of decision-maker specific coefficient vectors
#' \code{beta} given the allocation vector \code{z}, the class means \code{b},
#' and the class covariance matrices \code{Omega}.
#'
#' @details
#' Only applicable in the two-dimensional case, i.e. only if \code{P_r = 2}.
#'
#' @inheritParams RprobitB_parameter
#' @param ...
#' Optional visualization parameters:
#' \itemize{
#'   \item \code{colors}, a character vector of color specifications,
#'   \item \code{perc}, a numeric between 0 and 1 to draw the \code{perc} percentile
#'         ellipsoids for the underlying Gaussian distributions (\code{perc = 0.95} per default),
#'   \item \code{r}, the current iteration number of the Gibbs sampler to be displayed in the legend,
#'   \item \code{sleep}, the number of seconds to pause after plotting.
#' }
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal

plot_class_allocation <- function(beta, z, b, Omega, ...) {
  m <- as.vector(table(z))
  graphic_pars <- list(...)
  if (!is.null(graphic_pars[["colors"]])) {
    colors <- graphic_pars[["colors"]]
  } else {
    colors <- c(
      "black", "forestgreen", "red2", "orange", "cornflowerblue",
      "magenta", "darkolivegreen4", "indianred1", "tan4", "darkblue",
      "mediumorchid1", "firebrick4", "yellowgreen", "lightsalmon", "tan3",
      "tan1", "darkgray", "wheat4", "#DDAD4B", "chartreuse",
      "seagreen1", "moccasin", "mediumvioletred", "seagreen", "cadetblue1",
      "darkolivegreen1", "tan2", "tomato3", "#7CE3D8", "gainsboro"
    )
  }
  plot(t(beta), xlab = bquote(beta[1]), ylab = bquote(beta[2]))
  graphics::points(t(beta), col = colors[z], pch = 19)
  if (!is.null(graphic_pars[["perc"]])) {
    perc <- graphic_pars[["perc"]]
  } else {
    perc <- 0.95
  }
  for (c in 1:length(m)) {
    mixtools::ellipse(
      mu = b[, c], sigma = matrix(Omega[, c], ncol = nrow(Omega) / 2),
      alpha = 1 - perc, npoints = 250, col = colors[c]
    )
  }
  if (!is.null(graphic_pars[["r"]])) {
    title <- paste("Iteration", graphic_pars[["r"]])
  } else {
    title <- NULL
  }
  graphics::legend("topleft",
                   legend = paste0("class ", 1:length(m), " (", round(m / sum(m) * 100), "%)"),
                   pch = 19, col = colors[1:length(m)], title = title
  )
  if (!is.null(graphic_pars[["sleep"]])) {
    Sys.sleep(graphic_pars[["sleep"]])
  }
}
