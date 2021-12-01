#' Plotting mixing distribution contours.
#' @description
#' This function plots contours of the estimated mixing distributions and adds
#' the true beta values for comparison if available.
#' @param mean_est
#' A list of length \code{C}, where each element is a vector of two
#' estimated class means.
#' @param weight_est
#' A numeric vector of length \code{C} with estimated class weights.
#' @param cov_est
#' A list of length \code{C}, where each element is an estimated class
#' covariance matrix.
#' @param beta_true
#' Either \code{NULL} or a matrix of \code{C} rows with true \code{beta} values.
#' @param cov_names
#' Either \code{NULL} or a vector of two covariate names.
#' @return
#' No return value. Draws a plot to the current device.
#' @keywords
#' internal

plot_mixture_contour <- function(mean_est, weight_est, cov_est, beta_true = NULL,
                                 cov_names = NULL) {

  ### check inputs
  true_avail <- !is.null(beta_true)

  ### extract number of classes
  stopifnot(
    length(mean_est) == length(weight_est),
    length(weight_est) == length(cov_est)
  )
  C_est <- length(mean_est)

  ### specify grid
  xmin <- min(sapply(
    1:C_est,
    function(c) mean_est[[c]][1] - 3 * sqrt(cov_est[[c]][1, 1])
  ))
  xmax <- max(sapply(
    1:C_est,
    function(c) mean_est[[c]][1] + 3 * sqrt(cov_est[[c]][1, 1])
  ))
  ymin <- min(sapply(
    1:C_est,
    function(c) mean_est[[c]][2] - 3 * sqrt(cov_est[[c]][2, 2])
  ))
  ymax <- max(sapply(
    1:C_est,
    function(c) mean_est[[c]][2] + 3 * sqrt(cov_est[[c]][2, 2])
  ))
  grid_x <- seq(xmin, xmax, length.out = 200)
  grid_y <- seq(ymin, ymax, length.out = 200)

  ### compute density of estimated mixture distribution
  prob <- matrix(0, nrow = length(grid_x), ncol = length(grid_y))
  for (i in seq_len(length(grid_x))) {
    for (j in seq_len(length(grid_x))) {
      for (c in 1:C_est) {
        prob[i, j] <- prob[i, j] + weight_est[c] *
          mvtnorm::dmvnorm(
            x = t(matrix(c(grid_x[i], grid_y[j]))),
            mean = mean_est[[c]],
            sigma = cov_est[[c]]
          )
      }
    }
  }

  ### specify limits
  xlim <- c(
    min(grid_x[which(rowSums(prob) > 1e-2)]),
    max(grid_x[which(rowSums(prob) > 1e-2)])
  )
  ylim <- c(
    min(grid_y[which(colSums(prob) > 1e-2)]),
    max(grid_y[which(colSums(prob) > 1e-2)])
  )

  ### initialize plot
  plot(0,
    type = "n", xlim = xlim, ylim = ylim,
    xlab = bquote(paste(beta[.(cov_names[1])])),
    ylab = bquote(paste(beta[.(cov_names[2])])),
    main = ""
  )

  ### add true beta values
  if (true_avail) {
    points(x = beta_true[1, ], y = beta_true[2, ], pch = 16, col = "black")
  }

  ### add contour
  contour(add = TRUE, grid_x, grid_y, prob, labcex = 0.75)
}
