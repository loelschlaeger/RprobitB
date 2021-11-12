#' Plotting marginal mixing distributions.
#' @description
#' This function plots the estimated mixing distributions with respect to one
#' covariate and adds the true marginal mixing distribution for comparison if
#' available.
#' @param mean_est
#' A list of length \code{C}, where each element is an estimated class mean.
#' @param mean_true
#' Either \code{NULL} or a list of length \code{C}, where each element is a true
#' class mean.
#' @param weight_est
#' A numeric vector of length \code{C} with estimated class weights.
#' @param weight_true
#' Either \code{NULL} or a numeric vector of length \code{C} with true class
#' weights.
#' @param sd_est
#' A list of length \code{C}, where each element is an estimated class standard
#' deviation.
#' @param sd_true
#' Either \code{NULL} or a list of length \code{C}, where each element is a true
#' class standard deviation.
#' @param cov_name
#' Either \code{NULL} or the name of the corresponding covariate.
#' @return
#' No return value. Draws a plot to the current device.
#' @keywords
#' internal

plot_mixture_marginal = function(mean_est, mean_true = NULL, weight_est,
                                 weight_true = NULL, sd_est, sd_true = NULL,
                                 cov_name = NULL){

  ### check if true parameters are available
  true_avail = !(is.null(mean_true) || is.null(weight_true) || is.null(sd_true))

  ### extract number of classes
  stopifnot(length(mean_est) == length(weight_est),
            length(weight_est) == length(sd_est))
  C_est = length(mean_est)
  if(true_avail){
    stopifnot(length(mean_true) == length(weight_true),
              length(weight_true) == length(sd_true))
    C_true = length(mean_true)
  } else {
    C_true = 1
  }

  ### specify x-range
  xlim = c(min(unlist(mean_est) - 3*unlist(sd_est),
               unlist(mean_true) - 3*unlist(sd_true)),
           max(unlist(mean_est) + 3*unlist(sd_est),
               unlist(mean_true) + 3*unlist(sd_true)))
  x = seq(xlim[1], xlim[2], 0.01)

  ### compute mixture components
  mixture_est = matrix(NA, nrow = length(x), ncol = C_est)
  for(c in 1:C_est){
    mixture_est[,c] = weight_est[c] * dnorm(x, mean = mean_est[[c]],
                                            sd = sd_est[[c]])
  }
  if(true_avail){
    mixture_true = matrix(NA, nrow = length(x), ncol = C_true)
    for(c in 1:C_true){
      mixture_true[,c] = weight_true[c] * dnorm(x, mean = mean_true[[c]],
                                                sd = sd_true[[c]])
    }
  }

  ### specify y-range
  ylim = c(0, max(rowSums(mixture_est), if(true_avail) rowSums(mixture_true)))

  ### initialize plot
  plot(0, xlim = xlim, ylim = ylim, type = "n", main = "", xlab = "", ylab = "")
  title(main = "",
        xlab = bquote(paste(beta[.(cov_name)])),
        ylab = "")

  ### add full mixture
  lines(x, rowSums(mixture_est), col = "black", lty=1, lwd=2)
  if(true_avail){
    lines(x, rowSums(mixture_true), col = "black", lty=2, lwd=2)
  }

  ### add mixture components
  col = viridis::magma(n = max(C_est, C_true), begin = 0.1, end = 0.9,
                       alpha = 0.6)
  if(C_est > 1){
    for(c in 1:C_est){
      lines(x, mixture_est[,c], col = col[c], lty = 1, lwd = 2)
    }
  }
  if(true_avail && C_true > 1){
    for(c in 1:C_true){
      lines(x, mixture_true[,c], col = col[c], lty = 2, lwd = 2)
    }
  }

}
