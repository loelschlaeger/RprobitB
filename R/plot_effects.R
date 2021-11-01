#' Visualizing the effects.
#' @description
#' This function visualizes the effects of the covariates on the choices (i.e.
#' the linear coefficients) along with an uncertainty interval of plus and minus
#' one standard deviation.
#' @param x
#' An object of class \code{RprobitB_gibbs_samples}.
#' @param par_names
#' ...
#' @param cov_names
#' A character vector of covariate names.
#' @return
#' No return value. Draws a plot to the current device.

plot_effects = function(x, par_names, cov_names = NULL){
  means = unlist(RprobitB_gibbs_samples_statistics(x, list(mean))[par_names])
  sds = unlist(RprobitB_gibbs_samples_statistics(x, list(sd))[par_names])
  xlim = c(min(c(means - sds),0), max(c(means + sds),0))
  plot(x = means, y = 1:length(means),
       yaxt = "n", ylab = "", xlab = "", xlim = xlim, main = "")
  axis(2, at = 1:length(means), labels = names(means), las = 1)
  for(n in 1:length(means))
    segments(x0 = means[n] - sds[n], y0 = n, x1 = means[n] + sds[n], y1 = n)
  abline(v = 0, lty = 2)
}
