#' Visualizing the linear effects.
#' @description
#' This function visualizes the linear effects of the covariates on the choices
#' together with an uncertainty interval of plus / minus one standard deviation.
#' @param x
#' An object of class \code{RprobitB_gibbs_samples}.
#' @param coeff_names
#' A character vector of coefficient names.
#' @return
#' No return value. Draws a plot to the current device.
#' @keywords
#' internal

plot_effects = function(x, coeff_names){

  ### extract means and sds
  means = unlist(RprobitB_gibbs_samples_statistics(x,list(mean))[c("alpha","b")])
  sds = unlist(RprobitB_gibbs_samples_statistics(x, list(sd))[c("alpha","b")])

  ### determin coefficient labels
  labels = coeff_names

  ### plot means
  xlim = c(min(c(means - sds),0), max(c(means + sds),0))
  plot(x = means, y = 1:length(means),
       yaxt = "n", ylab = "", xlab = "", xlim = xlim, main = "")

  ### add uncertainty interval
  axis(2, at = 1:length(means), labels = labels, las = 1)
  for(n in 1:length(means))
    segments(x0 = means[n] - sds[n], y0 = n, x1 = means[n] + sds[n], y1 = n)

  ### mark zero
  abline(v = 0, lty = 2)
}
