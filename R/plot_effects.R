#' Visualizing the effects.
#' @description
#' This function visualizes the effects of the covariates on the choices (i.e.
#' the linear coefficients) along with an uncertainty interval of plus and minus
#' one standard deviation.
#' @inheritParams plot.RprobitB_model
#' @return
#' No return value. Draws a plot to the current device.

plot_effects = function(x, restrict = NULL, ...){
  cov_names = x$data$covs$names
  par_means = c()
  par_sds = c()
  if(x$data$P_f > 0){
    par_means = c(par_means, summary(x)$parameter_statistics[["alpha"]][,"mean"])
    par_sds = c(par_sds, summary(x)$parameter_statistics[["alpha"]][,"sd"])
  }
  if(x$data$P_r > 0){
    par_means = c(par_means, summary(x)$parameter_statistics[["b"]][,"mean"])
    par_sds = c(par_sds, summary(x)$parameter_statistics[["b"]][,"sd"])
  }
  effects = data.frame("cov_names" = cov_names,
                       "par_means" = par_means,
                       "par_sds" = par_sds)
  if(!is.null(restrict))
    effects = subset(effects, cov_names %in% restrict)
  if(nrow(effects) == 0){
    warning("No effects to visualize.")
  } else {
    xlim = c(min(c(effects$par_means - effects$par_sds),0),
             max(c(effects$par_means + effects$par_sds),0))
    plot(x = effects$par_means, y = 1:length(effects$cov_names),
         yaxt = "n", ylab = "", xlab = "", xlim = xlim, main = "Effects")
    axis(2, at = 1:length(effects$cov_names), labels = effects$cov_names, las = 1)
    for(n in 1:length(effects$cov_names))
      segments(x0 = effects$par_means[n] - effects$par_sds[n], y0 = n,
               x1 = effects$par_means[n] + effects$par_sds[n], y1 = n)
    abline(v = 0, lty = 2)
  }
}
