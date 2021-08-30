#' Visualizing the effects.
#' @description
#' This function visualizes the effects (i.e. the linear coefficients) of the
#' covariates together with an uncertainty interval of plus and minus one
#' standard deviation.
#' @inheritParams plot.RprobitB_model
#' @return
#' No return value. Draws a plot to the current device.

plot_effects = function(x, restrict = NULL, ...){
  cov_names = x$RprobitB_data$covs$names
  effects = data.frame("cov_names" = cov_names,
                       "par_means" = c(x$statistics$alpha[,"mean"],
                                       x$statistics$b[,"mean"]),
                       "par_sds" = c(x$statistics$alpha[,"sd"],
                                     x$statistics$b[,"sd"]))
  if(!is.null(restrict))
    effects = subset(effects, cov_names %in% restrict)
  if(nrow(effects) == 0){
    warning("No effects to visualize. ",
            "Set 'restrict' to 'all' to visualize all effects. ",
            "Or choose a subset of ",paste(cov_names, collapse=", "),".")
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
