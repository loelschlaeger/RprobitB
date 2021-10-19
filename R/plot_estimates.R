#' Visualizing the estimates.
#' @description
#' This function visualizes the parameter estimates and standard deviations.
#' @inheritParams plot.RprobitB_model
#' @return
#' No return value. Draws a plot to the current device.

plot_estimates = function(x, restrict = NULL, ...) {

  ### create coefficient labels
  labels = create_labels(P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
                         C = x$data$C, symmetric = TRUE)

  ###
  pars = names(labels)[!is.na(labels)]
  if(!is.null(restrict))
    pars = intersect(pars, restrict)
  bdata = sapply(x$gibbs_samples$gibbs_samples_nbt[pars], rbind)
  colnames(bdata) = sapply(pars, function(x) paste(x, labels[x], sep = "_"))
  boxplot(bdata, horizontal = TRUE, las = 1)


}
