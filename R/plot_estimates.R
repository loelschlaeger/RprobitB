#' Visualizing the effects.
#' @description
#' This function visualizes the effects of the covariates on the choices (i.e.
#' the linear coefficients) along with an uncertainty interval of plus and minus
#' one standard deviation.
#' @param x
#' An object of class \code{RprobitB_gibbs_samples}.
#' @param par_names
#' ...
#' @return
#' No return value. Draws a plot to the current device.

plot_estimates = function(x, par_names){
  gibbs_samples_matrix_nbt = sapply(x$gibbs_samples_nbt, rbind)
  colnames(gibbs_samples_matrix_nbt) = paste0(par_names, "_",
                                             sapply(x$gibbs_samples_nbt, colnames))
  boxplot(gibbs_samples_matrix_nbt)
}
