#' Autocorrelation plot of Gibbs samples.
#' @description
#' This function plots the autocorrelation of the Gibbs samples, including the
#' total sample size \code{SS}, effective sample size \code{ESS} and the factor
#' \code{SS/ESS}.
#' @param gibbs_samples
#' A matrix of Gibbs samples.
#' @param par_labels
#' A character vector of length equal to the number of columns of
#' \code{gibbs_samples}, containing labels for the Gibbs samples.
#' @return
#' No return value. Draws a plot to the current device.
#' @keywords
#' internal

plot_acf = function(gibbs_samples, par_labels){

  for(c in 1:ncol(gibbs_samples)) {
    ### compute autocorrelation and produce plot
    rho = acf(gibbs_samples[,c], las=1, main = "")
    title(par_labels[c], line = -1)

    ### compute effective sample size
    SS = length(gibbs_samples[,c])
    ESS = min(SS/(1+2*sum(rho$acf)),SS)
    legend("topright", x.intersp = -0.5, bg = "white",
           legend = sprintf("%s %.0f",paste0(c("SS", "ESS", "factor"),":"),
                            c(SS,ESS,SS/ESS)))
  }

}
