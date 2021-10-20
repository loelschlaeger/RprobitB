#' Autocorrelation plots
#' @description
#' Function that plots the autocorrelation of the Gibbs samples.
#' @details
#' Function computes the [effective sample size](https://mc-stan.org/docs/2_18/reference-manual/effective-sample-size-section.html).
#' @param gibbs_samples
#' A list of Gibbs samples.
#' @return
#' ...

plot_acf = function(gibbs_samples, names){

  for(c in 1:ncol(gibbs_samples)) {
    ###
    rho = acf(gibbs_samples[,c], las=1, main = names[c])

    ### compute effective sample size
    SS = length(gibbs_samples[,c])
    ESS = min(SS/(1+2*sum(rho$acf)),SS)
    legend("topright", x.intersp = -0.5, bg = "white",
           legend = sprintf("%s %.0f",paste0(c("total sample size","effective sample size","factor"),":"),c(SS,ESS,SS/ESS)))
  }

}
