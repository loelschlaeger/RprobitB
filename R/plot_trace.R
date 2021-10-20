#' Visualizing the trace of Gibbs samples.
#' @description
#' This function plots traces of Gibbs samples.
#' @param gibbs_samples
#' A matrix of Gibbs samples.
#' @param names
#' The parameter names.
#' @inheritParams fit
#' @return
#' ...

plot_trace = function(gibbs_samples, names, R, B, Q){
  col = viridis::magma(n = ncol(gibbs_samples), begin=0.1, end=0.9, alpha=0.6)
  plot.ts(gibbs_samples,
          plot.type = "single",
          ylim = c(min(tail(gibbs_samples,n=length(gibbs_samples/2))),max(tail(gibbs_samples,n=length(gibbs_samples/2)))),
          col = col,
          xlab = "",
          ylab = "",
          xaxt = "n",
          las = 1,
          main = "Main")
  at = c(1, (R-B)/Q)
  labels = c("B+1","R")
  axis(side=1, at = at, labels = labels)
  legend("topright", legend = names, lty = 1, col = col, cex = 0.75)
}
