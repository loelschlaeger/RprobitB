#' Compute Gelman-Rubin statistic.
#' @description
#' This function computes the Gelman-Rubin statistic \code{R_hat}.
#' @references
#' <https://bookdown.org/rdpeng/advstatcomp/monitoring-convergence.html>
#' @param samples
#' Normalized and burned but not thinned Gibbs samples.
#' @param parts
#' The number of parts to divide the samples into sub-chains.
#' @return
#' The Gelman-Rubin statistic.

compute_R_hat = function(samples, parts=2){
  sub_chains = split(samples,cut(seq_along(samples),parts,labels=FALSE))
  L = length(samples)/parts
  chain_means = sapply(sub_chains,mean)
  grand_mean = mean(chain_means)
  B = 1/(parts-1)*sum((chain_means-grand_mean)^2)
  chain_variances = sapply(sub_chains,var)
  W = sum(chain_variances)/parts
  R_hat = ((L-1)/L*W+B)/W
  return(R_hat)
}
