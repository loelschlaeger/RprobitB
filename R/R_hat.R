#' Compute Gelman-Rubin statistic
#'
#' @description
#' This function computes the Gelman-Rubin statistic \code{R_hat}.
#'
#' @references
#' <https://bookdown.org/rdpeng/advstatcomp/monitoring-convergence.html>
#'
#' @param samples
#' A vector or a matrix of samples from a Markov chain, e.g. Gibbs samples.
#' If \code{samples} is a matrix, each column gives the samples for a separate
#' run.
#' @param parts
#' The number of parts to divide each chain into sub-chains.
#'
#' @return
#' A numeric value, the Gelman-Rubin statistic.
#'
#' @examples
#' no_chains <- 2
#' length_chains <- 1e3
#' samples <- matrix(NA_real_, length_chains, no_chains)
#' samples[1, ] <- 1
#' Gamma <- matrix(c(0.8, 0.1, 0.2, 0.9), 2, 2)
#' for (c in 1:no_chains) {
#'   for (t in 2:length_chains) {
#'     samples[t, c] <- sample(1:2, 1, prob = Gamma[samples[t - 1, c], ])
#'   }
#' }
#' R_hat(samples)
#'
#' @keywords
#' utils
#'
#' @export
#'
#' @importFrom stats var

R_hat <- function(samples, parts = 2) {
  ### divide chains into parts
  samples <- as.matrix(samples)
  no_chains <- ncol(samples)
  length_chains <- nrow(samples)
  sub_chains <- list()
  for (c in 1:no_chains) {
    sub_chains <- c(
      sub_chains,
      split(samples[, c], cut(1:length_chains, parts))
    )
  }

  ### compute and return the Gelman-Rubin statistic
  L <- length_chains / parts
  chain_means <- sapply(sub_chains, mean)
  grand_mean <- mean(chain_means)
  B <- 1 / (parts - 1) * sum((chain_means - grand_mean)^2)
  chain_variances <- sapply(sub_chains, stats::var)
  W <- sum(chain_variances) / parts
  R_hat <- ((L - 1) / L * W + B) / W
  return(R_hat)
}
