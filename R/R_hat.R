#' Compute Gelman-Rubin statistic
#'
#' @description
#' This function computes the Gelman-Rubin statistic \code{R_hat}.
#'
#' @details
#' NA values in `samples` are ignored. The degenerate case is indicated by `NA`.
#' The Gelman-Rubin statistic is bounded by 1 from below. Values close to 1
#' indicate reasonable convergence.
#'
#' @param samples \[`numeric()` | `matrix`\]\cr
#' Samples from a Markov chain.
#'
#' If it is a matrix, each column gives the samples for a separate chain.
#'
#' @param parts \[`integer(1)`\]\cr
#' The number of parts to divide each chain into sub-chains.
#'
#' @return
#' The Gelman-Rubin statistic.
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
#' @export

R_hat <- function(samples, parts = 2) {

  ### input checks
  oeli::input_check_response(
    check = list(
      oeli::check_numeric_vector(samples),
      checkmate::check_matrix(samples, mode = "numeric")
    ),
    var_name = "samples"
  )
  oeli::input_check_response(
    check = checkmate::check_count(parts, positive = TRUE),
    var_name = "parts"
  )

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
  B <- L / (parts - 1) * sum((chain_means - grand_mean)^2)
  chain_variances <- sapply(sub_chains, stats::var)
  W <- mean(chain_variances)
  if (W == 0) return(NA)
  R_hat <- sqrt(((L - 1) / L * W + B / L) / W)
  max(R_hat, 1)
}
