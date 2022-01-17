#' Matrix difference operator.
#'
#' @description
#' This function creates the difference operator matrix \code{delta} for
#' subtracting a matrix row from the other matrix rows.
#'
#' @details
#' Given a matrix \code{x} with \code{J} rows, then \code{delta(i,J) %*% x}
#' computes differences with respect to row \code{i}.
#'
#' @param J
#' The number of matrix rows.
#' @param i
#' The row number to which respect differences are computed.
#' @return
#' A matrix with \code{J-1} rows.
#' @export
#' @examples
#' J <- 2
#' x <- matrix(1, nrow = J, ncol = 2)
#' delta(J, 1) %*% x
#' @keywords
#' utils

delta <- function(J, i) {
  stopifnot(is.numeric(J), J %% 1 == 0, J >= 2)
  stopifnot(is.numeric(i), i %% 1 == 0, i >= 1)
  stopifnot(J >= 2)
  stopifnot(J >= i)
  if (i == 1) {
    Delta <- cbind(-1, diag(J - 1))
  } else if (i == J) {
    Delta <- cbind(diag(J - 1), -1)
  } else {
    Delta <- cbind(diag(J - 1)[, 1:(i - 1)], -1, diag(J - 1)[, i:(J - 1)])
  }
  return(Delta)
}

#' Balancing visualization of multiple figures.
#'
#' @description
#' This function finds a balanced setting for \code{par(mfrow)}.
#'
#' @param n
#' The total number of figures.
#' @return
#' A vector of the form \code{c(nr,nc)}. If \code{par(mfrow = c(nr,nc))},
#' subsequent figures will be drawn in an \code{nr} x \code{nc} array on the
#' current device by rows.
#'
#' @export
#'
#' @examples
#' set_mfrow(3)
#' @keywords
#' utils

set_mfrow <- function(n) {
  if (n == 1) {
    return(c(1, 1))
  }
  ran <- 2:max(floor((n - 1) / 2), 1)
  ran2 <- pmax(ceiling(n / (ran)), 1)
  rem <- abs(n - ran2 * ran)
  score <- abs(sqrt(n) - (ran)) + abs(sqrt(n) - (ran2)) + rem
  nr <- ran[which.min(score)]
  nc <- ran2[which.min(score)]
  return(c(nr, nc))
}

#' Compute Gelman-Rubin statistic.
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
#' The Gelman-Rubin statistic.
#'
#' @examples
#' no_chains <- 2
#' length_chains <- 1e3
#' samples <- matrix(NA, length_chains, no_chains)
#' samples[1, ] <- 1
#' Gamma <- matrix(c(0.8, 0.1, 0.2, 0.9), 2, 2)
#' for (c in 1:no_chains) {
#'   for (t in 2:length_chains) {
#'     samples[t, c] <- sample(1:2, 1, prob = Gamma[samples[t - 1, c], ])
#'   }
#' }
#' R_hat(samples)
#' @export
#'
#' @keywords
#' utils
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

#' Check covariance matrix properties.
#'
#' @description
#' This function checks if the input is a proper covariance matrix, i.e. a
#' symmetric, numeric matrix with non-negative eigenvalues.
#'
#' @param x
#' A matrix.
#'
#' @return
#' A boolean, \code{TRUE} if \code{x} is a proper covariance matrix.
#'
#' @export
#'
#' @examples
#' x <- diag(2)
#' is_covariance_matrix(x)
#' @keywords
#' utils

is_covariance_matrix <- function(x) {
  is.matrix(x) &&
    is.numeric(x) &&
    ncol(x) == nrow(x) &&
    all(abs(x - t(x)) < sqrt(.Machine$double.eps)) &&
    all(eigen(x)$value > -sqrt(.Machine$double.eps))
}
