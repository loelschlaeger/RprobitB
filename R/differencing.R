#' Difference and undifference error term covariance matrix
#'
#' These functions difference and undifference the error term covariance matrix
#' \eqn{\Sigma} with respect to a reference alternative \code{diff_alt}.
#'
#' @param Sigma
#' A \code{matrix} of dimension \code{J} x \code{J}, the error term covariance
#' matrix.
#' @param diff_alt
#' An \code{integer} from \code{1} to \code{J}, the reference alternative for
#' utility differencing that maps \code{Sigma} to \code{Sigma_diff}, see
#' details.
#' Bydefault, \code{diff_alt = 1}.
#'
#' @return
#' A \code{matrix}, the differenced (undifferenced) error term covariance
#' matrix.
#'
#' @details
#' # Difference and undifference error term covariance matrix
#' The probit model equation
#' \deqn{U_{ntj} = X_{ntj}' \tilde{\beta}_n + \epsilon_{ntj},}
#' with \eqn{(\epsilon_{nt:}) \sim \text{MVN}_{J} (0,\Sigma)}
#' is invariant towards the level of utility, hence we take
#' utility differences with respect to some reference alternative \eqn{k}
#' (also denoted by \code{diff_alt}). The resulting error term differences again
#' are multivariate normally distributed with mean \eqn{0} and transformed
#' covariance matrix \eqn{\tilde{\Sigma}}, also denoted by \code{Sigma_diff}.
#'
#' For differencing:
#' \deqn{\tilde{\Sigma} = \Delta_k \Sigma \Delta_k',}
#' where \eqn{\Delta_k} is a difference operator that depends on the reference
#' alternative \eqn{k}, see below.
#'
#' The "undifferenced" covariance matrix \eqn{\Sigma} cannot be uniquely
#' computed from \eqn{\tilde{\Sigma}}.
#' For one solution, we add a column and a row of zeros
#' at column and row number \eqn{k} to \eqn{\tilde{\Sigma}}, respectively, and
#' add \eqn{1} to each matrix entry to make the result a proper covariance
#' matrix.
#'
#' # Difference operator
#' The matrix \eqn{\Delta_k} in equation
#' \deqn{\tilde{\Sigma} = \Delta_k \Sigma \Delta_k'}
#' is a matrix of dimension \eqn{(J-1)\times J}.
#' It is the unit matrix of dimension \eqn{J} without row \eqn{k} and with
#' \eqn{-1}s in column \eqn{k}.
#' It can be computed with \code{delta(diff_alt, J)}, where \code{diff_alt}
#' denotes \eqn{k}.
#'
#' @examples
#' J <- 3
#' diff_alt <- 2
#' delta(diff_alt, J)
#' (Sigma0 <- RprobitB:::sample_covariance_matrix(dim = J))
#' (Sigma_diff0 <- RprobitB:::diff_Sigma(Sigma0, diff_alt = diff_alt))
#' (Sigma1 <- RprobitB:::undiff_Sigma(Sigma_diff0, diff_alt = diff_alt))
#' (Sigma_diff1 <- RprobitB:::diff_Sigma(Sigma1, diff_alt = diff_alt))
#' all.equal(Sigma_diff0, Sigma_diff1)
#'
#' @seealso [is_covariance_matrix()] to check whether a matrix is a covariance matrix

diff_Sigma <- function(Sigma, diff_alt = 1) {
  stopifnot(is_covariance_matrix(Sigma))
  J <- nrow(Sigma)
  stopifnot(is_positive_integer(diff_alt), diff_alt <= J)
  D <- delta(diff_alt = diff_alt, J = J)
  D %*% Sigma %*% t(D)
}

#' @rdname diff_Sigma
#' @param Sigma_diff
#' A \code{matrix} of dimension \code{J-1} x \code{J-1}, the differenced error
#' term covariance matrix.

undiff_Sigma <- function(Sigma_diff, diff_alt = 1) {
  stopifnot(is_covariance_matrix(Sigma_diff))
  J <- nrow(Sigma_diff) + 1
  stopifnot(is_positive_integer(diff_alt), diff_alt <= J)
  Sigma <- matrix(0, J, J)
  Sigma[row(Sigma) != diff_alt & col(Sigma) != diff_alt] <- Sigma_diff
  Sigma + 1
}

#' @rdname diff_Sigma
#' @param J
#' An \code{integer}, number of alternatives.
#' @keywords internal

delta <- function(diff_alt, J) {
  stopifnot(is_positive_integer(diff_alt), is_positive_integer(J), diff_alt <= J)
  D <- diag(J)
  D[, diff_alt] <- -1
  D[-diff_alt, , drop = FALSE]
}


### TODO Not touched from here


#' Matrix difference operator for ranked vectors
#'
#' @description
#' This function creates the difference operator matrix for differencing ranked
#' vector elements such that the resulting vector is negative.
#'
#' @param ranking
#' A numeric vector of the ranking in decreasing order.
#'
#' @return
#' A matrix of dimension \code{length(rank)-1} x \code{length(rank)}.
#'
#' @examples
#' x <- c(-1, 5, 10, 2)
#' ranking <- order(x, decreasing = TRUE)
#' M(ranking) %*% x
#'
#' @export
#'
#' @keywords
#' internal utils

M <- function(ranking) {
  J <- length(ranking)
  out <- matrix(0, nrow = J - 1, ncol = J)
  for (i in 1:(J - 1)) {
    out[i, ranking[i]] <- -1
    out[i, ranking[i + 1]] <- 1
  }
  return(out)
}
