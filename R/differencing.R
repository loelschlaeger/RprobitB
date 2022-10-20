#' Difference and undifference error term covariance matrix
#'
#' These functions difference and undifference the error term covariance matrix
#' \eqn{Sigma} with respect to a reference alternative \code{diff_alt}.
#'
#' @param Sigma
#' A \code{matrix} of dimension \code{J} x \code{J}, the error term covariance
#' matrix.
#' @param diff_alt
#' An \code{integer} from \code{1} to \code{J}, the reference alternative for
#' utility differencing that maps \code{Sigma} to \code{Sigma_diff}, see
#' details.
#' Per default, \code{diff_alt = 1}.
#'
#' @return
#' A \code{matrix}, the differenced (undifferenced) error term covariance
#' matrix.
#'
#' @examples
#' TODO
#' Sigma <- RprobitB:::sample_cov_matrix()
#' RprobitB:::diff_Sigma(Sigma, diff_alt = 1)
#'
#' @seealso [is_cov_matrix()] to check whether a matrix is a covariance matrix
#' TODO

diff_Sigma <- function(Sigma, diff_alt = 1) {
  stopifnot(is_cov_matrix(Sigma))
  J <- nrow(Sigma_d) + 1
  stopifnot(is_int(diff_alt), diff_alt <= J)
  Sigma <- matrix(0, J, J)
  Sigma[row(Sigma) != diff_alt & col(Sigma) != diff_alt] <- Sigma_d
  Sigma <- Sigma + 1
  stopifnot(is_cov(Sigma))
  Sigma
}

#' undifference covariance matrix Sigma
#' @param Sigma_d covariance matrix
#' @param diff_alt alternative index for differencing
#' @return undifferenced covariance matrix
undiff_Sigma <- function(Sigma_d, diff_alt) {
  stopifnot(is_cov(Sigma_d))
  J <- nrow(Sigma_d) + 1
  stopifnot(is_int(diff_alt), diff_alt <= J)
  Sigma <- matrix(0, J, J)
  Sigma[row(Sigma) != diff_alt & col(Sigma) != diff_alt] <- Sigma_d
  Sigma <- Sigma + 1
  stopifnot(is_cov(Sigma))
  Sigma
}

#' build difference operator
#' @param diff_alt alternative index for differencing
#' @param J number of alternatives
#' @return matrix of dimension (`J`-1) x `J`
delta <- function(diff_alt, J){
  stopifnot(is_int(diff_alt), is_int(J), diff_alt <= J)
  D <- diag(J)
  D[,diff_alt] <- -1
  D[-diff_alt, , drop = FALSE]
}

#' Matrix difference operator
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
#'
#' @return
#' A matrix with \code{J-1} rows.
#'
#' @examples
#' J <- 2
#' x <- matrix(1, nrow = J, ncol = 2)
#' RprobitB:::delta(J, 1) %*% x
#'
#' @export
#'
#' @keywords
#' internal utils

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
#' x <- c(-1,5,10,2)
#' ranking <- order(x, decreasing = TRUE)
#' M(ranking) %*% x
#'
#' @export
#'
#' @keywords
#' internal utils

M <- function(ranking) {
  J <- length(ranking)
  out <- matrix(0, nrow = J-1, ncol = J)
  for(i in 1:(J-1)) {
    out[i,ranking[i]] <- -1
    out[i,ranking[i+1]] <- 1
  }
  return(out)
}
