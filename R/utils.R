#' Extract function body as character
#'
#' @description
#' This function extracts the body of a function as a character.
#'
#' @param fun
#' An object of class \code{function}.
#' @param braces
#' A boolean, if \code{FALSE} (default) removes \code{"{"} and \code{"}"} at
#' start and end (if any).
#' @param nchar
#' An integer, the maximum number of characters before abbreviation.
#'
#' @return
#' A character.
#'
#' @keywords utils
#'
#' @examples
#' add1 <- function(x) {
#'   stopifnot(is.numeric(x))
#'   x + 1
#' }
#' RprobitB:::function_body(add1)
#' RprobitB:::function_body(add1, braces = TRUE)
#' RprobitB:::function_body(add1, nchar = 20)

function_body <- function(fun, braces = FALSE, nchar = 100) {
  stopifnot(is.function(fun))
  out <- deparse1(body(fun))
  if (!braces) out <- gsub("^[{]|[}]$","", out)
  out <- trimws(gsub("\\s+", " ", out))
  if (nchar(out) > nchar) out <- paste0(strtrim(out, nchar - 3), '...')
  out
}

#' check for boolean
#' @param i any element
#' @return `TRUE` if `i` is either `TRUE` or `FALSE`, `FALSE` else
is_bool <- function(i) {
  length(i) == 1 && isTRUE(i) || isFALSE(i)
}

#' Check for single numeric
#'
#' @description
#' This function checks whether the input is a single numeric value.
#'
#' @param x
#' Any element.
#'
#' @return
#' \code{TRUE} if \code{x} is a single numeric, \code{FALSE} else.
#'
#' @keywords utils
#'
#' @examples
#' is_sn(1)
#' is_sn("1")
#' is_sn(NA_real_)

is_sn <- function(i) {
  is.numeric(i) && length(i) == 1 && !is.na(i)
}

#' check for positive integer
#' @param i any element
#' @return `TRUE` if `i` is an integer, `FALSE` else
is_int <- function(i) {
  is_sn(i) && i %% 1 == 0 && i > 0
}

#' check for covariance matrix
#' @param M any element
#' @return `TRUE` if `M` is a covariance matrix, `FALSE` else
is_cov <- function(M) {
  is.matrix(M) && is.numeric(M) && ncol(M) == nrow(M) &&
    all(abs(M - t(M)) < sqrt(.Machine$double.eps)) &&
    all(eigen(M)$value > -sqrt(.Machine$double.eps))
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

#' Check covariance matrix properties
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
#' @examples
#' x <- diag(2)
#' RprobitB:::is_covariance_matrix(x)
#' @keywords
#' internal utils

is_covariance_matrix <- function(x) {
  is.matrix(x) &&
    is.numeric(x) &&
    ncol(x) == nrow(x) &&
    all(abs(x - t(x)) < sqrt(.Machine$double.eps)) &&
    all(eigen(x)$value > -sqrt(.Machine$double.eps))
}

#' Permutations of a vector
#'
#' @description
#' This function returns all permutations of a given vector.
#'
#' @references
#' This function is a modified version of
#' <https://stackoverflow.com/a/20199902/15157768>.
#'
#' @param x
#' A vector.
#'
#' @return
#' A list of all permutations of \code{x}.
#'
#' @examples
#' RprobitB:::permutations(x = c("a","b","c"))
#'
#' @keywords
#' internal utils

permutations <- function(x){
  perm_index <- function(n){
    if(n==1){
      return(matrix(1))
    } else {
      sp <- perm_index(n-1)
      p <- nrow(sp)
      A <- matrix(nrow=n*p,ncol=n)
      for(i in 1:n){
        A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
      }
      return(A)
    }
  }
  p <- perm_index(length(x))
  out <- list()
  for(i in 1:nrow(p)) {
    out <- c(out, list(x[p[i,]]))
  }
  return(out)
}

