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

#' Check covariance matrix
#'
#' @description
#' This function checks if the input is a proper covariance matrix, i.e., a
#' symmetric, numeric matrix with non-negative eigenvalues.
#'
#' @param x
#' A \code{matrix}.
#' @param tol
#' A \code{numeric}, a numeric tolerance value.
#' Per default, \code{tol = sqrt(.Machine$double.eps)}.
#'
#' @return
#' A \code{logical}, \code{TRUE} if \code{x} is a proper covariance matrix,
#' \code{FALSE} else.
#'
#' @examples
#' TODO
#' x <- sample_cov_matrix()
#' RprobitB:::is_cov_matrix(x)
#'
#' @keywords internal utils

is_cov_matrix <- function(x, tol = sqrt(.Machine$double.eps)) {
  is.matrix(x) && is.numeric(x) && ncol(x) == nrow(x) &&
    all(abs(x - t(x)) < tol) && all(eigen(x)$value > -tol)
}

#' Sample covariance matrix
#'
#' @description
#' This function samples a covariance matrix from a Wishart distribution.
#'
#' @param dim
#' An \code{integer}, the matrix dimension.
#' @param df
#' An \code{integer}, the degrees of freedom.
#' Must be at least \code{dim}.
#' Per default, \code{df = dim}.
#' @param scale
#' A \code{matrix}, the scale matrix.
#' Must be a covariance matrix.
#' Per default, \code{scale = diag(dim)}.
#'
#' @return
#' A \code{matrix}, a covariance matrix.
#'
#' @examples
#' sample_cov_matrix(dim = 3)
#'
#' @keywords internal utils

sample_cov_matrix <- function(dim, df = dim, scale = diag(dim)) {
  stopifnot(is_int(dim), is_int(df), is_cov_matrix(scale))
  rwishart(nu = df, V = scale)$W # TODO: rename args nu and V in rwishart
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

