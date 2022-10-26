#' Simulate choice data
#'
#' This function simulates choice data from a probit model, see details.
#'
#' @inheritParams RprobitB_formula
#' @param N
#' An \code{integer}, the number of deciders.
#' @param T
#' An \code{integer} of length \code{N}, the number of choice occasions per
#' decider.
#' Can also be a single \code{integer} for a constant number of choice occasions
#' per decider.
#' By default, \code{T = 1}.
#' @param covariates
#'
#' @details
#' # Choice simulation
#' TODO
#' @inheritSection RprobitB_formula Model formula
#' @inheritSection RprobitB_formula Random effects

simulate_choices <- function(
    formula, N, T = 1, J, alternatives = LETTERS[1:J], re = NULL,
    ordered = FALSE, ranked = FALSE,
    covariates = function(n, t) {
      P <- P(formula, re, J)
      matrix(rnorm(P * J, mean = 0, sd = 9), nrow = P, ncol = J)
    },
    seed = NULL,
    true_parameter = RprobitB_parameter()
  ) {





  stopifnot(is_int(N), is_int(T), is_int(J), J >= 2, is_int(P))
  stopifnot(is.numeric(b), length(b) == P)
  stopifnot(is.null(Omega) || (is_cov(Omega) && all(dim(Omega) == P)))
  stopifnot(is_cov(Sigma), all(dim(Sigma) == J))
  stopifnot(is.function(X), is.matrix(X()), dim(X()) == c(J,P))



  b <- matrix(b)
  mix <- !(is.null(Omega) || all(Omega == 0))
  if(mix) {
    O <- t(chol(Omega))
    o <- O[lower.tri(O, diag = TRUE)]
  }
  ### normalize Sigma
  D <- delta(diff_alt, J)
  Sigma.d <- D %*% Sigma %*% t(D)
  L.d <- t(chol(Sigma.d))
  l.d <- L.d[lower.tri(L.d, diag = TRUE)]
  Sigma <- undiff_Sigma(Sigma.d, diff_alt)
  L <- t(chol(Sigma))
  beta <- lapply(1:N, function(x) if(mix) b + O %*% stats::rnorm(P) else b)
  structure(
    lapply(1:N, function(n) {
      out <- lapply(1:T, function(t) {
        X_nt <- X()
        U_nt <- X_nt %*% beta[[n]] + L %*% stats::rnorm(J)
        y_nt <- which.max(U_nt)
        list(X = X_nt, y = y_nt)
      })
      list(X = lapply(out, `[[`, "X"), y = sapply(out, `[[`, "y"))
    }),
    "class" = "probit_data",
    "true" = structure(
      c(b, if(mix) o, l.d),
      "class" = "theta",
      "b" = as.numeric(b), "o" = if(mix) o else NULL, "l" = l.d
    ),
    "N" = N, "T" = T, "J" = J, "P" = P, "mix" = mix, "diff_alt" = diff_alt
  )
}
