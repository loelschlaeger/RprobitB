#' Set initial values for the Gibbs sampler
#'
#' @description
#' This function sets initial values for the Gibbs sampler.
#'
#' @inheritParams RprobitB_data
#' @param C
#' The number (greater or equal 1) of latent classes.
#' @param suff_stat
#' Optionally the output of \code{\link{sufficient_statistics}}.
#'
#' @return
#' A list of initial values for the Gibbs sampler.
#'
#' @keywords
#' internal
#'
#' @examples
#' RprobitB:::set_initial_gibbs_values(
#'   N = 2, T = 3, J = 3, P_f = 1, P_r = 2, C = 2
#' )
set_initial_gibbs_values <- function(
    N, T, J, P_f, P_r, C, ordered = FALSE, suff_stat = NULL) {
  ### check inputs
  stopifnot(is.numeric(N), N %% 1 == 0, N > 0)
  stopifnot(is.numeric(T), T %% 1 == 0, T > 0)
  stopifnot(is.numeric(P_f), P_f %% 1 == 0, P_f >= 0)
  stopifnot(is.numeric(P_r), P_r %% 1 == 0, P_r >= 0)
  stopifnot(is.numeric(C), C %% 1 == 0, C > 0)
  stopifnot(is.logical(ordered))

  ### define initial values
  alpha0 <- if (P_f > 0) numeric(P_f) else NA
  z0 <- if (P_r > 0) rep(1, N) else NA
  m0 <- if (P_r > 0) round(rep(N, C) * 2^(C:1 - 1) / sum(2^(C:1 - 1))) else NA
  b0 <- if (P_r > 0) matrix(0, nrow = P_r, ncol = C) else NA
  Omega0 <- if (P_r > 0) {
    matrix(rep(as.vector(diag(P_r)), C),
           nrow = P_r * P_r,
           ncol = C
    )
  } else {
    NA
  }
  beta0 <- if (P_r > 0) matrix(0, nrow = P_r, ncol = N) else NA
  U0 <- matrix(0, nrow = J - 1, ncol = N * max(T))
  Sigma0 <- diag(J - 1)

  ### special case of ordered probit
  if (ordered) {
    d0 <- rep(0, J - 2)
    U0 <- matrix(0, nrow = 1, ncol = N * max(T))
    Sigma0 <- diag(1)
    if (!is.null(suff_stat)) {
      if (P_f > 0) {
        W_mat <- as.matrix(do.call(rbind, suff_stat$W))
        alpha0 <- as.numeric(solve(t(W_mat) %*% W_mat) %*% t(W_mat) %*% na.omit(as.numeric(t(suff_stat$y))))
      }
      if (P_r > 0) {
        X_mat <- as.matrix(do.call(rbind, suff_stat$X))
        b0 <- as.numeric(solve(t(X_mat) %*% X_mat) %*% t(X_mat) %*% na.omit(as.numeric(t(suff_stat$y))))
        b0 <- matrix(rep(b0, times = C), nrow = P_r, ncol = C)
      }
    }
  } else {
    d0 <- NA
  }

  ### define 'init'
  init <- list(
    "alpha0" = alpha0,
    "z0" = z0,
    "m0" = m0,
    "b0" = b0,
    "Omega0" = Omega0,
    "beta0" = beta0,
    "U0" = U0,
    "Sigma0" = Sigma0,
    "d0" = d0
  )

  ### return 'init'
  return(init)
}
