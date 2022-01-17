#' Compute sufficient statistics.
#' @description
#' This function computes sufficient statistics from \code{data} for estimation.
#' @inheritParams mcmc
#' @param normalization
#' An object of class \code{RprobitB_normalization}.
#' @return
#' A list of sufficient statistics.
#' @keywords
#' internal

sufficient_statistics <- function(data, normalization) {

  ### check input
  if (!inherits(data, "RprobitB_data")) {
    stop("'data' must be of class 'RprobitB_data'.")
  }
  if (!inherits(normalization, "RprobitB_normalization")) {
    stop("'normalization' must be of class 'RprobitB_normalization'.")
  }

  ### extract parameters
  N <- data$N
  T <- if (length(data$T) == 1) rep(data$T, N) else data$T
  J <- data$J
  P_r <- data$P_r
  P_f <- data$P_f

  ### compute utility differences with respect to 'normalization$level'
  for (n in seq_len(N)) {
    for (t in seq_len(T[n])) {
      data$data[[n]]$X[[t]] <- delta(J, normalization$level) %*% data$data[[n]]$X[[t]]
    }
  }

  ### compute sufficient statistics
  y <- matrix(0, nrow = N, ncol = max(T))
  for (n in 1:N) {
    y_n <- data$data[[n]][[2]]
    y[n, ] <- c(y_n, rep(NA, max(T) - length(y_n)))
  }
  W <- list()
  X <- list()
  if (P_f > 0 & P_r > 0) {
    for (n in seq_len(N)) {
      for (t in seq_len(T[n])) {
        W[[sum(T[seq_len(n - 1)]) + t]] <-
          data$data[[n]][[1]][[t]][, seq_len(P_f), drop = FALSE]
        X[[sum(T[seq_len(n - 1)]) + t]] <-
          data$data[[n]][[1]][[t]][, -seq_len(P_f), drop = FALSE]
      }
    }
  }
  if (P_f > 0 & P_r == 0) {
    X <- NA
    for (n in seq_len(N)) {
      for (t in seq_len(T[n])) {
        W[[sum(T[seq_len(n - 1)]) + t]] <- data$data[[n]][[1]][[t]]
      }
    }
  }
  if (P_f == 0 & P_r > 0) {
    W <- NA
    for (n in seq_len(N)) {
      for (t in seq_len(T[n])) {
        X[[sum(T[seq_len(n - 1)]) + t]] <- data$data[[n]][[1]][[t]]
      }
    }
  }
  XkX <- NA
  if (P_r > 0) {
    XkX <- list()
    for (n in seq_len(N)) {
      XnkXn <- 0
      for (t in seq_len(T[n])) {
        XnkXn <- XnkXn +
          kronecker(
            t(X[[sum(T[seq_len(n - 1)]) + t]]),
            t(X[[sum(T[seq_len(n - 1)]) + t]])
          )
      }
      XkX[[n]] <- XnkXn
    }
  }
  WkW <- NA
  if (P_f > 0) {
    WkW <- matrix(0, nrow = P_f^2, ncol = (J - 1)^2)
    for (n in seq_len(N)) {
      for (t in seq_len(T[n])) {
        WkW <- WkW +
          kronecker(
            t(W[[sum(T[seq_len(n - 1)]) + t]]),
            t(W[[sum(T[seq_len(n - 1)]) + t]])
          )
      }
    }
  }

  ### build and return 'suff_statistics'
  suff_statistics <- list(
    "Tvec" = T,
    "csTvec" = cumsum(T) - T,
    "W" = W,
    "X" = X,
    "y" = y,
    "XkX" = XkX,
    "WkW" = WkW
  )
  return(suff_statistics)
}
