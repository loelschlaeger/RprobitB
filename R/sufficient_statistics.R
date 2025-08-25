#' Compute sufficient statistics
#'
#' @description
#' This function computes sufficient statistics from an `RprobitB_data`
#' object for the Gibbs sampler to save computation time.
#'
#' @param data
#' An object of class `RprobitB_data`.
#' @param normalization
#' An object of class `RprobitB_normalization`, which can be created
#' via \code{\link{RprobitB_normalization}}.
#'
#' @return
#' A list of sufficient statistics on the data for Gibbs sampling, containing
#' \itemize{
#'   \item the elements \code{N}, \code{T}, \code{J}, \code{P_f} and \code{P_r}
#'         from \code{data},
#'   \item \code{Tvec}, the vector of choice occasions for each decider of
#'         length \code{N},
#'   \item \code{csTvec}, a vector of length \code{N} with the cumulated sums of
#'         \code{Tvec} starting from \code{0},
#'   \item \code{W}, a list of design matrices differenced with respect to
#'         alternative number \code{normalization$level$level}
#'         for each decider in each choice occasion with covariates that
#'         are linked to a fixed coefficient (or \code{NA} if \code{P_f = 0}),
#'   \item \code{X}, a list of design matrices differenced with respect to
#'         alternative number \code{normalization$level$level}
#'         for each decider in each choice occasion with covariates that
#'         are linked to a random coefficient (or \code{NA} if \code{P_r = 0}),
#'   \item \code{y}, a matrix of dimension \code{N} x \code{max(Tvec)} with the
#'         observed choices of deciders in rows and choice occasions in columns,
#'         decoded to numeric values with respect to their appearance in
#'         \code{data$alternatives}, where rows are filled with \code{NA} in
#'         case of an unbalanced panel,
#'   \item \code{WkW}, a matrix of dimension \code{P_f^2} x \code{(J-1)^2}, the
#'         sum over Kronecker products of each transposed element in \code{W}
#'         with itself,
#'   \item \code{XkX}, a list of length \code{N}, each element is constructed in
#'         the same way as \code{WkW} but with the elements in \code{X} and
#'         separately for each decider,
#'   \item \code{rdiff} (for the ranked case only), a list of matrices that
#'         reverse the base differencing and instead difference in such a way
#'         that the resulting utility vector is negative.
#' }
#'
#' @keywords internal

sufficient_statistics <- function(data, normalization) {

  ### check input
  if (!inherits(data, "RprobitB_data")) {
    stop("'data' must be of class 'RprobitB_data'.", call. = FALSE)
  }
  if (!inherits(normalization, "RprobitB_normalization")) {
    stop("'normalization' must be of class 'RprobitB_normalization'.",
         call. = FALSE
    )
  }

  ### make a copy of 'data'
  data_copy <- data

  ### extract parameters
  N <- data_copy$N
  T <- data_copy$T
  Tvec <- if (length(T) == 1) rep(T, N) else T
  J <- data_copy$J
  P_f <- data_copy$P_f
  P_r <- data_copy$P_r

  ### compute utility differences with respect to 'normalization$level$level'
  ### (not for an ordered probit model)
  RprobitB_pp("Computing sufficient statistics", 0, 4)
  if (!data$ordered) {
    delta_level <- oeli::delta(ref = normalization$level$level, dim = J)
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        data_copy$data[[n]]$X[[t]] <- delta_level %*% data_copy$data[[n]]$X[[t]]
      }
    }
  }

  ### decode choice to numeric with respect to appearance
  RprobitB_pp("Computing sufficient statistics", 1, 4)
  y <- matrix(0, nrow = N, ncol = max(Tvec))
  if (data$ranked) {
    choice_set <- sapply(oeli::permutations(data$alternatives), paste, collapse = ",")
  } else {
    choice_set <- data$alternatives
  }
  for (n in 1:N) {
    y_n <- match(data_copy$data[[n]][[2]], choice_set)
    y[n, ] <- c(y_n, rep(NA, max(Tvec) - length(y_n)))
  }

  ### extract covariates linked to fixed ('W') and to random coefficients ('X')
  RprobitB_pp("Computing sufficient statistics", 2, 4)
  W <- list()
  X <- list()
  if (P_f > 0 & P_r > 0) {
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        W[[sum(Tvec[seq_len(n - 1)]) + t]] <-
          data_copy$data[[n]][[1]][[t]][, seq_len(P_f), drop = FALSE]
        X[[sum(Tvec[seq_len(n - 1)]) + t]] <-
          data_copy$data[[n]][[1]][[t]][, -seq_len(P_f), drop = FALSE]
      }
    }
  }
  if (P_f > 0 & P_r == 0) {
    X <- NA
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        W[[sum(Tvec[seq_len(n - 1)]) + t]] <- data_copy$data[[n]][[1]][[t]]
      }
    }
  }
  if (P_f == 0 & P_r > 0) {
    W <- NA
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        X[[sum(Tvec[seq_len(n - 1)]) + t]] <- data_copy$data[[n]][[1]][[t]]
      }
    }
  }
  if (data$ordered) {
    if (!identical(W, NA)) {
      W <- lapply(W, function(x) matrix(as.numeric(x), nrow = 1))
    }
    if (!identical(X, NA)) {
      X <- lapply(X, function(x) matrix(as.numeric(x), nrow = 1))
    }
  }

  ### compute \sum kronecker(t(W_nt),t(W_nt)) for each W_nt in W
  RprobitB_pp("Computing sufficient statistics", 3, 4)
  WkW <- NA
  if (P_f > 0) {
    WkW <- if (data$ordered) {
      matrix(0, nrow = P_f^2, ncol = 1)
    } else {
      matrix(0, nrow = P_f^2, ncol = (J - 1)^2)
    }
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        var <- W[[sum(Tvec[seq_len(n - 1)]) + t]]
        if (data$ordered) {
          var <- as.numeric(var)
          WkW <- WkW + t(kronecker(t(var), t(var)))
        } else {
          WkW <- WkW + kronecker(t(var), t(var))
        }
      }
    }
  }

  ### for each n, compute \sum kronecker(t(X_nt),t(X_nt))
  RprobitB_pp("Computing sufficient statistics", 4, 4)
  XkX <- NA
  if (P_r > 0) {
    XkX <- list()
    for (n in seq_len(N)) {
      XnkXn <- if (data$ordered) {
        matrix(0, nrow = P_r^2, ncol = 1)
      } else {
        matrix(0, nrow = P_r^2, ncol = (J - 1)^2)
      }
      for (t in seq_len(Tvec[n])) {
        var <- X[[sum(Tvec[seq_len(n - 1)]) + t]]
        if (data$ordered) {
          var <- as.numeric(var)
          XnkXn <- XnkXn + t(kronecker(t(var), t(var)))
        } else {
          XnkXn <- XnkXn + kronecker(t(var), t(var))
        }
      }
      XkX[[n]] <- XnkXn
    }
  }

  ### compute 'rdiff' (only in the ranked case)
  if (data$ranked) {
    rdiff <- list()
    perm <- oeli::permutations(data$alternatives)
    delta_level <- oeli::delta(ref = normalization$level$level, dim = J)
    Dinv <- round(MASS::ginv(delta_level))
    for (p in 1:length(perm)) {
      ranking <- match(perm[[p]], data$alternatives)
      J <- length(ranking)
      M <- matrix(0, nrow = J - 1, ncol = J)
      for (i in 1:(J - 1)) {
        M[i, ranking[i]] <- -1
        M[i, ranking[i + 1]] <- 1
      }
      rdiff[[p]] <- M %*% Dinv
    }
  } else {
    rdiff <- NA
  }

  ### build and return 'suff_statistics'
  suff_statistics <- list(
    "N" = N,
    "T" = T,
    "J" = J,
    "P_f" = P_f,
    "P_r" = P_r,
    "Tvec" = Tvec,
    "csTvec" = cumsum(Tvec) - Tvec,
    "W" = W,
    "X" = X,
    "y" = y,
    "WkW" = WkW,
    "XkX" = XkX,
    "rdiff" = rdiff
  )
  return(suff_statistics)
}
