#' Create object of class \code{RprobitB_parameter}.
#' @description
#' This function creates an object of class \code{RprobitB_parameter}.
#' If \code{sample = TRUE}, missing parameters are sampled. All parameters are
#' checked against the values of \code{P_f}, \code{P_r}, \code{J}, and \code{N}.
#' @inheritParams RprobitB_data
#' @param alpha
#' The fixed coefficient vector of length \code{P_f}.
#' Set to \code{NA} if \code{P_f = 0}.
#' @param C
#' The number (greater or equal 1) of latent classes of decision makers.
#' Set to \code{NA} if \code{P_r = 0}. Otherwise, \code{C = 1} per default.
#' @param s
#' The vector of class weights of length \code{C}.
#' Set to \code{NA} if \code{P_r = 0}.
#' For identifiability, the vector must be non-ascending.
#' @param b
#' The matrix of class means as columns of dimension \code{P_r} x \code{C}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param Omega
#' The matrix of class covariance matrices as columns of dimension
#' \code{P_r*P_r} x \code{C}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param Sigma
#' The differenced error term covariance matrix of dimension
#' \code{J-1} x \code{J-1} with respect to alternative \code{J}.
#' @param Sigma_full
#' The error term covariance matrix of dimension \code{J} x \code{J}.
#' Internally, \code{Sigma_full} gets differenced with respect to alternative
#' \code{J}, so it becomes an identified covariance matrix of dimension
#' \code{J-1} x \code{J-1}. If \code{Sigma} is specified, \code{Sigma_full} is
#' ignored.
#' @param beta
#' The matrix of the decision-maker specific coefficient vectors of dimension
#' \code{P_r} x \code{N}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param z
#' The vector of the allocation variables of length \code{N}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param sample
#' A boolean, if \code{TRUE} missing parameters get sampled.
#' @param seed
#' Set a seed for sampling missing parameters.
#' @return
#' An object of class \code{RprobitB_parameter}, i.e. a named list with the
#' model parameters \code{alpha}, \code{C}, \code{s}, \code{b}, \code{Omega},
#' \code{Sigma}, \code{Sigma_full}, \code{beta}, and \code{z}.
#' @keywords
#' s3

RprobitB_parameter <- function(P_f, P_r, J, N, alpha = NULL, C = NULL, s = NULL,
                               b = NULL, Omega = NULL, Sigma = NULL,
                               Sigma_full = NULL, beta = NULL, z = NULL,
                               seed = NULL, sample = TRUE) {

  ### seed for sampling missing parameters
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ### alpha
  if (P_f == 0) {
    alpha <- NA
  } else {
    if (is.null(alpha) && !sample) {
      alpha <- NA
    } else {
      if (is.null(alpha)) {
        alpha <- round(runif(P_f, -3, 3), 1)
      }
      if (length(alpha) != P_f || !is.numeric(alpha)) {
        stop("'alpha' must be a numeric vector of length ", P_f, ".")
      }
      names(alpha) <- create_labels_alpha(P_f)
    }
  }

  ### C, s, b, Omega, z, beta
  if (P_r == 0) {
    C <- NA
    s <- NA
    b <- NA
    Omega <- NA
    z <- NA
    beta <- NA
  } else {

    ### C
    if (!is.null(C)) {
      if (!is.numeric(C) || !C %% 1 == 0 || !C > 0) {
        stop("'C' must be a number greater or equal 1.")
      }
    } else {
      C <- 1
    }

    ### s
    if (is.null(s) && !sample) {
      s <- NA
    } else {
      if (is.null(s)) {
        s <- round(sort(as.vector(rdirichlet(rep(1, C))), decreasing = TRUE), 2)
      }
      if (length(s) != C || !is.numeric(s) ||
        abs(sum(s) - 1) > .Machine$double.eps || is.unsorted(rev(s))) {
        stop("'s' must be a non-ascending numeric vector of length ", C, " which sums up to 1.")
      }
      names(s) <- create_labels_s(P_r, C)
    }

    ### b
    if (is.null(b) && !sample) {
      b <- NA
    } else {
      if (is.null(b)) {
        b <- matrix(0, nrow = P_r, ncol = C)
        for (c in 1:C) b[, c] <- round(runif(P_r, -3, 3), 1)
      }
      b <- as.matrix(b)
      if (!is.numeric(b) || nrow(b) != P_r || ncol(b) != C) {
        stop("'b' must be a numeric matrix of dimension ", P_r, " x ", C, ".")
      }
      names(b) <- create_labels_b(P_r, C)
    }

    ### Omega
    if (is.null(Omega) && !sample) {
      Omega <- NA
    } else {
      if (is.null(Omega)) {
        Omega <- matrix(0, nrow = P_r * P_r, ncol = C)
        for (c in 1:C) {
          Omega[, c] <- as.vector(rwishart(P_r, diag(P_r))$W)
        }
      }
      Omega <- as.matrix(Omega)
      if (!is.numeric(Omega) || nrow(Omega) != P_r * P_r ||
        ncol(Omega) != C) {
        stop(
          "'Omega' must be a numeric matrix of dimension ", P_r * P_r, " x ",
          C, "."
        )
      }
      for (c in 1:C) {
        if (!is_covariance_matrix(matrix(Omega[, c], nrow = P_r, ncol = P_r))) {
          stop(paste("Column", c, "in 'Omega' builds no covariance matrix."))
        }
      }
      names(Omega) <- create_labels_Omega(P_r, C, cov_sym = TRUE)
    }

    ### z
    if (is.null(z) && !sample) {
      z <- NA
    } else {
      if (is.null(z)) {
        z <- sample(1:C, N, prob = s, replace = TRUE)
      }
      if (length(z) != N || !is.numeric(z) || !all(z %in% 1:C)) {
        stop(
          "'z' must be a numeric vector of length ", N,
          " with elements of value ", paste(seq_len(C), collapse = ", "), "."
        )
      }
    }

    ### beta
    if (is.null(beta) && !sample) {
      beta <- NA
    } else {
      if (is.null(beta)) {
        beta <- matrix(0, nrow = P_r, ncol = N)
        for (n in seq_len(N)) {
          beta[, n] <- b[, z[n]] +
            t(chol(matrix(Omega[, z[n]], nrow = P_r, ncol = P_r))) %*% rnorm(P_r)
        }
      }
      if (!is.numeric(beta) || nrow(beta) != P_r ||
        ncol(beta) != N) {
        stop("'beta' must be a numeric matrix of dimension ", P_r, " x ", N, ".")
      }
    }
  }

  ### Sigma
  if (is.null(Sigma_full) && is.null(Sigma) && !sample) {
    Sigma <- NA
    Sigma_full <- NA
  } else {
    if (is.null(Sigma)) {
      if (is.null(Sigma_full)) {
        Sigma_full <- rwishart(J, diag(J))$W
      } else {
        Sigma_full <- as.matrix(Sigma_full)
      }
      Sigma <- delta(J, J) %*% Sigma_full %*% t(delta(J, J))
    } else {
      Sigma <- as.matrix(Sigma)
      Sigma_full <- undiff_Sigma(Sigma, i = J)
    }
    if (!(is_covariance_matrix(Sigma) && nrow(Sigma) == J - 1)) {
      stop("'Sigma' is not a proper (differenced) covariance matrix of dimension ", J - 1, " x ", J - 1, ".")
    }
    if (!(is_covariance_matrix(Sigma_full) && nrow(Sigma_full) == J)) {
      stop("'Sigma_diff' is not a proper covariance matrix of dimension ", J, " x ", J, ".")
    }
    names(Sigma) <- create_labels_Sigma(J, cov_sym = TRUE)
    names(Sigma_full) <- create_labels_Sigma(J + 1, cov_sym = TRUE)
  }

  ### build and return 'RprobitB_parameter'-object
  out <- list(
    "alpha" = alpha,
    "C" = C,
    "s" = s,
    "b" = b,
    "Omega" = Omega,
    "Sigma" = Sigma,
    "Sigma_full" = Sigma_full,
    "beta" = beta,
    "z" = z
  )
  class(out) <- "RprobitB_parameter"
  return(out)
}

#' Print method for \code{RprobitB_parameter}.
#' @description
#' This function is the print method for an object of class
#' \code{RprobitB_parameter}.
#' @param x
#' An object of class \code{RprobitB_parameter}.
#' @param ...
#' Ignored.
#' @noRd

print.RprobitB_parameter <- function(x, ...) {

  cat("RprobitB model parameter:\n\n")

  for(i in seq_along(x)) {

    if("numeric" %in% class(x[[i]]) || "integer" %in% class(x[[i]])) {

      cat(paste0(names(x)[i], ":"), x[[i]], "\n\n")

    } else if("matrix" %in% class(x[[i]])) {

      cat(paste0(names(x)[i], ": "))

      rowdots <- min(nrow(x[[i]]) - 1, 4)
      coldots <- min(ncol(x[[i]]) - 1, 4)

      ramify::pprint(x[[i]], rowdots = rowdots, coldots = coldots)
      cat("\n")
    }

  }

  return(invisible(x))
}
