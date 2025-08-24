#' Define probit model parameter
#'
#' @description
#' This function creates an object of class \code{RprobitB_parameter}, which
#' contains the parameters of a probit model.
#'
#' If \code{sample = TRUE}, missing parameters are sampled. All parameters are
#' checked against the values of \code{P_f}, \code{P_r}, \code{J}, and \code{N}.
#'
#' Note that parameters are automatically ordered with respect to a
#' non-ascending \code{s} for class identifiability.
#'
#' @param C \[`integer(1)`\]\cr
#' The number (greater or equal 1) of latent classes of decision makers.
#'
#' @param alpha \[`numeric(P_f)`\]\cr
#' The fixed coefficient vector.
#'
#' @param s \[`numeric(C)`\]\cr
#' The vector of class weights.
#'
#' @param b \[`matrix(nrow = P_r, ncol = C)`\]\cr
#' The matrix of class means as columns.
#'
#' @param Omega \[`matrix(nrow = P_r * P_r, ncol = C)`\]\cr
#' The matrix of vectorized class covariance matrices as columns.
#'
#' @param Sigma \[`matrix(nrow = J - 1, ncol = J - 1)` | `numeric(1)`\]\cr
#' The differenced (wrt. alternative \code{J}) error covariance matrix.
#'
#' In case of \code{ordered = TRUE}, the single error variance.
#'
#' @param Sigma_full \[`matrix(nrow = J, ncol = J)`\]\cr
#' The error covariance matrix.
#'
#' Ignored if \code{Sigma} is specified or \code{ordered = TRUE}.
#'
#' Internally, \code{Sigma_full} gets differenced wrt.  alternative \code{J}.
#'
#' @param beta \[`matrix(nrow = P_r, ncol = N)`\]\cr
#' The matrix of the decider-specific coefficient vectors.
#'
#' @param z \[`numeric(N)`\]\cr
#' The decider class allocations.
#'
#' @param d \[`numeric(J - 2)`\]\cr
#' The logarithmic increases of the utility thresholds in the ordered probit
#' case (\code{ordered = TRUE}).
#'
#' @param sample \[`logical(1)`\]\cr
#' Sample missing parameters?
#'
#' @inheritParams RprobitB_data
#'
#' @return
#' An object of class \code{RprobitB_parameter}, which is a named list with the
#' model parameters.
#'
#' @export
#'
#' @examples
#' RprobitB_parameter(P_f = 1, P_r = 2, J = 3, N = 10, C = 2)

RprobitB_parameter <- function(
    P_f, P_r, J, N, C = 1, ordered = FALSE,
    alpha = NULL, s = NULL, b = NULL, Omega = NULL, Sigma = NULL,
    Sigma_full = NULL, beta = NULL, z = NULL, d = NULL, sample = TRUE
  ) {

  ### alpha
  if (P_f == 0) {
    alpha <- NA
  } else {
    if (is.null(alpha) && !sample) {
      alpha <- NA
    } else {
      if (is.null(alpha)) {
        alpha <- (-3 + (runif(P_f) + 0:(P_f - 1)) / P_f * 6) |>
          sample(size = P_f) |> round(1)
      }
      oeli::input_check_response(
        check = oeli::check_numeric_vector(alpha, len = P_f),
        var_name = "alpha"
      )
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
    oeli::input_check_response(
      check = checkmate::check_count(C, positive = TRUE),
      var_name = "C"
    )

    ### s
    if (C == 1) {
      s <- 1
    } else {
      if (is.null(s) && !sample) {
        s <- NA
      } else {
        if (is.null(s)) {
          if (is.null(z)) {
            s <- oeli::rdirichlet(n = 1, concentration = rep(1, C)) |>
              sort(decreasing = TRUE)
          } else {
            s <- as.numeric(table(factor(z, levels = 1:C)) / N)
          }
          s <- round(s, digits = 2)
          s[C] <- 1 - sum(s[-C])
        }
        oeli::input_check_response(
          check = oeli::check_probability_vector(s, len = C),
          var_name = "s"
        )
        names(s) <- create_labels_s(P_r, C)
      }
    }

    ### b
    if (is.null(b) && !sample) {
      b <- NA
    } else {
      if (is.null(b)) {
        b <- matrix(0, nrow = P_r, ncol = C)
        for (c in seq_len(C)) {
          b[, c] <- (-3 + (runif(P_r) + 0:(P_r - 1)) / P_r * 6) |>
            sample(size = P_r) |> round(1)
        }
      }
      b <- as.matrix(b)
      oeli::input_check_response(
        check = checkmate::check_matrix(
          b, mode = "numeric", nrows = P_r, ncols = C, any.missing = FALSE
        ),
        var_name = "b"
      )
      names(b) <- create_labels_b(P_r, C)
    }

    ### Omega
    if (is.null(Omega) && !sample) {
      Omega <- NA
    } else {
      if (is.null(Omega)) {
        Omega <- matrix(0, nrow = P_r * P_r, ncol = C)
        for (c in 1:C) {
          Omega[, c] <- diag(P_r) +
            oeli::rwishart(df = 2 * P_r, scale = diag(P_r), inv = TRUE) |>
            as.vector()
        }
      }
      Omega <- as.matrix(Omega)
      oeli::input_check_response(
        check = checkmate::check_matrix(
          Omega, mode = "numeric", any.missing = FALSE, nrow = P_r^2, ncol = C
        ),
        var_name = "Omega"
      )
      for (c in 1:C) {
        oeli::input_check_response(
          check = oeli::check_covariance_matrix(
            matrix(Omega[, c], nrow = P_r, ncol = P_r), dim = P_r
          ),
          var_name = paste("Column", c, "in 'Omega'")
        )
      }
      names(Omega) <- create_labels_Omega(P_r, C, cov_sym = TRUE)
    }

    ### z
    if (is.null(z) && !sample) {
      z <- NA
    } else {
      if (is.null(z)) {
        z <- sample(seq_len(C), N, prob = s, replace = TRUE)
      }
      oeli::input_check_response(
        check = checkmate::check_integerish(
          z, lower = 1, upper = C, any.missing = FALSE, len = N
        ),
        var_name = "z"
      )
    }

    ### beta
    if (is.null(beta) && !sample) {
      beta <- NA
    } else {
      if (is.null(beta)) {
        beta <- matrix(0, nrow = P_r, ncol = N)
        for (n in seq_len(N)) {
          beta[, n] <- b[, z[n]] +
            t(chol(matrix(Omega[, z[n]], nrow = P_r, ncol = P_r))) %*%
            stats::rnorm(P_r)
        }
      }
      oeli::input_check_response(
        check = checkmate::check_matrix(
          beta, mode = "numeric", any.missing = FALSE, nrow = P_r, ncol = N
        ),
        var_name = "beta"
      )
    }
  }

  ### Sigma
  if (is.null(Sigma_full) && is.null(Sigma) && !sample) {
    Sigma <- NA
    Sigma_full <- NA
  } else {
    if (ordered) {
      Sigma_full <- NA
      if (is.null(Sigma)) {
        Sigma <- round(runif(1, min = 1, max = 3), 2)
      }
      oeli::input_check_response(
        check = checkmate::check_number(
          Sigma, lower = 0, finite = TRUE
        ),
        var_name = "Sigma"
      )
      names(Sigma) <- create_labels_Sigma(J, ordered = TRUE)
    } else {
      if (is.null(Sigma)) {
        if (is.null(Sigma_full)) {
          Sigma_full <- diag(J) +
            oeli::rwishart(df = 2 * J, scale = diag(J), inv = TRUE)
        } else {
          Sigma_full <- as.matrix(Sigma_full)
        }
        delta_J <- oeli::delta(ref = J, dim = J)
        Sigma <- delta_J %*% Sigma_full %*% t(delta_J)
      } else {
        Sigma <- as.matrix(Sigma)
        Sigma_full <- oeli::undiff_cov(cov = Sigma, ref = J)
      }
      oeli::input_check_response(
        check = oeli::check_covariance_matrix(Sigma, dim = J - 1),
        var_name = "Sigma"
      )
      oeli::input_check_response(
        check = oeli::check_covariance_matrix(Sigma_full, dim = J),
        var_name = "Sigma_full"
      )
      names(Sigma) <- create_labels_Sigma(J, cov_sym = TRUE)
      names(Sigma_full) <- create_labels_Sigma(J + 1, cov_sym = TRUE)
    }
  }

  ### d
  if (ordered) {
    if (is.null(d)) {
      d <- round(runif(J - 2, min = -1, max = 2), 2)
    }
    oeli::input_check_response(
      check = oeli::check_numeric_vector(d, any.missing = FALSE, len = J - 2)
    )
    names(d) <- create_labels_d(J, ordered = TRUE)
  } else {
    d <- NA
  }

  ### build and return 'RprobitB_parameter'-object
  structure(
    list(
      "alpha" = alpha,
      "C" = C,
      "s" = s,
      "b" = b,
      "Omega" = Omega,
      "Sigma" = Sigma,
      "Sigma_full" = Sigma_full,
      "beta" = beta,
      "z" = z,
      "d" = d
    ),
    class = c("RprobitB_parameter", "list")
  )
}

#' @rdname RprobitB_parameter
#'
#' @param x
#' An `RprobitB_parameter` object.
#'
#' @param ... \[`character()`\]\cr
#' Names of parameters to be printed. If not specified, all parameters are
#' printed.
#'
#' @param digits \[`integer(1)`\]\cr
#' The number of decimal places.
#'
#' @export

print.RprobitB_parameter <- function(x, ..., digits = 4) {
  oeli::input_check_response(
    check = checkmate::check_class(x, "RprobitB_parameter"),
    var_name = "x"
  )
  pars <- list(...)
  ind <- if (length(pars) != 0) {
    sapply(pars, function(par) which(names(x) == par))
  } else {
    seq_along(x)
  }
  for (i in ind) {
    oeli::print_matrix(x[[i]], label = names(x)[i])
    cat("\n\n")
  }
  invisible(x)
}
