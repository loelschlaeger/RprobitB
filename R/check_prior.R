#' Check prior parameters
#'
#' @description
#' This function checks the compatibility of submitted parameters for the prior
#' distributions and sets missing values to default values.
#'
#' @inheritParams RprobitB_data
#' @param eta
#' The mean vector of length `P_f` of the normal prior for `alpha`.
#' Per default, `eta = numeric(P_f)`.
#' @param Psi
#' The covariance matrix of dimension `P_f` x `P_f` of the normal prior for
#' `alpha`.
#' Per default, `Psi = diag(P_f)`.
#' @param delta
#' A numeric for the concentration parameter vector `rep(delta,C)` of the
#' Dirichlet prior for `s`. Per default, `delta = 1`.
#' @param xi
#' The mean vector of length `P_r` of the normal prior for each `b_c`.
#' Per default, `xi = numeric(P_r)`.
#' @param D
#' The covariance matrix of dimension `P_r` x `P_r` of the normal prior for
#' each `b_c`.
#' Per default, `D = diag(P_r)`.
#' @param nu
#' The degrees of freedom (a natural number greater than `P_r`) of the Inverse
#' Wishart prior for each `Omega_c`.
#' Per default, `nu = P_r + 2`.
#' @param Theta
#' The scale matrix of dimension `P_r` x `P_r` of the Inverse Wishart prior for
#' each `Omega_c`.
#' Per default, `Theta = diag(P_r)`.
#' @param kappa
#' The degrees of freedom (a natural number greater than `J-1`) of the Inverse
#' Wishart prior for `Sigma`.
#' Per default, `kappa = J + 1`.
#' @param E
#' The scale matrix of dimension `J-1` x `J-1` of the Inverse Wishart
#' prior for `Sigma`.
#' Per default, `E = diag(J - 1)`.
#' @param zeta
#' The mean vector of length `J - 2` of the normal prior for the logarithmic
#' increments `d` of the utility thresholds in the ordered probit model.
#' Per default, `zeta = numeric(J - 2)`.
#' @param Z
#' The covariance matrix of dimension `J-2` x `J-2` of the normal prior for the
#' logarithmic increments `d` of the utility thresholds in the ordered probit
#' model. Per default, `Z = diag(J - 2)`.
#'
#' @details
#' A priori, we assume that the model parameters follow these distributions:
#' \itemize{
#'   \item \eqn{\alpha \sim N(\eta, \Psi)}
#'   \item \eqn{s \sim Dir(\delta)}
#'   \item \eqn{b_c \sim N(\xi, D)} for all classes \eqn{c}
#'   \item \eqn{\Omega_c \sim IW(\nu,\Theta)} for all classes \eqn{c}
#'   \item \eqn{\Sigma \sim IW(\kappa,E)}
#'   \item \eqn{d \sim N(\zeta, Z)}
#' }
#' where \eqn{N} denotes the normal, \eqn{Dir} the Dirichlet, and \eqn{IW}
#' the Inverted Wishart distribution.
#'
#' @return
#' An object of class `RprobitB_prior`, which is a list containing all
#' prior parameters. Parameters that are not relevant for the model
#' configuration are set to `NA`.
#'
#' @export
#'
#' @examples
#' check_prior(P_f = 1, P_r = 2, J = 3, ordered = TRUE)
check_prior <- function(
    P_f, P_r, J, ordered = FALSE, eta = numeric(P_f), Psi = diag(P_f),
    delta = 1, xi = numeric(P_r), D = diag(P_r), nu = P_r + 2,
    Theta = diag(P_r), kappa = if (ordered) 4 else (J + 1),
    E = if (ordered) diag(1) else diag(J - 1), zeta = numeric(J - 2),
    Z = diag(J - 2)) {
  ### initialize prior list
  prior <- list()

  ### check supplied values and set missing prior parameters to default values
  if (P_f > 0) {
    ### alpha ~ MVN(eta,Psi)
    if (!is.numeric(eta) || length(eta) != P_f) {
      stop("'eta' must be a numeric vector of length 'P_f'.",
           call. = FALSE
      )
    }
    if (!is.numeric(Psi) || !is.matrix(Psi) || any(dim(Psi) != c(P_f, P_f))) {
      stop("'Psi' must be a numeric matrix of dimension 'P_f' x 'P_f'.",
           call. = FALSE
      )
    }
  } else {
    eta <- NA
    Psi <- NA
  }
  if (P_r > 0) {
    ### s ~ D(delta)
    if (!is.numeric(delta) || length(delta) != 1) {
      stop("'delta' must be a single numeric value.",
           call. = FALSE
      )
    }

    ### b_c ~ MVN(xi,D)
    if (!is.numeric(xi) || length(xi) != P_r) {
      stop("'xi' must be a numeric vector of length 'P_r'.",
           call. = FALSE
      )
    }
    if (!is.numeric(D) || !is.matrix(D) ||
        any(dim(D) != c(P_r, P_r))) {
      stop("'D' must be a numeric matrix of dimension 'P_r' x 'P_r'.",
           call. = FALSE
      )
    }

    ### Omega_c ~ IW(nu,Theta)
    if (!is.numeric(nu) || length(nu) != 1 || nu <= P_r) {
      stop("'nu' must be a single numeric value greater or equal 'P_r'.",
           call. = FALSE
      )
    }
    if (!is.numeric(Theta) || !is.matrix(Theta) ||
        any(dim(Theta) != c(P_r, P_r))) {
      stop("'Theta' must be a numeric matrix of dimension 'P_r' x 'P_r'.",
           call. = FALSE
      )
    }
  } else {
    delta <- NA
    xi <- NA
    D <- NA
    nu <- NA
    Theta <- NA
  }

  ### Sigma ~ IW(kappa,E)
  if (ordered) {
    if (!is.numeric(kappa) || length(kappa) != 1 || kappa <= 3) {
      stop("'kappa' must be a single numeric value greater or equal '3'.",
           call. = FALSE
      )
    }
    if (!is.numeric(E) || !is.matrix(E) || any(dim(E) != c(1, 1))) {
      stop("'E' must be a numeric matrix of dimension '1' x '1'.",
           call. = FALSE
      )
    }
  } else {
    if (!is.numeric(kappa) || length(kappa) != 1 || kappa <= J - 1) {
      stop("'kappa' must be a single numeric value greater or equal 'J-1'.",
           call. = FALSE
      )
    }
    if (!is.numeric(E) || !is.matrix(E) || any(dim(E) != c(J - 1, J - 1))) {
      stop("'E' must be a numeric matrix of dimension 'J-1' x 'J-1'.",
           call. = FALSE
      )
    }
  }

  ### d ~ N(zeta,Z)
  if (ordered) {
    if (!is.numeric(zeta) || length(zeta) != J - 2) {
      stop("'zeta' must be a numeric vector of length 'J - 2'.",
           call. = FALSE
      )
    }
    if (!is.numeric(Z) || !is.matrix(Z) || any(dim(Z) != c(J - 2, J - 2))) {
      stop("'Z' must be a numeric matrix of dimension 'J-2' x 'J-2'.",
           call. = FALSE
      )
    }
  } else {
    zeta <- NA
    Z <- NA
  }

  ### build and return prior parameters
  prior <- list(
    "eta" = eta, "Psi" = Psi, "delta" = delta, "xi" = xi, "D" = D, "nu" = nu,
    "Theta" = Theta, "kappa" = kappa, "E" = E, "zeta" = zeta, "Z" = Z
  )
  class(prior) <- c("RprobitB_prior", "list")
  return(prior)
}
