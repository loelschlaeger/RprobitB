#' Sample from prior distributions
#'
#' @description
#' This function returns a sample from each parameter's prior distribution.
#'
#' @param prior
#' An object of class \code{RprobitB_prior}, which is the output of
#' \code{\link{check_prior}}.
#' @param C
#' The number of latent classes.
#'
#' @return
#' A list of draws for \code{alpha}, \code{s}, \code{b}, \code{Omega}, and
#' \code{Sigma} (if specified for the model).
#'
#' @keywords
#' internal
#'
#' @examples
#' prior <- check_prior(P_f = 1, P_r = 2, J = 3)
#' RprobitB:::draw_from_prior(prior, C = 2)
draw_from_prior <- function(prior, C = 1) {
  ### input checks
  if (!inherits(prior, "RprobitB_prior")) {
    stop("'prior' must be of class 'RprobitB_prior.", call. = FALSE)
  }

  ### alpha ~ MVN(eta,Psi)
  if (identical(prior$eta, NA) || identical(prior$Psi, NA)) {
    alpha <- NULL
  } else {
    alpha <- oeli::rmvnorm(n = 1, mean = prior$eta, Sigma = prior$Psi)
  }

  ### s ~ D(delta)
  if (identical(prior$delta, NA)) {
    s <- NULL
  } else {
    s <- oeli::rdirichlet(n = 1, concentration = rep(prior$delta, C)) |>
      sort(decreasing = TRUE)
  }

  ### b_c ~ MVN(xi,D) for all c
  if (identical(prior$xi, NA) || identical(prior$D, NA)) {
    b <- NULL
  } else {
    b <- replicate(C, oeli::rmvnorm(n = 1, mean = prior$xi, Sigma = prior$D)) |>
      matrix(ncol = C)
  }

  ### Omega_c ~ IW(nu,Theta) for all c
  if (identical(prior$nu, NA) || identical(prior$Theta, NA)) {
    Omega <- NULL
  } else {
    Omega <- replicate(C, oeli::rwishart(df = prior$nu, scale = prior$Theta, inv = TRUE)) |>
      matrix(ncol = C)
  }

  ### Sigma ~ IW(kappa,E)
  if (identical(prior$kappa, NA) || identical(prior$E, NA)) {
    Sigma <- NULL
  } else {
    Sigma <- oeli::rwishart(df = prior$kappa, scale = prior$E, inv = TRUE)
  }

  ### return draws
  draws <- list(
    "alpha" = alpha,
    "s" = s,
    "b" = b,
    "Omega" = Omega,
    "Sigma" = Sigma
  )
  return(draws)
}
