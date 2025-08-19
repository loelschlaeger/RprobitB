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
#' @keywords internal
#'
#' @examples
#' prior <- check_prior(P_f = 1, P_r = 2, J = 3)
#' RprobitB:::draw_from_prior(prior, C = 2)

draw_from_prior <- function(prior, C = 1) {
  ### input checks
  if (!inherits(prior, "RprobitB_prior")) {
    stop("'prior' must be of class 'RprobitB_prior.", call. = FALSE)
  }

  ### alpha ~ MVN(mu_alpha_0,Sigma_alpha_0)
  if (identical(prior$mu_alpha_0, NA) || identical(prior$Sigma_alpha_0, NA)) {
    alpha <- NULL
  } else {
    alpha <- oeli::rmvnorm(n = 1, mean = prior$mu_alpha_0, Sigma = prior$Sigma_alpha_0)
  }

  ### s ~ D(delta)
  if (identical(prior$delta, NA)) {
    s <- NULL
  } else {
    s <- oeli::rdirichlet(n = 1, concentration = rep(prior$delta, C)) |>
      sort(decreasing = TRUE)
  }

  ### b_c ~ MVN(mu_b_0, Sigma_b_0) for all c
  if (identical(prior$mu_b_0, NA) || identical(prior$Sigma_b_0, NA)) {
    b <- NULL
  } else {
    b <- replicate(C, oeli::rmvnorm(n = 1, mean = prior$mu_b_0, Sigma = prior$Sigma_b_0)) |>
      matrix(ncol = C)
  }

  ### Omega_c ~ IW(n_Omega_0,V_Omega_0) for all c
  if (identical(prior$n_Omega_0, NA) || identical(prior$V_Omega_0, NA)) {
    Omega <- NULL
  } else {
    Omega <- replicate(C, oeli::rwishart(df = prior$n_Omega_0, scale = prior$V_Omega_0, inv = TRUE)) |>
      matrix(ncol = C)
  }

  ### Sigma ~ IW(n_Sigma_0,V_Sigma_0)
  if (identical(prior$n_Sigma_0, NA) || identical(prior$V_Sigma_0, NA)) {
    Sigma <- NULL
  } else {
    Sigma <- oeli::rwishart(df = prior$n_Sigma_0, scale = prior$V_Sigma_0, inv = TRUE)
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
