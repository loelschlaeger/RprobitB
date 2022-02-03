#' Change the length of the burn-in period, the thinning factor and the scale
#' after Gibbs sampling.
#'
#' @description
#' Given an object of class \code{RprobitB_fit}, this function can:
#' \itemize{
#'   \item change the length \code{B} of the burn-in period,
#'   \item change the the thinning factor \code{Q} of the Gibbs samples,
#'   \item change the model \code{scale}.
#' }
#'
#' @details
#' See the vignette "Model fitting" for more details:
#' \code{vignette("model_fitting", package = "RprobitB")}.
#'
#' @inheritParams mcmc
#' @param _data
#' An object of class \code{\link{RprobitB_fit}}.
#' @param check_preference_flip
#' If \code{TRUE} check for flip in preferences with new scale.
#' @param ...
#' Ignored.
#'
#' @return
#' An object of class \code{RprobitB_fit}.
#'
#' @export

transform.RprobitB_fit <- function(`_data`, B = NULL, Q = NULL, scale = NULL,
                                   check_preference_flip = TRUE, ...) {

  ### check inputs
  x <- `_data`
  if (!inherits(x, "RprobitB_fit")) {
    stop("'x' must be of class 'RprobitB_fit'.")
  }
  if (is.null(B)) {
    B <- x$B
  } else {
    x$B <- B
  }
  if (is.null(Q)) {
    Q <- x$Q
  } else {
    x$Q <- Q
  }
  R <- x$R
  P_f <- x$data$P_f
  J <- x$data$J
  if (!is.numeric(B) || !B %% 1 == 0 || !B > 0 || !B < R) {
    stop("'B' must be a positive integer smaller than 'R'.")
  }
  if (!is.numeric(Q) || !Q %% 1 == 0 || !Q > 0 || !Q < R) {
    stop("'Q' must be a positive integer smaller than 'R'.")
  }
  if (is.null(scale)) {
    normalization <- x$normalization
  } else {
    ### check if new scale flips preferences
    if (check_preference_flip) {
      model_new <- transform.RprobitB_fit(x, scale = scale, check_preference_flip = FALSE)
      preference_flip(model_old = x, model_new = model_new)
    }
    normalization <- RprobitB_normalization(J = J, P_f = P_f, scale = scale)
    x$normalization <- normalization
  }

  ### scale, burn and thin Gibbs samples
  gibbs_samples <- transform_gibbs_samples(
    gibbs_samples = x$gibbs_samples$gibbs_samples, R = R, B = B, Q = Q,
    normalization = normalization
  )
  x$gibbs_samples <- gibbs_samples

  ### scale true parameters
  if (x$data$simulated) {
    x$data$true_parameter <- transform_parameter(
      parameter = x$data$true_parameter, normalization = normalization
    )
  }

  ### return 'RprobitB_fit'
  return(x)
}

#' Transformation of Gibbs samples.
#'
#' @description
#' This function normalizes, burns and thins the Gibbs samples.
#'
#' @param gibbs_samples
#' The output of \link{gibbs_sampling}.
#' @inheritParams RprobitB_data
#' @inheritParams mcmc
#' @inheritParams sufficient_statistics
#' @return
#' An object of class \code{RprobitB_gibbs_samples}, i.e. a list of transformed
#' Gibbs samples. Each element is a list, containing (if available) the
#' Gibbs samples for \code{s}, \code{alpha}, \code{b}, \code{Omega}, and
#' \code{Sigma}:
#' \itemize{
#'   \item \code{gibbs_samples}:
#'   The function input \code{gibbs_samples}.
#'   \item \code{gibbs_samples_n}:
#'   A list of normalized samples based on \code{normalization}.
#'   \item \code{gibbs_samples_nb}:
#'   A list of normalized and burned samples based on \code{normalization} and \code{B}.
#'   \item \code{gibbs_samples_nt}:
#'   A list of normalized and thinned samples based on \code{normalization} and \code{Q}
#'   \item \code{gibbs_samples_nbt}:
#'   A list of normalized, burned and thinned samples based on \code{normalization},
#'   \code{B} and \code{Q}
#' }
#'
#' @keywords
#' internal

transform_gibbs_samples <- function(gibbs_samples, R, B, Q, normalization) {

  ### check inputs
  if (!inherits(normalization, "RprobitB_normalization")) {
    stop("'normalization' must be of class 'RprobitB_normalization'.")
  }

  ### function to scale the samples
  scaling <- function(samples, factor) {
    if (is.null(samples)) NULL else samples * factor
  }

  ### scaling of samples
  scale <- normalization$scale
  s_n <- scaling(gibbs_samples$s, 1)
  if (scale$parameter == "a") {
    factor <- scale$value / gibbs_samples$alpha[, scale$index]
    alpha_n <- scaling(gibbs_samples$alpha, factor)
    b_n <- scaling(gibbs_samples$b, factor)
    Omega_n <- scaling(gibbs_samples$Omega, factor^2)
    Sigma_n <- scaling(gibbs_samples$Sigma, factor^2)
  }
  if (scale$parameter == "s") {
    factor <- scale$value / gibbs_samples$Sigma[, paste0(scale$index, ",", scale$index)]
    alpha_n <- scaling(gibbs_samples$alpha, sqrt(factor))
    b_n <- scaling(gibbs_samples$b, sqrt(factor))
    Omega_n <- scaling(gibbs_samples$Omega, factor)
    Sigma_n <- scaling(gibbs_samples$Sigma, factor)
  }
  gibbs_samples_n <- list(
    "s" = s_n,
    "alpha" = alpha_n,
    "b" = b_n,
    "Omega" = Omega_n,
    "Sigma" = Sigma_n
  )
  gibbs_samples_n <- gibbs_samples_n[lengths(gibbs_samples_n) != 0]

  ### function to burn samples
  burn <- function(samples) {
    if (is.null(samples)) NULL else samples[(B + 1):R, , drop = FALSE]
  }

  ### burning of normalized samples
  s_nb <- burn(s_n)
  alpha_nb <- burn(alpha_n)
  b_nb <- burn(b_n)
  Omega_nb <- burn(Omega_n)
  Sigma_nb <- burn(Sigma_n)
  gibbs_samples_nb <- list(
    "s" = s_nb,
    "alpha" = alpha_nb,
    "b" = b_nb,
    "Omega" = Omega_nb,
    "Sigma" = Sigma_nb
  )
  gibbs_samples_nb <- gibbs_samples_nb[lengths(gibbs_samples_nb) != 0]

  ### function to thin samples
  thin <- function(samples, end) {
    if (any(is.null(samples))) NULL else samples[seq(1, end, Q), , drop = FALSE]
  }

  ### thinning of normalized samples
  s_nt <- thin(s_n, R)
  alpha_nt <- thin(alpha_n, R)
  b_nt <- thin(b_n, R)
  Omega_nt <- thin(Omega_n, R)
  Sigma_nt <- thin(Sigma_n, R)
  gibbs_samples_nt <- list(
    "s" = s_nt,
    "alpha" = alpha_nt,
    "b" = b_nt,
    "Omega" = Omega_nt,
    "Sigma" = Sigma_nt
  )
  gibbs_samples_nt <- gibbs_samples_nt[lengths(gibbs_samples_nt) != 0]

  ### thinning of normalized and burned samples
  s_nbt <- thin(s_nb, R - B)
  alpha_nbt <- thin(alpha_nb, R - B)
  b_nbt <- thin(b_nb, R - B)
  Omega_nbt <- thin(Omega_nb, R - B)
  Sigma_nbt <- thin(Sigma_nb, R - B)
  gibbs_samples_nbt <- list(
    "s" = s_nbt,
    "alpha" = alpha_nbt,
    "b" = b_nbt,
    "Omega" = Omega_nbt,
    "Sigma" = Sigma_nbt
  )
  gibbs_samples_nbt <- gibbs_samples_nbt[lengths(gibbs_samples_nbt) != 0]

  ### build and add class to 'gibbs_samples'
  gibbs_samples <- list(
    "gibbs_samples" = gibbs_samples,
    "gibbs_samples_n" = gibbs_samples_n,
    "gibbs_samples_nb" = gibbs_samples_nb,
    "gibbs_samples_nt" = gibbs_samples_nt,
    "gibbs_samples_nbt" = gibbs_samples_nbt
  )
  class(gibbs_samples) <- "RprobitB_gibbs_samples"

  ### return list of transformed Gibbs samples
  return(gibbs_samples)
}

#' Transformation of parameter values.
#'
#' @description
#' This function transforms parameter values based on \code{normalization}.
#'
#' @param parameter
#' An object of class \code{RprobitB_parameter}.
#' @param normalization
#' An object of class \code{RprobitB_normalization}.
#'
#' @return
#' An object of class \code{RprobitB_parameter}.
#'
#' @keywords
#' internal

transform_parameter <- function(parameter, normalization) {

  ### check inputs
  if (!inherits(parameter, "RprobitB_parameter")) {
    stop("'parameter' must be of class 'RprobitB_parameter'.")
  }
  if (!inherits(normalization, "RprobitB_normalization")) {
    stop("'normalization' must be of class 'RprobitB_normalization'.")
  }

  ### function to scale the parameters
  scaling <- function(par, factor) {
    if (any(is.na(par))) {
      NA
    } else {
      out <- par * factor
      ### preserve names
      names(out) <- names(par)
      return(out)
    }
  }

  ### scale elements of 'parameter'
  scale <- normalization$scale
  if (scale$parameter == "a") {
    factor <- scale$value / parameter$alpha[scale$index]
    parameter$alpha <- scaling(parameter$alpha, factor)
    parameter$b <- scaling(parameter$b, factor)
    parameter$Omega <- scaling(parameter$Omega, factor^2)
    parameter$Sigma <- scaling(parameter$Sigma, factor^2)
    parameter$beta <- scaling(parameter$beta, factor)
  }
  if (scale$parameter == "s") {
    factor <- scale$value / parameter$Sigma[scale$index, scale$index]
    parameter$alpha <- scaling(parameter$alpha, sqrt(factor))
    parameter$b <- scaling(parameter$b, sqrt(factor))
    parameter$Omega <- scaling(parameter$Omega, factor)
    parameter$Sigma <- scaling(parameter$Sigma, factor)
    parameter$beta <- scaling(parameter$beta, sqrt(factor))
    parameter$Sigma_full <- undiff_Sigma(parameter$Sigma, normalization$level)
  }

  ### return 'parameter'
  return(parameter)
}

#' Check for flip in preferences after change in model scale.
#'
#' @description
#' This function checks if a change in the model scale flipped the preferences.
#'
#' @param model_old
#' An object of class \code{RprobitB_fit}, the model before the scale change.
#' @param model_new
#' An object of class \code{RprobitB_fit}, the model after the scale change.
#'
#' @return
#' No return value, called for side-effects.
#'
#' @keywords
#' internal
#'
#' @noRd
#'
#' @importFrom stats ecdf

preference_flip <- function(model_old, model_new) {
  stopifnot(class(model_old) == "RprobitB_fit")
  stopifnot(class(model_new) == "RprobitB_fit")
  stopifnot(model_old$data$P_f == model_new$data$P_f)
  stopifnot(model_old$data$P_r == model_new$data$P_r)
  flag <- FALSE
  for (p in seq_len(model_old$data$P_f)) {
    P1 <- stats::ecdf(model_old$gibbs_samples$gibbs_samples_nbt$alpha[, p])
    P2 <- stats::ecdf(model_new$gibbs_samples$gibbs_samples_nbt$alpha[, p])
    if (P1(0) != P2(0)) {
      flag <- TRUE
    }
  }
  for (p in seq_len(model_old$data$P_r)) {
    P1 <- stats::ecdf(model_old$gibbs_samples$gibbs_samples_nbt$b[, p])
    P2 <- stats::ecdf(model_new$gibbs_samples$gibbs_samples_nbt$b[, p])
    if (P1(0) != P2(0)) {
      flag <- TRUE
    }
  }
  if (flag) {
    stop(
      "Caution, this transformation may flip preferences. ",
      "Set 'check_preference_flip = FALSE' to transform anyway."
    )
  }
}
