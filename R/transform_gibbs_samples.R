#' Transformation of Gibbs samples
#'
#' @description
#' This function normalizes, burns and thins the Gibbs samples.
#'
#' @param gibbs_samples
#' The output of \code{\link{gibbs_sampling}}, i.e. a list of Gibbs samples for
#' \itemize{
#'   \item \code{Sigma},
#'   \item \code{alpha} (if \code{P_f>0}),
#'   \item \code{s}, \code{z}, \code{b}, \code{Omega} (if \code{P_r>0}).
#' }
#' @inheritParams RprobitB_data
#' @inheritParams fit_model
#' @inheritParams sufficient_statistics
#'
#' @return
#' A list, the first element \code{gibbs_sampes_raw} is the input
#' \code{gibbs_samples}, the second element is the normalized, burned, and
#' thinned version of \code{gibbs_samples} called \code{gibbs_samples_nbt}.
#' The list gets the class \code{RprobitB_gibbs_samples}.
#'
#' @keywords
#' internal

transform_gibbs_samples <- function(gibbs_samples, R, B, Q, normalization) {
  ### check inputs
  if (!is.list(gibbs_samples)) {
    stop("'gibbs_samples' must be a list of Gibbs samples.",
         call. = FALSE
    )
  }
  if (!is.numeric(R) || !R %% 1 == 0 || !R > 0) {
    stop("'R' must be a positive integer.",
         call. = FALSE
    )
  }
  if (!is.numeric(B) || !B %% 1 == 0 || !B > 0 || !B < R) {
    stop("'B' must be a positive integer smaller than 'R'.",
         call. = FALSE
    )
  }
  if (!is.numeric(Q) || !Q %% 1 == 0 || !Q > 0 || !Q < R) {
    stop("'Q' must be a positive integer smaller than 'R'.",
         call. = FALSE
    )
  }
  if (!inherits(normalization, "RprobitB_normalization")) {
    stop("'normalization' must be of class 'RprobitB_normalization'.",
         call. = FALSE
    )
  }

  ### function to scale the samples
  scaling <- function(samples, factor) {
    if (is.null(samples)) NULL else samples * factor
  }

  ### normalization (scaling) of samples
  scale <- normalization[["scale"]]
  s_n <- scaling(gibbs_samples[["s"]], 1)
  z_n <- scaling(gibbs_samples[["z"]], 1)
  d_n <- scaling(gibbs_samples[["d"]], 1)
  if (scale[["parameter"]] == "a") {
    factor <- scale[["value"]] / gibbs_samples[["alpha"]][, scale[["index"]]]
    alpha_n <- scaling(gibbs_samples[["alpha"]], factor)
    b_n <- scaling(gibbs_samples[["b"]], factor)
    Omega_n <- scaling(gibbs_samples[["Omega"]], factor^2)
    Sigma_n <- scaling(gibbs_samples[["Sigma"]], factor^2)
    beta_n <- gibbs_samples[["beta"]]
    for (i in 1:length(beta_n)) beta_n[[i]] <- scaling(beta_n[[i]], factor[i])
  }
  if (scale[["parameter"]] == "s") {
    factor <- scale[["value"]] / gibbs_samples[["Sigma"]][, paste0(scale[["index"]], ",", scale[["index"]])]
    alpha_n <- scaling(gibbs_samples[["alpha"]], sqrt(factor))
    b_n <- scaling(gibbs_samples[["b"]], sqrt(factor))
    Omega_n <- scaling(gibbs_samples[["Omega"]], factor)
    Sigma_n <- scaling(gibbs_samples[["Sigma"]], factor)
    beta_n <- gibbs_samples[["beta"]]
    for (i in 1:length(beta_n)) beta_n[[i]] <- scaling(beta_n[[i]], factor[i])
  }
  gibbs_samples_n <- list(
    "s" = s_n,
    "z" = z_n,
    "alpha" = alpha_n,
    "beta" = beta_n,
    "b" = b_n,
    "Omega" = Omega_n,
    "Sigma" = Sigma_n,
    "d" = d_n
  )
  gibbs_samples_n <- gibbs_samples_n[lengths(gibbs_samples_n) != 0]

  ### function to burn samples
  burn <- function(samples) {
    if (is.null(samples)) {
      return(NULL)
    } else {
      if (!is.list(samples)) {
        return(samples[(B + 1):R, , drop = FALSE])
      }
      if (is.list(samples)) {
        return(samples[(B + 1):R])
      }
    }
  }

  ### burning of normalized samples
  s_nb <- burn(s_n)
  z_nb <- burn(z_n)
  alpha_nb <- burn(alpha_n)
  b_nb <- burn(b_n)
  Omega_nb <- burn(Omega_n)
  Sigma_nb <- burn(Sigma_n)
  beta_nb <- burn(beta_n)
  d_nb <- burn(d_n)
  gibbs_samples_nb <- list(
    "s" = s_nb,
    "z" = z_nb,
    "alpha" = alpha_nb,
    "beta" = beta_nb,
    "b" = b_nb,
    "Omega" = Omega_nb,
    "Sigma" = Sigma_nb,
    "d" = d_nb
  )
  gibbs_samples_nb <- gibbs_samples_nb[lengths(gibbs_samples_nb) != 0]

  ### function to thin samples
  thin <- function(samples, end) {
    if (identical(samples, NULL)) {
      return(NULL)
    } else {
      if (!is.list(samples)) {
        return(samples[seq(1, end, Q), , drop = FALSE])
      }
      if (is.list(samples)) {
        return(samples[seq(1, end, Q)])
      }
    }
  }

  ### thinning of normalized samples
  s_nt <- thin(s_n, R)
  z_nt <- thin(z_n, R)
  alpha_nt <- thin(alpha_n, R)
  b_nt <- thin(b_n, R)
  Omega_nt <- thin(Omega_n, R)
  Sigma_nt <- thin(Sigma_n, R)
  beta_nt <- thin(beta_n, R)
  d_nt <- thin(d_n, R)
  gibbs_samples_nt <- list(
    "s" = s_nt,
    "z" = z_nt,
    "alpha" = alpha_nt,
    "beta" = beta_nt,
    "b" = b_nt,
    "Omega" = Omega_nt,
    "Sigma" = Sigma_nt,
    "d" = d_nt
  )
  gibbs_samples_nt <- gibbs_samples_nt[lengths(gibbs_samples_nt) != 0]

  ### thinning of normalized and burned samples
  s_nbt <- thin(s_nb, R - B)
  z_nbt <- thin(z_nb, R - B)
  alpha_nbt <- thin(alpha_nb, R - B)
  b_nbt <- thin(b_nb, R - B)
  Omega_nbt <- thin(Omega_nb, R - B)
  Sigma_nbt <- thin(Sigma_nb, R - B)
  beta_nbt <- thin(beta_nb, R - B)
  d_nbt <- thin(d_nb, R - B)
  gibbs_samples_nbt <- list(
    "s" = s_nbt,
    "z" = z_nbt,
    "alpha" = alpha_nbt,
    "beta" = beta_nbt,
    "b" = b_nbt,
    "Omega" = Omega_nbt,
    "Sigma" = Sigma_nbt,
    "d" = d_nbt
  )
  gibbs_samples_nbt <- gibbs_samples_nbt[lengths(gibbs_samples_nbt) != 0]

  ### return list of transformed Gibbs samples
  gibbs_samples <- list(
    "gibbs_samples_raw" = gibbs_samples,
    "gibbs_samples_nbt" = gibbs_samples_nbt
  )
  class(gibbs_samples) <- "RprobitB_gibbs_samples"
  return(gibbs_samples)
}
