#' Transformation of Gibbs samples
#' @description
#' Function that normalizes, burns and thins the Gibbs samples.
#' @param gibbs_samples_raw
#' The output of \code{\link{gibbs_sampling}}, i.e. a list of raw Gibbs samples.
#' @inheritParams RprobitB_data
#' @inheritParams fit
#' @inheritParams compute_suff_statistics
#' @return
#' A list of transformed Gibbs samples. Each element is a list, containing the
#' Gibbs samples for \code{s}, \code{alpha}, \code{b}, \code{Omega}, and
#' \code{Sigma} (if available):
#' \itemize{
#'   \item \code{gibbs_samples_raw}:
#'   The function input \code{gibbs_samples_raw}
#'   \item \code{gibbs_samples_n}:
#'   A list of normalized samples based on \code{scale}
#'   \item \code{gibbs_samples_nb}:
#'   A list of normalized and burned samples based on \code{scale} and \code{B}
#'   \item \code{gibbs_samples_nt}:
#'   A list of normalized and thinned samples based on \code{scale} and \code{Q}
#'   \item \code{gibbs_samples_nbt}:
#'   A list of normalized, burned and thinned samples based on \code{scale},
#'   \code{B} and \code{Q}
#' }

transform_gibbs_samples = function(gibbs_samples_raw, R, B, Q, scale) {

  ### determine estimated number of latent classes
  last_s_draw = gibbs_samples_raw$s_draws[nrow(gibbs_samples_raw$s_draws),]
  C_est = length(last_s_draw[last_s_draw!=0])

  ### function to normalize the samples
  normalize = function(par, factor) if(any(is.na(par))) NA else par * factor

  ### normalization of samples
  s_draws_n = normalize(gibbs_samples_raw$s_draws, 1)
  if(scale$parameter=="a"){
    factor = scale$value / gibbs_samples_raw$alpha_draws[,scale$index]
    alpha_draws_n = normalize(gibbs_samples_raw$alpha_draws, factor)
    b_draws_n = normalize(gibbs_samples_raw$b_draws, factor)
    Omega_draws_n = normalize(gibbs_samples_raw$Omega_draws, factor^2)
    Sigma_draws_n = normalize(gibbs_samples_raw$Sigma_draws, factor^2)
  }
  if(scale$parameter=="s"){
    Jm1 = sqrt(length(gibbs_samples_raw$Sigma_draws[1,]))
    factor = gibbs_samples_raw$Sigma_draws[,(Jm1)*(scale$index-1)+scale$index]
    alpha_draws_n = normalize(gibbs_samples_raw$alpha_draws, sqrt(factor))
    b_draws_n = normalize(gibbs_samples_raw$b_draws, sqrt(factor))
    Omega_draws_n = normalize(gibbs_samples_raw$Omega_draws, factor)
    Sigma_draws_n = normalize(gibbs_samples_raw$Sigma_draws, factor)
  }
  gibbs_samples_n = list("s"     = s_draws_n,
                         "alpha" = alpha_draws_n,
                         "b"     = b_draws_n,
                         "Omega" = Omega_draws_n,
                         "Sigma" = Sigma_draws_n)

  ### function to burn samples
  burn = function(samples)
    if(any(is.na(samples))) NA else samples[(B+1):R,,drop=FALSE]

  ### burning of normalized samples
  s_draws_nb = burn(s_draws_n)
  alpha_draws_nb = burn(alpha_draws_n)
  b_draws_nb = burn(b_draws_n)
  Omega_draws_nb = burn(Omega_draws_n)
  Sigma_draws_nb = burn(Sigma_draws_n)
  gibbs_samples_nb = list("s"     = s_draws_nb,
                          "alpha" = alpha_draws_nb,
                          "b"     = b_draws_nb,
                          "Omega" = Omega_draws_nb,
                          "Sigma" = Sigma_draws_nb)

  ### function to thin samples
  thin = function(samples,end)
    if(any(is.na(samples))) NA else samples[seq(1,end,Q),,drop=FALSE]

  ### thinning of normalized samples
  s_draws_nt = thin(s_draws_n,R)
  alpha_draws_nt = thin(alpha_draws_n,R)
  b_draws_nt = thin(b_draws_n,R)
  Omega_draws_nt = thin(Omega_draws_n,R)
  Sigma_draws_nt = thin(Sigma_draws_n,R)
  gibbs_samples_nt = list("s"     = s_draws_nt,
                          "alpha" = alpha_draws_nt,
                          "b"     = b_draws_nt,
                          "Omega" = Omega_draws_nt,
                          "Sigma" = Sigma_draws_nt)

  ### thinning of normalized and burned samples
  s_draws_nbt = thin(s_draws_nb,R-B)
  alpha_draws_nbt = thin(alpha_draws_nb,R-B)
  b_draws_nbt = thin(b_draws_nb,R-B)
  Omega_draws_nbt = thin(Omega_draws_nb,R-B)
  Sigma_draws_nbt = thin(Sigma_draws_nb,R-B)
  gibbs_samples_nbt = list("s"    = s_draws_nbt,
                           "alpha" = alpha_draws_nbt,
                           "b"     = b_draws_nbt,
                           "Omega" = Omega_draws_nbt,
                           "Sigma" = Sigma_draws_nbt)

  ### build and add class to 'gibbs_samples'
  gibbs_samples = list("gibbs_samples_raw" = gibbs_samples_raw,
                       "gibbs_samples_n"   = gibbs_samples_n,
                       "gibbs_samples_nb"  = gibbs_samples_nb,
                       "gibbs_samples_nt"  = gibbs_samples_nt,
                       "gibbs_samples_nbt" = gibbs_samples_nbt)
  class(gibbs_samples) = "RprobitB_gibbs_samples"

  ### return list of transformed Gibbs samples
  return(gibbs_samples)
}
