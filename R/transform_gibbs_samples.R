#' Transformation of Gibbs samples.
#' @description
#' This function normalizes, burns and thins the Gibbs samples.
#' @param gibbs_samples
#' The output of \link{gibbs_sampling}.
#' @inheritParams RprobitB_data
#' @inheritParams mcmc
#' @inheritParams compute_sufficient_statistics
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
#' @keywords
#' internal

transform_gibbs_samples = function(gibbs_samples, R, B, Q, normalization) {

  ### check inputs
  if(!inherits(normalization, "RprobitB_normalization"))
    stop("'normalization' must be of class 'RprobitB_normalization'.")

  ### function to scale the samples
  scaling = function(samples, factor){
    if(is.null(samples)) NULL else samples * factor
  }

  ### scaling of samples
  scale = normalization$scale
  s_n = scaling(gibbs_samples$s, 1)
  if(scale$parameter == "a"){
    factor = scale$value / gibbs_samples$alpha[,scale$index]
    alpha_n = scaling(gibbs_samples$alpha, factor)
    b_n = scaling(gibbs_samples$b, factor)
    Omega_n = scaling(gibbs_samples$Omega, factor^2)
    Sigma_n = scaling(gibbs_samples$Sigma, factor^2)
  }
  if(scale$parameter == "s"){
    factor = scale$value / gibbs_samples$Sigma[,paste0(scale$index,",",scale$index)]
    alpha_n = scaling(gibbs_samples$alpha, sqrt(factor))
    b_n = scaling(gibbs_samples$b, sqrt(factor))
    Omega_n = scaling(gibbs_samples$Omega, factor)
    Sigma_n = scaling(gibbs_samples$Sigma, factor)
  }
  gibbs_samples_n = list("s"     = s_n,
                         "alpha" = alpha_n,
                         "b"     = b_n,
                         "Omega" = Omega_n,
                         "Sigma" = Sigma_n)
  gibbs_samples_n = gibbs_samples_n[lengths(gibbs_samples_n) != 0]

  ### function to burn samples
  burn = function(samples){
    if(is.null(samples)) NULL else samples[(B+1):R,,drop=FALSE]
  }

  ### burning of normalized samples
  s_nb = burn(s_n)
  alpha_nb = burn(alpha_n)
  b_nb = burn(b_n)
  Omega_nb = burn(Omega_n)
  Sigma_nb = burn(Sigma_n)
  gibbs_samples_nb = list("s"     = s_nb,
                          "alpha" = alpha_nb,
                          "b"     = b_nb,
                          "Omega" = Omega_nb,
                          "Sigma" = Sigma_nb)
  gibbs_samples_nb = gibbs_samples_nb[lengths(gibbs_samples_nb) != 0]

  ### function to thin samples
  thin = function(samples, end){
    if(any(is.null(samples))) NULL else samples[seq(1,end,Q),,drop=FALSE]
  }

  ### thinning of normalized samples
  s_nt = thin(s_n,R)
  alpha_nt = thin(alpha_n,R)
  b_nt = thin(b_n,R)
  Omega_nt = thin(Omega_n,R)
  Sigma_nt = thin(Sigma_n,R)
  gibbs_samples_nt = list("s"     = s_nt,
                          "alpha" = alpha_nt,
                          "b"     = b_nt,
                          "Omega" = Omega_nt,
                          "Sigma" = Sigma_nt)
  gibbs_samples_nt = gibbs_samples_nt[lengths(gibbs_samples_nt) != 0]

  ### thinning of normalized and burned samples
  s_nbt = thin(s_nb,R-B)
  alpha_nbt = thin(alpha_nb,R-B)
  b_nbt = thin(b_nb,R-B)
  Omega_nbt = thin(Omega_nb,R-B)
  Sigma_nbt = thin(Sigma_nb,R-B)
  gibbs_samples_nbt = list("s"     = s_nbt,
                           "alpha" = alpha_nbt,
                           "b"     = b_nbt,
                           "Omega" = Omega_nbt,
                           "Sigma" = Sigma_nbt)
  gibbs_samples_nbt = gibbs_samples_nbt[lengths(gibbs_samples_nbt) != 0]

  ### build and add class to 'gibbs_samples'
  gibbs_samples = list("gibbs_samples"     = gibbs_samples,
                       "gibbs_samples_n"   = gibbs_samples_n,
                       "gibbs_samples_nb"  = gibbs_samples_nb,
                       "gibbs_samples_nt"  = gibbs_samples_nt,
                       "gibbs_samples_nbt" = gibbs_samples_nbt)
  class(gibbs_samples) = "RprobitB_gibbs_samples"

  ### return list of transformed Gibbs samples
  return(gibbs_samples)
}
