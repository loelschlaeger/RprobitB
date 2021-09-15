#' Transform fitted \code{RprobitB_model}.
#' @description
#' Given an \code{RprobitB_model}, this function can:
#' \itemize{
#'   \item change the length \code{B} of the burn-in period,
#'   \item change the thinning factor \code{Q},
#'   \item change the utility normalization \code{scale}.
#' }
#' @details
#' For more details see the vignettes
#' "Model fitting":
#' \code{vignette("model_fitting", package = "RprobitB")} and
#' "Model checking and model selection":
#' \code{vignette("model_checking_and_model_selection", package = "RprobitB")}.
#' @inheritParams fit
#' @param model
#' An object of class \code{RprobitB_model}.
#' @return
#' An object of class \code{RprobitB_model}.
#' @examples
#' ### fit a model to simulated data
#' data = simulate(form = choice ~ var | 1, N = 100, T = 10, J = 2)
#' scale = list("parameter" = "s", "index" = 1, "value" = 1)
#' model = fit(data = data, R = 1e4, B = 1, Q = 1)
#'
#' ### change 'B' and 'Q'
#' model = transform(model = model, B = 1e4/2, Q = 10)
#'
#' ### change 'scale'
#' scale = list("parameter" = "a", "index" = 1, "value" = -1)
#' model = transform(model = model, scale = scale)
#' @export

transform = function(model, B = NULL, Q = NULL, scale = NULL) {

  ### check inputs
  if(!inherits(model,"RprobitB_model"))
    stop("'model' must be of class 'RprobitB_model'.")
  if(is.null(B)){
    B = model$B
  } else {
    model$B = B
  }
  if(is.null(Q)){
    Q = model$Q
  } else {
    model$Q = Q
  }
  R = model$R
  P_f = model$RprobitB_data$P_f
  J = model$RprobitB_data$J
  if(!is.numeric(B) || !B%%1 == 0 || !B>0 || !B<R)
    stop("'B' must be a positive integer smaller than 'R'.")
  if(!is.numeric(Q) || !Q%%1 == 0 || !Q>0 || !Q<R)
    stop("'Q' must be a positive integer smaller than 'R'.")
  if(is.null(scale)){
    scale = model$scale
  } else {
    scale = RprobitB_scale(scale = scale, P_f = P_f, J = J)
    model$scale = scale
  }

  ### normalize, burn and thin Gibbs samples
  gibbs_samples = transform_gibbs_samples(
    gibbs_samples = model$gibbs_samples$gibbs_samples, R = R, B = B, Q = Q,
    scale = scale)
  model$gibbs_samples = gibbs_samples

  ### compute statistics from Gibbs samples
  statistics = compute_parameter_statistics(
    gibbs_samples = gibbs_samples, P_f = model$RprobitB_data$P_f,
    P_r = model$RprobitB_data$P_r, J = model$RprobitB_data$J,
    C = model$latent_classes$C)
  model$statistics = statistics

  ### scale true parameters
  if(model$RprobitB_data$simulated)
    model$RprobitB_data$true_parameter = transform_parm(
      parm = model$RprobitB_data$parm, scale = scale)

  ### return 'RprobitB_model'
  return(model)

}

#' Transformation of true parameter values.
#' @description
#' This function transforms the true parameter values 'true_parameter' based
#' on 'scale'.
#' @param true_parameter
#' An object of class \code{RprobitB_parameter} with true model parameters.
#' @inheritParams RprobitB_scale
#' @examples
#' true_parameter = RprobitB_parameter(P_f = 2, P_r = 2, J = 3, N = 100)
#' scale = RprobitB_scale(scale = NULL, P_f = 2, J = 3)
#' transform_parm(true_parameter = true_parameter, scale = scale)
#' @return
#' An object of class \code{RprobitB_parameter}.

transform_true_parameter = function(true_parameter, scale) {

  ### check inputs
  if(!inherits(true_parameter, "RprobitB_parameter"))
    stop("'true_parameter' must be of class 'RprobitB_parameter'.")
  if(!inherits(scale, "RprobitB_scale"))
    stop("'scale' must be of class 'RprobitB_scale'.")

  ### function to normalize the parameters
  normalize = function(par, factor){
    if(any(is.na(par))){
      return(NA)
    } else {
      return(par * factor)
    }
  }

  ### scale elements of 'true_parameter'
  if(scale$parameter=="a"){
    factor = scale$value / true_parameter$alpha[scale$index]
    true_parameter$alpha = normalize(true_parameter$alpha, factor)
    true_parameter$b = normalize(true_parameter$b, factor)
    true_parameter$Omega = normalize(true_parameter$Omega, factor^2)
    true_parameter$Sigma = normalize(true_parameter$Sigma, factor^2)
    true_parameter$beta = normalize(true_parameter$beta, factor)
  }
  if(scale$parameter=="s"){
    factor = scale$value / true_parameter$Sigma[scale$index,scale$index]
    true_parameter$alpha = normalize(true_parameter$alpha, sqrt(factor))
    true_parameter$b = normalize(true_parameter$b, sqrt(factor))
    true_parameter$Omega = normalize(true_parameter$Omega, factor)
    true_parameter$Sigma = normalize(true_parameter$Sigma, factor)
    true_parameter$beta = normalize(true_parameter$beta, sqrt(factor))
  }

  ### return 'true_parameter'
  return(true_parameter)
}

#' Transformation of Gibbs samples.
#' @description
#' This function normalizes, burns and thins the Gibbs samples.
#' @param gibbs_samples
#' The output of \code{\link{gibbs_sampling}}, i.e. a list of Gibbs samples.
#' @inheritParams RprobitB_data
#' @inheritParams fit
#' @inheritParams compute_sufficient_statistics
#' @return
#' A list of transformed Gibbs samples. Each element is a list, containing the
#' Gibbs samples for \code{s}, \code{alpha}, \code{b}, \code{Omega}, and
#' \code{Sigma} (if available):
#' \itemize{
#'   \item \code{gibbs_samples}:
#'   The function input \code{gibbs_samples}
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

transform_gibbs_samples = function(gibbs_samples, R, B, Q, scale) {

  ### check inputs
  if(!inherits(scale, "RprobitB_scale"))
    stop("'scale' must be of class 'RprobitB_scale'.")

  ### determine estimated number of latent classes
  last_s_draw = gibbs_samples$s_draws[nrow(gibbs_samples$s_draws),]
  C_est = length(last_s_draw[last_s_draw!=0])

  ### function to normalize the samples
  normalize = function(par, factor) if(any(is.na(par))) NA else par * factor

  ### normalization of samples
  s_draws_n = normalize(gibbs_samples$s_draws, 1)
  if(scale$parameter=="a"){
    factor = scale$value / gibbs_samples$alpha_draws[,scale$index]
    alpha_draws_n = normalize(gibbs_samples$alpha_draws, factor)
    b_draws_n = normalize(gibbs_samples$b_draws, factor)
    Omega_draws_n = normalize(gibbs_samples$Omega_draws, factor^2)
    Sigma_draws_n = normalize(gibbs_samples$Sigma_draws, factor^2)
  }
  if(scale$parameter=="s"){
    Jm1 = sqrt(length(gibbs_samples$Sigma_draws[1,]))
    factor = scale$value / gibbs_samples$Sigma_draws[,(Jm1)*(scale$index-1)+scale$index]
    alpha_draws_n = normalize(gibbs_samples$alpha_draws, sqrt(factor))
    b_draws_n = normalize(gibbs_samples$b_draws, sqrt(factor))
    Omega_draws_n = normalize(gibbs_samples$Omega_draws, factor)
    Sigma_draws_n = normalize(gibbs_samples$Sigma_draws, factor)
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
  gibbs_samples = list("gibbs_samples"     = gibbs_samples,
                       "gibbs_samples_n"   = gibbs_samples_n,
                       "gibbs_samples_nb"  = gibbs_samples_nb,
                       "gibbs_samples_nt"  = gibbs_samples_nt,
                       "gibbs_samples_nbt" = gibbs_samples_nbt)
  class(gibbs_samples) = "RprobitB_gibbs_samples"

  ### return list of transformed Gibbs samples
  return(gibbs_samples)
}


