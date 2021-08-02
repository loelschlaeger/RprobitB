#' Fit a (latent class) (mixed) (multinomial) probit model
#' @description
#' Function that fits a (latent class) (mixed) (multinomial) probit model via
#' Bayesian estimation.
#' @details
#' For more details see the vignette "Model fitting":
#' \code{vignette("model_fitting", package = "RprobitB")}
#' @inheritParams compute_suff_statistics
#' @param R
#' The number of Gibbs sampler iterations.
#' @param B
#' The length of the burn-in period, i.e. a non-negative number of samples to
#' be discarded.
#' @param Q
#' The thinning factor for the Gibbs samples, i.e. only every \code{Q}th
#' sample is kept.
#' @param print_progress
#' A boolean, determining whether to print the progress.
#' @inheritParams check_lcus
#' @inheritParams check_prior
#' @return
#' An object of class \code{RprobitB_model}
#' @examples
#' fit(data = simulate(form = choice ~ var, N = 10, T = 10, J = 3, re = "ASC"))
#' @export

fit = function(data, scale = list("parameter" = "s", "index" = 1, "value" = 1),
               R = 1e4, B = R/2, Q = 10, print_progress = TRUE, lcus = NULL,
               prior = NULL) {

  ### check inputs
  if(!inherits(data,"RprobitB_data"))
    stop("'data' must an object of class 'RprobitB_data', i.e. the output of
         'RprobitB::prepare()' or 'RprobitB::simulate()'.")
  if(!is.natural.number(R) || !R>0)
    stop("'R' must be a positive integer.")
  if(!is.natural.number(B) || !B>0 || !B<R)
    stop("'B' must be a positive integer smaller than 'R'.")
  if(!is.natural.number(Q) || !Q>0 || !Q<R)
    stop("'Q' must be a positive integer smaller than 'R'.")
  if(!is.logical(print_progress))
    stop("'progress' must be a boolean.")
  if(data$P_r==0){
    lcus = list("do_lcus" = FALSE)
  } else {
    lcus = check_lcus(lcus = lcus)
  }
  prior = check_prior(prior = prior, P_f = data$P_f, P_r = data$P_r, J = data$J)

  ### compute sufficient statistics
  suff_statistics = compute_suff_statistics(data = data, scale = scale)

  ### set initial values for the Gibbs sampler
  init = set_init(N = data$N, T = data$T, J = data$J, P_f = data$P_f,
                  P_r = data$P_r, C = data$C, lcus = lcus)

  ### perform Gibbs sampling
  gibbs_samples_raw = gibbs_sampling(
    R = R, B = B, print_progress = print_progress, N = data$N, Jm1 = data$J-1,
    P_f = data$P_f, P_r = data$P_r, C = data$C, lcus = lcus,
    suff_statistics = suff_statistics, prior = prior, init = init)

  ### normalize, burn and thin Gibbs samples
  gibbs_samples = transform_gibbs_samples(
    gibbs_samples_raw = gibbs_samples_raw, R = R, B = B, Q = Q, scale = scale)

  ### compute statistics from Gibbs samples
  statistics = compute_statistics(
    gibbs_samples = gibbs_samples, P_f = data$P_f, P_r = data$P_r, J = data$J,
    C = data$C)

  ### build RprobitB_model
  out = RprobitB_model(RprobitB_data = data,
                       scale         = scale,
                       R             = R,
                       B             = B,
                       Q             = Q,
                       lcus          = lcus,
                       prior         = prior,
                       gibbs_samples = gibbs_samples,
                       statistics    = statistics)

  ### return RprobitB_model
  return(out)

}
