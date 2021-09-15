#' Fit a (latent class) (mixed) (multinomial) probit model.
#' @description
#' This function fits a (latent class) (mixed) (multinomial) probit model via
#' Bayesian estimation to discrete choice data.
#' @details
#' For more details see the vignette "Model fitting":
#' \code{vignette("model_fitting", package = "RprobitB")}.
#' @inheritParams compute_sufficient_statistics
#' @param R
#' The number of iterations of the Gibbs sampler.
#' @param B
#' The length of the burn-in period, i.e. a non-negative number of samples to
#' be discarded.
#' @param Q
#' The thinning factor for the Gibbs samples, i.e. only every \code{Q}th
#' sample is kept.
#' @param print_progress
#' A boolean, determining whether to print the Gibbs sampler progress and the
#' estimated remaining computation time.
#' @inheritParams check_latent_classes
#' @inheritParams check_prior
#' @param seed
#' Set a seed for the Gibbs sampling.
#' @return
#' An object of class \code{RprobitB_model}.
#' @examples
#' \dontrun{
#' ### probit model
#' p = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2, seed = 1)
#' m1 = fit(data = p, seed = 1)
#'
#' ### multinomial probit model
#' mnp = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 3, seed = 1)
#' m2 = fit(data = mnp, seed = 1)
#'
#' ### mixed multinomial probit model
#' mmnp = simulate(form = choice ~ 0 | var, N = 100, T = 10, J = 3, re = "var",
#'                 seed = 1)
#' m3 = fit(data = mmnp, seed = 1)
#'
#' ### mixed multinomial probit model with 2 latent classes
#' lcmmnp = simulate(form = choice ~ 0 | var, N = 100, T = 10, J = 3,
#'                   re = "var", parm = list("C" = 2), seed = 1)
#' m4 = fit(data = lcmmnp, latent_classes = list("C" = 2), seed = 1)
#'
#' ### update of latent classes
#' m5 = fit(data = lcmmnp, latent_classes = list("update" = TRUE), seed = 1)
#' }
#' @export

fit = function(data, scale = list("parameter" = "s", "index" = 1, "value" = 1),
               R = 1e4, B = R/2, Q = 10, print_progress = TRUE, prior = NULL,
               latent_classes = NULL, seed = NULL) {

  ### check inputs
  if(!inherits(data,"RprobitB_data"))
    stop("'data' must an object of class 'RprobitB_data', i.e. the output of
         'RprobitB::prepare()' or 'RprobitB::simulate()'.")
  if(!is.numeric(R) || !R%%1 == 0 || !R>0)
    stop("'R' must be a positive integer.")
  if(!is.numeric(B) || !B%%1 == 0 || !B>0 || !B<R)
    stop("'B' must be a positive integer smaller than 'R'.")
  if(!is.numeric(Q) || !Q%%1 == 0 || !Q>0 || !Q<R)
    stop("'Q' must be a positive integer smaller than 'R'.")
  if(!is.logical(print_progress))
    stop("'progress' must be a boolean.")
  scale = check_scale(scale = scale, P_f = data$P_f, J = data$J)
  latent_classes = check_latent_classes(latent_classes = latent_classes)
  prior = check_prior(prior = prior, P_f = data$P_f, P_r = data$P_r, J = data$J)

  ### compute sufficient statistics
  sufficient_statistics = compute_sufficient_statistics(data = data,
                                                        scale = scale)

  ### set initial values for the Gibbs sampler
  init = set_init(N = data$N, T = data$T, J = data$J, P_f = data$P_f,
                  P_r = data$P_r, C = latent_classes$C)

  ### perform Gibbs sampling
  if(!is.null(seed))
    set.seed(seed)
  gibbs_samples = gibbs_sampling(
    R = R, B = B, print_progress = print_progress, N = data$N, J = data$J,
    P_f = data$P_f, P_r = data$P_r, latent_classes = latent_classes,
    sufficient_statistics = sufficient_statistics, prior = prior, init = init)

  ### normalize, burn and thin 'gibbs_samples'
  gibbs_samples = transform_gibbs_samples(
    gibbs_samples = gibbs_samples, R = R, B = B, Q = Q, scale = scale)

  ### normalize 'data$parm' in based on 'scale'
  data$parm = transform_parm(parm = data$parm, scale = scale)

  ### compute statistics from 'gibbs_samples'
  statistics = compute_parameter_statistics(
    gibbs_samples = gibbs_samples, P_f = data$P_f, P_r = data$P_r, J = data$J,
    C = latent_classes$C)

  ### build RprobitB_model
  out = RprobitB_model(RprobitB_data = data,
                       scale = scale,
                       R = R,
                       B = B,
                       Q = Q,
                       latent_classes = latent_classes,
                       prior = prior,
                       gibbs_samples = gibbs_samples,
                       statistics = statistics)

  ### return RprobitB_model
  return(out)
}
