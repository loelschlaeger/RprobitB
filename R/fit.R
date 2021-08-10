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
#' @inheritParams check_latent_classes
#' @inheritParams check_prior
#' @return
#' An object of class \code{RprobitB_model}
#' @examples
#' \dontrun{
#' ### covariates with fixed effects
#' data = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2)
#' model = fit(data = data)
#' summary(model)
#'
#' ### random effects
#' data = simulate(form = choice ~ 0 | var, N = 100, T = 10, J = 2, re = "var")
#' model = fit(data = data)
#' summary(model)
#'
#' ### latent classes
#' data = simulate(form = choice ~ 0 | var, N = 100, T = 10, J = 2, re = "var",
#'                 parm = list("C" = 2))
#' model = fit(data = data, latent_classes = list("C" = 2))
#' summary(model)
#'
#' ### update of latent classes
#' data = simulate(form = choice ~ 0 | var, N = 100, T = 10, J = 2, re = "var",
#'                 parm = list("C" = 2))
#' model = fit(data = data, latent_classes = list("update" = TRUE))
#' summary(model)
#' }
#' @export

fit = function(data, scale = list("parameter" = "s", "index" = 1, "value" = 1),
               R = 1e4, B = R/2, Q = 10, print_progress = TRUE,
               latent_classes = NULL, prior = NULL) {

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
  suff_statistics = compute_suff_statistics(data = data, scale = scale)

  ### set initial values for the Gibbs sampler
  init = set_init(N = data$N, T = data$T, J = data$J, P_f = data$P_f,
                  P_r = data$P_r, C = latent_classes$C)

  ### perform Gibbs sampling
  gibbs_samples = gibbs_sampling(
    R = R, B = B, print_progress = print_progress, N = data$N, J = data$J,
    P_f = data$P_f, P_r = data$P_r, latent_classes = latent_classes,
    suff_statistics = suff_statistics, prior = prior, init = init)

  ### normalize, burn and thin 'gibbs_samples'
  gibbs_samples = transform_gibbs_samples(
    gibbs_samples = gibbs_samples, R = R, B = B, Q = Q, scale = scale)

  ### normalize 'data$parm' in based on 'scale'
  data$parm = transform_parm(parm = data$parm, scale = scale)

  ### compute statistics from 'gibbs_samples'
  statistics = compute_statistics(
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
