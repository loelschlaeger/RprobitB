#' Change the length of the burn-in period, the thinning factor and the scale
#' of a fitted \code{RprobitB_model}.
#' @description
#' Given a fitted \code{RprobitB_model}, this function can:
#' \itemize{
#'   \item change the length \code{B} of the burn-in period and the thinning
#'         factor \code{Q} of the Gibbs sampler,
#'   \item change the model \code{scale}.
#' }
#' @details
#' For more details see the vignette "Model fitting":
#' \code{vignette("model_fitting", package = "RprobitB")}.
#' @inheritParams fit
#' @param x
#' An object of class \code{RprobitB_model}.
#' @param check_preference_flip
#' If \code{TRUE} check for flip in preferences with new scale.
#' @return
#' An object of class \code{RprobitB_model}.
#' @examples
#' \dontrun{
#' ### fit a model to simulated data
#' data = simulate(form = choice ~ var | 1, N = 100, T = 10, J = 2,
#'                 Sigma = 2, alpha = c(0.5,2))
#' scale = list("parameter" = "s", "index" = 1, "value" = 1)
#' m1 = fit(data = data, R = 1e4, B = 1, Q = 1, scale = scale)
#'
#' ### change 'B' and 'Q'
#' m2 = transform(m1, B = 5e3, Q = 10)
#'
#' ### change 'scale'
#' scale_new = list("parameter" = "a", "index" = 1, "value" = 1)
#' m3 = transform(m2, scale = scale_new)
#' }
#' @export

transform = function(x, B = NULL, Q = NULL, scale = NULL, check_preference_flip = TRUE) {

  ### check inputs
  if(!inherits(x,"RprobitB_model"))
    stop("'x' must be of class 'RprobitB_model'.")
  if(is.null(B)){
    B = x$B
  } else {
    x$B = B
  }
  if(is.null(Q)){
    Q = x$Q
  } else {
    x$Q = Q
  }
  R = x$R
  P_f = x$data$P_f
  J = x$data$J
  if(!is.numeric(B) || !B%%1 == 0 || !B>0 || !B<R)
    stop("'B' must be a positive integer smaller than 'R'.")
  if(!is.numeric(Q) || !Q%%1 == 0 || !Q>0 || !Q<R)
    stop("'Q' must be a positive integer smaller than 'R'.")
  if(is.null(scale)){
    normalization = x$normalization
  } else {
    ### check if new scale flips preferences
    if(check_preference_flip){
      model_new = transform(x = x, scale = scale, check_preference_flip = FALSE)
      preference_flip(model_old = x, model_new = model_new)
    }
    normalization = RprobitB_normalization(J = J, P_f = P_f, scale = scale)
    x$normalization = normalization
  }

  ### scale, burn and thin Gibbs samples
  gibbs_samples = transform_gibbs_samples(
    gibbs_samples = x$gibbs_samples$gibbs_samples, R = R, B = B, Q = Q,
    normalization = normalization)
  x$gibbs_samples = gibbs_samples

  ### compute statistics from Gibbs samples
  statistics = compute_parameter_statistics(
    gibbs_samples = gibbs_samples, P_f = x$data$P_f,
    P_r = x$data$P_r, J = x$data$J,
    C = x$latent_classes$C)
  x$statistics = statistics

  ### scale true parameters
  if(x$data$simulated)
    x$data$true_parameter = transform_parameter(
      parameter = x$data$true_parameter, normalization = normalization)

  ### return 'RprobitB_model'
  return(x)
}


