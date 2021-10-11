#' Change the length of the burn-in period, the thinning factor and the scale
#' of a fitted probit model.
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
#' @param model
#' An object of class \code{RprobitB_model}.
#' @param check_preference_flip
#' A boolean, if \code{TRUE} the function checks if the new \code{scale} flips
#' the preferences in the model.
#' @return
#' An object of class \code{RprobitB_model}.
#' @examples
#' \dontrun{
#' ### fit a model to simulated data
#' data = simulate(form = choice ~ var | 1, N = 100, T = 10, J = 2,
#'                 Sigma = 2, alpha = c(0.5,2))
#' scale = list("parameter" = "s", "index" = 1, "value" = 1)
#' model = fit(data = data, R = 1e4, B = 1, Q = 1, scale = scale)
#'
#' ### change 'B' and 'Q'
#' model = transform(model = model, B = 5e3, Q = 10)
#'
#' ### change 'scale'
#' scale_new = list("parameter" = "a", "index" = 1, "value" = 1)
#' model_new = transform(model = model, scale = scale_new)
#' }
#' @export

transform = function(model, B = NULL, Q = NULL, scale = NULL,
                     check_preference_flip = TRUE) {

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
  P_f = model$data$P_f
  J = model$data$J
  if(!is.numeric(B) || !B%%1 == 0 || !B>0 || !B<R)
    stop("'B' must be a positive integer smaller than 'R'.")
  if(!is.numeric(Q) || !Q%%1 == 0 || !Q>0 || !Q<R)
    stop("'Q' must be a positive integer smaller than 'R'.")
  if(is.null(scale)){
    normalization = model$normalization
  } else {
    ### check if new scale flips preferences
    if(check_preference_flip){
      model_new = transform(model = model, scale = scale, check_preference_flip = FALSE)
      preference_flip(model_old = model, model_new = model_new)
    }
    normalization = RprobitB_normalization(J = J, P_f = P_f, scale = scale)
    model$normalization = normalization
  }

  ### scale, burn and thin Gibbs samples
  gibbs_samples = transform_gibbs_samples(
    gibbs_samples = model$gibbs_samples$gibbs_samples, R = R, B = B, Q = Q,
    normalization = normalization)
  model$gibbs_samples = gibbs_samples

  ### compute statistics from Gibbs samples
  statistics = compute_parameter_statistics(
    gibbs_samples = gibbs_samples, P_f = model$data$P_f,
    P_r = model$data$P_r, J = model$data$J,
    C = model$latent_classes$C)
  model$statistics = statistics

  ### scale true parameters
  if(model$data$simulated)
    model$data$true_parameter = transform_parameter(
      parameter = model$data$true_parameter, normalization = normalization)

  ### return 'RprobitB_model'
  return(model)
}


