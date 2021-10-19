#' Summary method for \code{RprobitB_model}.
#' @param object
#' An object of class \code{RprobitB_model}.
#' @param ...
#' Ignorded.
#' @export

summary.RprobitB_model = function(object, ...) {

  ### check class of 'object'
  if(!inherits(object, "RprobitB_model"))
    stop("Not of class 'RprobitB_model'.")

  ### determine estimated number of latent classes
  if(object$latent_classes$update){
    s_draws = object$gibbs_samples$gibbs_samples$s_draws
    last_s_draw = tail(s_draws,1)
    C_est = sum(last_s_draw != 0)
  } else {
    C_est = NA
  }

  ### compute statistics from 'gibbs_samples'
  if(object$latent_classes$update){
    if(object$data$simulated){
      C = max(object$latent_classes$Cmax, object$data$true_parameter$C)
    } else {
      C = object$latent_classes$Cmax
    }
  } else {
    if(object$data$simulated){
      C = max(object$latent_classes$C, object$data$true_parameter$C)
    } else {
      C = object$latent_classes$C
    }
  }
  parameter_statistics = compute_parameter_statistics(
    gibbs_samples = object$gibbs_samples,
    P_f = object$data$P_f, P_r = object$data$P_r, J = object$data$J, C = C)

  ### build 'summary.RprobitB_model' object
  out = list("form" = object$data$form,
             "R" = object$R,
             "B" = object$B,
             "Q" = object$Q,
             "P_f" = object$data$P_f,
             "P_r" = object$data$P_r,
             "covs" = object$data$covs,
             "J" = object$data$J,
             "alternatives" = object$data$alternatives,
             "normalization" = object$normalization,
             "latent_classes" = object$latent_classes,
             "prior" = object$prior,
             "parameter_statistics" = parameter_statistics,
             "C_est" = C_est,
             "simulated" = object$data$simulated,
             "true_parameter" = object$data$true_parameter)
  class(out) = "summary.RprobitB_model"

  ### return 'summary.RprobitB_model' object
  return(out)
}
