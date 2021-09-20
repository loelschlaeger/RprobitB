#' Summary method for \code{RprobitB_model}
#' @param object
#' An object of class \code{RprobitB_model}.
#' @param ...
#' ignored
#' @export

summary.RprobitB_model = function(object, ... ) {

  ### check if 'object' is of class 'RprobitB_model'
  if(!inherits(object, "RprobitB_model"))
    stop("Not of class 'RprobitB_model'.")

  ### compute statistics from 'gibbs_samples'
  statistics = compute_parameter_statistics(
    gibbs_samples = gibbs_samples, P_f = data$P_f, P_r = data$P_r, J = data$J,
    C = latent_classes$C)

  ### build 'summary.RprobitB_model' object
  out = list("form" = object$data$form,
             "R" = object$R,
             "B" = object$B,
             "Q" = object$Q,
             "P_f" = object$data$P_f,
             "P_r" = object$data$P_r,
             "cov_fix" = object$data$cov_fix,
             "cov_random" = object$data$cov_random,
             "J" = object$data$J,
             "alternatives" = object$data$alternatives,
             "scale" = object$scale,
             "latent_classes" = object$latent_classes,
             "prior" = object$prior,
             "statistics" = object$statistics,
             "simulated" = object$data$simulated,
             "true_parameter" = object$data$true_parameter)
  class(out) = "summary.RprobitB_model"

  ### return 'summary.RprobitB_model' object
  return(out)
}
