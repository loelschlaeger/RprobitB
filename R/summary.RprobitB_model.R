#' Summary method for \code{RprobitB_model}
#' @param object
#' An object of class \code{RprobitB_model}
#' @param ...
#' ignored
#' @export

summary.RprobitB_model = function(object, ... ) {

  if(!inherits(object, "RprobitB_model"))
    stop("Not of class 'RprobitB_model'.")

  ### build 'summary.RprobitB_model' object
  out = list("form" = object$RprobitB_data$form,
             "R" = object$R,
             "B" = object$B,
             "Q" = object$Q,
             "P_f" = object$RprobitB_data$P_f,
             "P_r" = object$RprobitB_data$P_r,
             "cov_fix" = object$RprobitB_data$cov_fix,
             "cov_random" = object$RprobitB_data$cov_random,
             "J" = object$RprobitB_data$J,
             "alternatives" = object$RprobitB_data$alternatives,
             "scale" = object$scale,
             "latent_classes" = object$latent_classes,
             "prior" = object$prior,
             "statistics" = object$statistics,
             "simulated" = object$RprobitB_data$simulated,
             "parm" = object$RprobitB_data$parm)
  class(out) = "summary.RprobitB_model"

  ### return 'summary.RprobitB_model' object
  return(out)
}
