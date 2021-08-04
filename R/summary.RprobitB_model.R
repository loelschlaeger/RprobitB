#' @param digits
#' @export

summary.RprobitB_model = function(object, digits = 2, ... ) {

  if(!is.RprobitB_model(object))
    stop("Not of class 'RprobitB_model'.")

  ### extract parameters
  simulated = object$RprobitB_data$simulated
  statistics = object$statistics
  true = object$RprobitB_data$parm

  ### round statistics
  statistics = lapply(statistics, function(x) round(x, digits = digits))

  ### build matrix with estimates
  est = data.frame()
  if(object$RprobitB_data$P_f>0){
    if(simulated){
      est = rbind(est, cbind(true$alpha, statistics$alpha))
    } else {
      est = rbind(est, statistics$alpha)
    }
  }
  if(object$RprobitB_data$P_r>0){
    if(simulated){
      est = rbind(est, cbind(true$s, statistics$s))
      est = rbind(est, cbind(true$b, statistics$b))
      est = rbind(est, cbind(true$Omega, statistics$Omega))
    } else {
      est = rbind(est, statistics$s)
      est = rbind(est, statistics$b)
      est = rbind(est, statistics$Omega)
    }
  }
  if(simulated){
    est = rbind(est, cbind(true$Sigma, statistics$Sigma))
  } else {
    est = rbind(est, statistics$Sigma)
  }

  ### build 'summary.RprobitB_model' object
  out = list("form" = object$RprobitB_data$form,
             "R" = object$R,
             "B" = object$B,
             "Q" = object$Q,
             "scale" = object$scale,
             "lcus" = object$lcus,
             "prior" = object$prior,
             "estimates" = est,
             "parm" = object$RprobitB_data$parm)

  class(out) = "summary.RprobitB_model"

  ### return 'summary.RprobitB_model' object
  return(out)
}
