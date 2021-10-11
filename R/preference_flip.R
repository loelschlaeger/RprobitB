#' Check for flip in preferences after change in scale.
#' @description
#' This function checks if a change in the scale flipped the preferences.
#' @param model_old
#' An object of class \code{RprobitB_model}, the model before the scale change.
#' @param model_new
#' An object of class \code{RprobitB_model}, the model after the scale change.
#' @return
#' No return value, called for side-effects.

preference_flip = function(model_old, model_new) {
  stopifnot(class(model_old) == "RprobitB_model")
  stopifnot(class(model_new) == "RprobitB_model")
  stopifnot(model_old$data$P_f == model_new$data$P_f)
  stopifnot(model_old$data$P_r == model_new$data$P_r)
  for(p in seq_len(model_old$data$P_f)){
    P1 = ecdf(model_old$gibbs_samples$gibbs_samples_nbt$alpha[,p])
    P2 = ecdf(model_new$gibbs_samples$gibbs_samples_nbt$alpha[,p])
    if(P1(0) != P2(0)){
      stop("This transformation flips preferences. ",
           "Set 'check_preference_flip = FALSE' to transform anyway.")
    }
  }
  for(p in seq_len(model_old$data$P_r)){
    P1 = ecdf(model_old$gibbs_samples$gibbs_samples_nbt$b[,p])
    P2 = ecdf(model_new$gibbs_samples$gibbs_samples_nbt$b[,p])
    if(P1(0) != P2(0)){
      stop("This transformation flips preferences. ",
           "Set 'check_preference_flip = FALSE' to transform anyway.")
    }
  }

}
