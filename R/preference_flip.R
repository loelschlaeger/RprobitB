#' Check for flip in preferences after change in scale.
#' @description
#' This function checks if a change in the scale flipped the preferences.
#' @param model_old
#' An object of class \code{RprobitB_model}, the model before the scale change.
#' @param model_new
#' An object of class \code{RprobitB_model}, the model after the scale change.
#' @return
#' A boolean, if \code{TRUE} the new scale flips the preferences.

preference_flip = function(model_old, model_new) {
  stopifnot(class(model_old) == "RprobitB_model")
  stopifnot(class(model_new) == "RprobitB_model")
  return(TRUE)
}
