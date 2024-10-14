#' Check for flip in preferences after change in model scale.
#'
#' @description
#' This function checks if a change in the model scale flipped the preferences.
#'
#' @param model_old
#' An object of class \code{RprobitB_fit}, the model before the scale change.
#' @param model_new
#' An object of class \code{RprobitB_fit}, the model after the scale change.
#'
#' @return
#' No return value, called for side-effects.
#'
#' @keywords
#' internal

preference_flip <- function(model_old, model_new) {
  stopifnot(inherits(model_old, "RprobitB_fit"))
  stopifnot(inherits(model_new, "RprobitB_fit"))
  stopifnot(model_old[["data"]][["P_f"]] == model_new[["data"]][["P_f"]])
  stopifnot(model_old[["data"]][["P_r"]] == model_new[["data"]][["P_r"]])
  flag <- FALSE
  for (p in seq_len(model_old[["data"]][["P_f"]])) {
    P1 <- stats::ecdf(model_old[["gibbs_samples"]][["gibbs_samples_nbt"]][["alpha"]][, p])
    P2 <- stats::ecdf(model_new[["gibbs_samples"]][["gibbs_samples_nbt"]][["alpha"]][, p])
    if (P1(0) != P2(0)) {
      flag <- TRUE
    }
  }
  for (p in seq_len(model_old[["data"]][["P_r"]])) {
    P1 <- stats::ecdf(model_old[["gibbs_samples"]][["gibbs_samples_nbt"]][["b"]][, p])
    P2 <- stats::ecdf(model_new[["gibbs_samples"]][["gibbs_samples_nbt"]][["b"]][, p])
    if (P1(0) != P2(0)) {
      flag <- TRUE
    }
  }
  if (flag) {
    stop("This transformation seems to flip the preferences. ",
         "Set 'check_preference_flip = FALSE' to transform anyway.",
         call. = FALSE
    )
  }
}
