#' Classify deciders.
#' @description
#' This function classifies deciders based on the estimated latent classes.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @return
#' A data frame with the deciders id and the latent class number.
#' @export

preference_classification <- function(x) {
  if (is.null(x$classification)) {
    warning("No classification available.")
  } else {
    data.frame(
      id = unique(x$data$choice_data$id),
      class = x$classification
    )
  }
}
