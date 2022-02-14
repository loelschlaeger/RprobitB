#' Classify preferences of deciders.
#'
#' @description
#' This function classifies the preferences of deciders based on the estimated
#' latent classes.
#'
#' @param x
#' An object of class \code{RprobitB_model}.
#'
#' @return
#' A data frame with the deciders id and the latent class number.
#'
#' @export

preference_classification <- function(x) {
  if (is.null(x$gibbs_samples$gibbs_samples_nbt$z)) {
    warning("No classification available.")
  } else {
    class <- apply(x$gibbs_samples$gibbs_samples_nbt$z, 2,
                   FUN = function(x) {
                     tab <- table(x)
                    ans <- names(tab[which.max(tab)])
                    return(as.numeric(ans))
                    })
    data.frame(
      id = unique(x$data$choice_data$id),
      class = class
    )
  }
}
