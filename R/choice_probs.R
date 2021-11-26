#' Compute choice probabilities of an \code{RprobitB_model}.
#' @description
#' This function computes the choice probabilities of an \code{RprobitB_model}.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @param data
#' Either \code{NULL} or an object of class \code{RprobitB_data}.
#' @param at_true
#' If \code{TRUE}, choice probabilities are computed at the true parameters.
#' @return
#' A data frame, choice situations in rows and alternatives in columns.
#' @export

choice_probs <- function(x, data = NULL, at_true = FALSE) {

  ### extract parameters
  if (at_true) {
    parameter <- x$data$true_parameter
    if (is.null(parameter)) {
      stop("True parameters are not available.")
    }
  } else {
    parameter <- compute_point_estimates(x, FUN = mean)
  }

  ### choose data
  if (is.null(data)) {
    data <- x$data
  }
  if (class(data) != "RprobitB_data") {
    stop("'data' is not of class 'RprobitB_data'.")
  }

  ### compute probabilities
  probabilities <- matrix(NA, nrow = 0, ncol = data$J + 2)
  for (n in 1:data$N) {
    for (t in 1:data$T[n]) {
      P_nt <- compute_choice_probabilities(
        X = data$data[[n]]$X[[t]],
        parameter = parameter
      )
      probabilities <- rbind(probabilities, c(n, t, P_nt))
    }
  }
  probabilities <- as.data.frame(probabilities)

  ### add decision maker ids
  probabilities <- cbind(data$choice_data$id, probabilities)
  colnames(probabilities) <- c("id", "N", "T", data$alternatives)

  ### return probabilities
  out <- as.data.frame(probabilities)
  return(out)
}
