#' Extract estimated covariance matrix of mixing distribution
#'
#' @description
#' This helper function returns the estimated covariance matrix of the
#' mixing distribution.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#'
#' @param cor \[`integer(1)`\]\cr
#' Return the correlation matrix instead?
#'
#' @return
#' The estimated covariance matrix of the mixing distribution. In case of
#' multiple classes, a list of matrices for each class.
#'
#' @export

cov_mix <- function(x, cor = FALSE) {
  oeli::input_check_response(
    check = checkmate::check_class(x, "RprobitB_fit"),
    var_name = "x"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(cor),
    var_name = "cor"
  )
  if (x$data$P_r == 0) stop("No random effects.", call. = FALSE)
  est_Omega <- point_estimates(x)$Omega
  random <- NULL
  cov_names <- subset(x$data$effects, random == TRUE)$effect
  out <- list()
  for (c in 1:x$latent_classes$C) {
    out[[c]] <- matrix(est_Omega[, c], nrow = x$data$P_r)
    colnames(out[[c]]) <- rownames(out[[c]]) <- cov_names
  }
  if (cor) {
    out <- lapply(out, stats::cov2cor)
  }
  if (x$latent_classes$C == 1) {
    return(out[[1]])
  } else {
    return(out)
  }
}
