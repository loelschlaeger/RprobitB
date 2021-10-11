#' Compute log-likelihood of an \code{RprobitB_model}.
#' @description
#' This function computes the log-likelihood of an \code{RprobitB_model}.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @return
#' A numeric value.
#' @examples
#' ### probit model
#' p = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2, seed = 1)
#' m1 = fit(data = p, seed = 1)
#' compute_loglikelihood(m1)
#' @export

compute_loglikelihood = function(x) {


}
