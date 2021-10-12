#' Compute point estimates of an \code{RprobitB_model}.
#' @description
#' This function computes the point estimates of an \code{RprobitB_model}.
#' @param object
#' An object of class \code{RprobitB_model}.
#' @return
#' An object of class \code{RprobitB_parameter}.
#' @examples
#' ### probit model
#' p = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2, seed = 1)
#' m1 = fit(data = p, seed = 1)
#' compute_point_estimates(m1)
#' @export

compute_point_estimates = function(object) {

}
