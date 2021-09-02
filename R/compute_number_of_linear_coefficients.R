#' Compute number of linear coefficients.
#' @description
#' This function computes the numbers \code{P_f} and \code{P_r} of linear
#' coefficients with fixed and random effects, respectively.
#' @inheritParams RprobitB_data
#' @return
#' A list with two elements:
#' \itemize{
#'   \item \code{P_f}:
#'   The number of linear coefficients with fixed effect (can be 0).
#'   \item \code{P_r}:
#'   The number of linear coefficients with random effect (can be 0).
#' }

compute_number_of_linear_coefficients = function(vars, ASC, J, re) {

  ### check inputs
  stopifnot(is.list(vars), length(vars) == 3)
  stopifnot(is.logical(ASC))
  stopifnot(is.numeric(J), J%%1 == 0, J >= 2)

  ### determine numbers P_f and P_r
  P_f_plus_P_r = length(vars[[1]]) + (length(vars[[2]]) + ASC) * (J-1) +
    length(vars[[3]]) * J
  P_r = sum(re %in% vars[[1]]) +
    (sum(re %in% vars[[2]]) + "ASC" %in% re) * (J-1) +
    sum(re %in% vars[[3]]) * J
  P_f = P_f_plus_P_r - P_r

  ### return
  out = list("P_f" = P_f, "P_r" = P_r)
  return(out)

}
