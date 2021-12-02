#' Compute WAIC value.
#' @description
#' This function computes the WAIC value of an \code{RprobitB_model}.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @return
#' The WAIC value.
#' @examples
#' model = mcmc(data = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2))
#' compute_waic(model)
#' @export

compute_waic = function(x, no_samples, pis = NULL) {
  if(is.null(pis)){
    pis <- compute_pis(x, no_samples)
  }
  lppd <- sum(log(1/ncol(pis)*rowSums(pis)))
  pwaic <- sum(apply(log(pis), 1, var))
  waic <- -2 * (lppd - pwaic)
  out <- list("lppd" = lppd, "pwaic" = pwaic, "waic" = waic)
  return(out)
}
