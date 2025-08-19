#' Gibbs sample mode
#'
#' @description
#' This function approximates the Gibbs sample mode.
#'
#' @param samples \[`numeric()`\]\cr
#' Gibbs samples.
#'
#' @return
#' The (approximated) mode.
#'
#' @export
#'
#' @examples
#' samples <- oeli::rmixnorm(
#'   n = 1000, mean = matrix(c(-2, 2), ncol = 2),
#'   Sigma = matrix(c(1, 1), ncol = 2), proportions = c(0.7, 0.3)
#' )
#' hist(samples)
#' mean(samples) # expected: 0.7 * (-2) + 0.3 * 2 = -0.8
#' mode(samples) # expected: -2

mode <- function(samples) {
  oeli::input_check_response(
    check = list(
      oeli::check_numeric_vector(samples, any.missing = FALSE),
      checkmate::check_matrix(samples, mode = "numeric", any.missing = FALSE)
    ),
    var_name = "samples"
  )
  samples <- as.vector(samples) |> stats::na.omit()
  if (length(samples) == 0) {
    return(NA_real_)
  }
  if (length(unique(samples)) == 1) {
    return(samples[1])
  }
  den <- oeli::quiet(stats::density(samples))
  den$x[den$y == max(den$y)][1]
}
