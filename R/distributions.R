#' @inherit ddirichlet_cpp title description return
#' @inheritParams ddirichlet_cpp
#'
#' @examples
#' x <- c(0.5,0.3,0.2)
#' concentration <- 1:3
#' ddirichlet(x = x, concentration = concentration)
#' ddirichlet(x = x, concentration = concentration, log = TRUE)
#'
#' @export
#' @keywords internal utils

ddirichlet <- function(x, concentration, log = FALSE) {
  stopifnot(
    is.numeric(x), is.vector(x), sum(x) == 1, all(x >= 0), all(x <= 1),
    is.numeric(concentration), is.vector(concentration), is_true_false(log),
    length(x) == length(concentration)
  )
  ddirichlet_cpp(x, concentration, log)
}

#' @inherit rdirichlet_cpp title description return
#' @inheritParams rdirichlet_cpp
#'
#' @examples
#' rdirichlet(concentration = 1:3)
#'
#' @export
#' @keywords internal utils

rdirichlet <- function(concentration) {
  stopifnot(is.numeric(concentration), is.vector(concentration))
  as.vector(rdirichlet_cpp(concentration))
}

#' @inherit dmvnorm_cpp title description return
#' @inheritParams dmvnorm_cpp
#'
#' @examples
#' x <- c(0,0)
#' mean <- c(0,0)
#' Sigma <- diag(2)
#' dmvnorm(x = x, mean = mean, Sigma = Sigma)
#' dmvnorm(x = x, mean = mean, Sigma = Sigma, log = TRUE)
#'
#' @export
#' @keywords internal utils

dmvnorm <- function(x, mean, Sigma, log = FALSE) {
  stopifnot(
    is.numeric(x), is.vector(x), is.numeric(mean), is.vector(mean),
    is.matrix(Sigma), is_covariance_matrix(Sigma), length(x) == length(mean),
    length(mean) == dim(Sigma)[1], is_true_false(log)
  )
  dmvnorm_cpp(x, mean, Sigma, log)
}

#' @inherit rmvnorm_cpp title description return
#' @inheritParams rmvnorm_cpp
#'
#' @examples
#' mean <- c(0,0)
#' Sigma <- diag(2)
#' rmvnorm(mean = mean, Sigma = Sigma)
#' rmvnorm(mean = mean, Sigma = Sigma, log = TRUE)
#'
#' @export
#' @keywords internal utils

rmvnorm <- function(mean, Sigma, log = FALSE) {
  stopifnot(
    is.numeric(mean), is.vector(mean),
    is.matrix(Sigma), is_covariance_matrix(Sigma),
    length(mean) == dim(Sigma)[1], is_true_false(log)
  )
  as.vector(rmvnorm_cpp(mean, Sigma, log))
}

#' @inherit dwishart_cpp title description return
#' @inheritParams dwishart_cpp
#'
#' @examples
#' x <- diag(2)
#' df <- 4
#' scale <- diag(2)
#' dwishart(x = x, df = df, scale = scale)
#' dwishart(x = x, df = df, scale = scale, log = TRUE)
#' dwishart(x = x, df = df, scale = scale, inv = TRUE)
#'
#' @export
#' @keywords internal utils

dwishart <- function(x, df, scale, log = FALSE, inv = FALSE) {
  stopifnot(
    is.numeric(x), is.matrix(x), is_covariance_matrix(x),
    is_positive_integer(df), df >= dim(x)[1], is.matrix(scale), is_covariance_matrix(scale),
    dim(x)[1] == dim(scale)[1], is_true_false(log), is_true_false(inv)
  )
  dwishart_cpp(x, df, scale, log, inv)
}

#' @inherit rwishart_cpp title description return
#' @inheritParams rwishart_cpp
#'
#' @examples
#' df <- 4
#' scale <- diag(2)
#' rwishart(df = df, scale = scale)
#' rwishart(df = df, scale = scale, inv = TRUE)
#'
#' @export
#' @keywords internal utils

rwishart <- function(df, scale, inv = FALSE) {
  stopifnot(
    is_positive_integer(df), is.matrix(scale), is_covariance_matrix(scale), is_true_false(inv)
  )
  rwishart_cpp(df, scale, inv)
}

