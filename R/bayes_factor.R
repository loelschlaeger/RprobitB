#' Approximation of the model's marginal likelihood
#'
#' @description
#' This function approximates the model's marginal likelihood.
#'
#' @details
#' The model's marginal likelihood \eqn{p(y\mid M)} for a model \eqn{M} and data
#' \eqn{y} is required for the computation of Bayes factors. In general, the term
#' has no closed form and must be approximated numerically. This function offers
#' the following approximation methods. The strong law of large numbers guarantees
#' (almost surely), that the following approximations converge to the model's
#' marginal likelihood as the sample size \code{S} goes to infinity.
#'
#' ## The prior arithmetic mean estimator
#' Set \code{method = "pame"} to compute the prior arithmetic mean estimate.
#' For this approach, \code{S} samples \eqn{\theta_1,\dots,\theta_S}
#' are drawn from the model's prior distribution. Then,
#' \deqn{p(y\mid M) = \mathbb{E}_\text{prior} p(y\mid \theta,M) \approx \frac{1}{S}
#' \sum_s p(y\mid \theta_s,M).}
#'
#' ## The posterior harmonic mean estimator
#' Set \code{method = "phme"} to compute the posterior harmonic mean estimate.
#' For this approach, \code{S} samples \eqn{\theta_1,\dots,\theta_S}
#' are drawn from the model's posterior distribution. Then,
#' \deqn{p(y\mid M) = \left(\mathbb{E}_\text{posterior} p(y\mid \theta,M)^{-1}
#' \right)^{-1} \approx \left( \frac{1}{S} \sum_s 1/p(y\mid \theta_s,M) \right) ^{-1}.}
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#' @param S
#' The sample size.
#' @param method
#' The approximation method, one of
#' \itemize{
#'   \item \code{"pame"} for the prior arithmetic mean estimate,
#'   \item \code{"phme"} for the posterior harmonic mean estimate.
#' }
#' @param print_progress
#' Set to \code{TRUE} to print computation progress.
#' @param check_conv
#' Set to \code{TRUE} to plot the convergence behavior of the approximation.
#' @param ncores
#' Computation is parallelized, set the number of cores.
#' @param seq
#' Set to \code{TRUE} to add the attribute \code{"seq"} to the output
#' which is a vector containing the approximation sequence.
#'
#' @return
#' The model's marginal likelihood value.
#'
#' @examples
#' data <- simulate_choices(
#'   form = choice ~ cov | 0,
#'   N = 10,
#'   T = 10,
#'   J = 2,
#'   seed = 1
#' )
#' x <- mcmc(data, R = 1000, print_progress = FALSE)
#' ### S = 10 is too small for a good approximation
#' RprobitB:::mml(x = x, S = 10, method = "pame", check_conv = TRUE)
#'
#' @keywords
#' internal
#'
#' @importFrom progress progress_bar
#' @importFrom rlang .data
#' @importFrom foreach %dopar%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW

mml <- function(x, S = 100, method = "pame", print_progress = TRUE,
                check_conv = TRUE, ncores = 1, seq = FALSE) {

  ### input checks
  if(class(x) != "RprobitB_fit"){
    stop("'x' must be of class 'RprobitB_fit.")
  }
  if(!(is.numeric(S) && length(S)==1 && S>0 && S%%1==0)){
    stop("'S' must be an integer.")
  }
  if(!(is.character(method) && length(method)==1 && method %in% c("pame", "phme"))){
    stop("'method' must be one of 'pame' or 'phme'.")
  }

  ### helper variables
  cont <- numeric(S)
  add_args <- list(P_f = x$data$P_f,
                   P_r = x$data$P_r,
                   J = x$data$J,
                   N = x$data$N,
                   C = x$latent_classes$C,
                   sample = FALSE)

  ### progress bar
  if(print_progress){
    pb <- progress_bar$new(
      format = "Computing mml: :percent",
      total = S,
      clear = FALSE)
  }

  ### loop over samples
  if(method == "pame"){
    cont <- numeric(S)
    for(s in 1:S) {
      if(print_progress) pb$tick()
      prior_sample <- draw_from_prior(x$prior, C  = x$latent_classes$C)
      par <- do.call(what = RprobitB_parameter, args = c(prior_sample, add_args))
      cont[s] <- exp(log_likelihood(x, par_set = par))
    }
    mml_value <- sum(cont)/S
    approx_seq <- cumsum(cont)/seq_along(cont)
    if(seq) attr(mml_value, "seq") <- approx_seq
  }
  if(method == "phme"){
    posterior_samples <- posterior_pars(x = x, S = S)
    cont <- numeric(S)
    for(s in 1:S) {
      if(print_progress) pb$tick()
      cont[s] <- 1/exp(log_likelihood(x, par_set = posterior_samples[[s]]))
    }
    mml_value <- 1/(sum(cont)/S)
    approx_seq <- 1/(cumsum(cont)/seq_along(cont))
    if(seq) attr(mml_value, "seq") <- approx_seq
  }

  ### plot sequence
  if(check_conv){
    p <- ggplot2::ggplot(data = data.frame("approx_seq" = approx_seq),
                         ggplot2::aes(x = 1:S, y = .data$approx_seq)) +
      ggplot2::geom_line() +
      ggplot2::labs(
        x = "Number of samples",
        y = "Marginal likelihood",
        title = "The marginal likelihood value for different sample sizes") +
      ggplot2::theme_minimal()
    print(p)
  }

  ### compute and return model's marginal likelihood
  return(mml_value)
}

#' Sample from prior distributions
#'
#' @description
#' This function returns a sample from each parameter's prior distribution.
#'
#' @param prior
#' An object of class \code{RprobitB_prior}, which is the output of
#' \code{\link{check_prior}}.
#' @param C
#' The number of latent classes.
#'
#' @return
#' A list of draws for \code{alpha}, \code{s}, \code{b}, \code{Omega}, and
#' \code{Sigma} (if specified for the model).
#'
#' @keywords
#' internal
#'
#' @examples
#' prior <- check_prior(P_f = 1, P_r = 2, J = 3)
#' RprobitB:::draw_from_prior(prior, C = 2)

draw_from_prior <- function(prior, C = 1) {

  ### input checks
  if(class(prior) != "RprobitB_prior"){
    stop("'prior' must be of class 'RprobitB_prior.")
  }

  ### alpha ~ MVN(eta,Psi)
  if(identical(prior$eta,NA) || identical(prior$Psi,NA)){
    alpha <- NULL
  } else {
    alpha <- rmvnorm(mu = prior$eta, Sigma = prior$Psi)
  }

  ### s ~ D(delta)
  if(identical(prior$delta,NA)){
    s <- NULL
  } else {
    s <- sort(rdirichlet(rep(prior$delta,C)), decreasing = TRUE)
  }

  ### b_c ~ MVN(xi,D) for all c
  if(identical(prior$xi,NA) || identical(prior$D,NA)){
    b <- NULL
  } else {
    b <- matrix(replicate(C, rmvnorm(mu = prior$xi, Sigma = prior$D)), ncol = C)
  }

  ### Omega_c ~ IW(nu,Theta) for all c
  if(identical(prior$nu,NA) || identical(prior$Theta,NA)){
    Omega <- NULL
  } else {
    Omega <- matrix(replicate(C, rwishart(nu = prior$nu, V = prior$Theta)$IW), ncol = C)
  }

  ### Sigma ~ IW(kappa,E)
  if(identical(prior$kappa,NA) || identical(prior$E,NA)){
    Sigma <- NULL
  } else {
    Sigma <-  rwishart(nu = prior$kappa, V = prior$E)$IW
  }

  ### return draws
  draws <- list("alpha" = alpha,
                "s" = s,
                "b" = b,
                "Omega" = Omega,
                "Sigma" = Sigma)
  return(draws)
}
