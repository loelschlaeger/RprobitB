### sample from prior distribution
draw_from_prior <- function(prior) {

  ### input checks
  if(class(prior) != "RprobitB_prior"){
    stop("'prior' must be of class 'RprobitB_prior.")
  }

  ### alpha ~ MVN(eta,Psi)
  if(identical(prior$eta,NA) || identical(prior$Psi,NA)){
    alpha <- NULL
  } else {
    alpha <- mvn_draw(mu = prior$eta, Sigma = prior$Psi)
  }

  ### s ~ D(delta)
  if(identical(prior$delta,NA)){
    s <- NULL
  } else {
    s <- rdirichlet(prior$delta)
  }

  ### b_c ~ MVN(xi,D)
  if(identical(prior$xi,NA) || identical(prior$D,NA)){
    b <- NULL
  } else {
    b <- mvn_draw(mu = prior$xi, Sigma = prior$D)
  }

  ### Omega_c ~ IW(nu,Theta)
  if(identical(prior$nu,NA) || identical(prior$Theta,NA)){
    Omega <- NULL
  } else {
    Omega <- rwishart(nu = prior$nu, V = prior$Theta)$IW
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
  class(draws) <- "RprobitB_prior_draws"
  return(draws)
}

### compute Bayes factor
bayes_factor <- function(marg_like1, marg_like2) {
  marg_like1 / marg_like2
}

### compute posterior odds
posterior_odds <- function(prior_odds, bayes_factor) {
  prior_odds * bayes_factor
}

### the naive monte carlo estimator of a model's marginal log-likelihood
naive_monte_carlo_estimator <- function(x, N) {

  ### input checks
  if(class(x) != "RprobitB_fit"){
    stop("'x' must be of class 'RprobitB_fit.")
  }
  if(!(is.numeric(N) && length(N)==1 && N>0 && N%%1==0)){
    stop("'N' must be an integer.")
  }

  ### loop over samples
  seq <- numeric(N)
  add_args <- list(P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J, N = x$data$N, sample = FALSE)
  for(n in 1:N){

    ### draw sample from prior
    prior_sample <- draw_from_prior(x$prior)

    ### transform to 'RprobitB_parameter'
    par <- do.call(what = RprobitB_parameter, args = c(prior_sample, add_args))

    ### compute likelihood of data wrt prior
    seq[n] <- log_likelihood(x, par_set = par)
  }

  ### average likelihoods
  avg <- sum(seq)/N
  attr(avg, "seq") <- seq
  return(avg)
}


# Example -----------------------------------------------------------------

### simulate data
b <- matrix(-1)
Omega <- matrix(1)
Sigma_full <- diag(2)
form <- y ~ x | 0
data <- simulate_choices(form = form, N = 10, T = 10, J = 2, re = "x",
                         b = b, Omega = Omega, Sigma_full = Sigma_full)
choice_data <- data$choice_data

### sparse model
data_sparse_model <- prepare_data(form = form, choice_data = choice_data, re = NULL)
sparse_model <- mcmc(data_sparse_model)
log_likelihood(sparse_model)

### mixed model
data_mixed_model <- prepare_data(form = form, choice_data = choice_data, re = "x")
mixed_model <- mcmc(data_mixed_model)
log_likelihood(mixed_model)
