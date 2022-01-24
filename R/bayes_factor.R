### sample from prior distribution
prior_samples <- function(prior) {

  stopifnot(class(prior) == "RprobitB_prior")

  ### alpha ~ MVN(eta,Psi)
  L <- suppressWarnings(t(chol(prior$Psi, pivot = TRUE)))

  ### s ~ D(delta)

  ### b_c ~ MVN(xi,D)

  ### Omega_c ~ IW(nu,Theta)

  ### Sigma ~ IW(kappa,E)

}

### compute Bayes factor
bayes_factor <- function(marg_like1, marg_like2) {
  marg_like1 / marg_like2
}

### compute posterior odds
posterior_odds <- function(prior_odds, bayes_factor) {
  prior_odds * bayes_factor
}

### the naive monte carlo estimator of the marginal likelihood
naive_monte_carlo_estimator <- function(N) {

  ### loop over samples
  like <- numeric(N)
  for(n in 1:N){

    ### draw sample from prior

    ### compute likelihood of data wrt prior
    like[n] <- NA

  }

  ### average likelihoods
  sum(like)/N
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
