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

### example
data <- simulate_choices(form = y ~ x, N = 10, T = 10, J = 2, re = "x")
