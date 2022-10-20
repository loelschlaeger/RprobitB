# binary probit: U = b1 + b2*X + eps, eps ~ N(0,sigma)
b <- c(1, -1)
sigma <- 1
X <- rnorm(100)
U <- sapply(X, function(x) b[1] + x*b[2] + rnorm(1, sd = sigma))

ll <- function(theta, sigma, X, U){
  sys <- sapply(X, function(x) theta[1] + x*theta[2])
  suppressWarnings(sum(dnorm(U, mean = sys, sd = sigma, log = TRUE)))
}

log_prior <- function(theta){
  if (sum(theta) > 0) return(-Inf)
  b1_prior <- dunif(theta[1], min = -2, max = 2, log = TRUE)
  b2_prior <- dnorm(theta[2], mean = 0, sd = 2, log = TRUE)
  b1_prior + b2_prior
}

log_posterior <- function(theta, sigma, X, U) {
  ll(theta, sigma, X, U) + log_prior(theta)
}

proposal <- function(theta) {
  rnorm(2, mean = theta, sd = 2)
}

metropolis <- function(init, R, B = R/2, sigma, X, U) {
  chain <- array(dim = c(R-B, 2))
  theta <- init
  acceptance <- numeric(R)
  for (r in 1:R) {
    p <- proposal(theta)
    prob_ratio <- exp(log_posterior(p, sigma, X, U) - log_posterior(theta, sigma, X, U))
    if (is.nan(prob_ratio)) prob_ratio <- 0
    if (runif(1) < prob_ratio) {
      theta <- p
      acceptance[r] <- 1
    }
    if (r > B) {
      chain[r - B,] <- theta
    }
  }
  structure(chain, "arate" = sum(acceptance[-(1:B)])/(R-B))
}

chain <- metropolis(init = c(0,0), R = 5000, sigma = sigma, X = X, U = U)
colMeans(chain)
attr(chain, "arate")
plot(chain[,1], type = "l")



