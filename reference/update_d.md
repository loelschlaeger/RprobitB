# Update utility threshold increments

Update utility threshold increments

## Usage

``` r
update_d(d, y, sys, ll, mu_d_0, Sigma_d_0, Tvec, step_scale = 0.1)
```

## Arguments

- d:

  \[`numeric(J - 2)`\]  
  Threshold increments.

- y:

  \[`matrix(nrow = N, ncol = max(Tvec))`\]  
  Choices `1,...,J` for each decider in each choice occasion.

- sys:

  \[`matrix(nrow = N, ncol = max(Tvec))`\]  
  Systematic utilties for each decider in each choice occasion.

- ll:

  \[`numeric(1)`\]  
  Current log-likelihood value.

- mu_d_0:

  \[`numeric(J - 2)`\]  
  The mean vector of the normal prior for `d` .

- Sigma_d_0:

  \[`matrix(J - 2, J - 2)`\]  
  The covariance matrix of the normal prior for `d`.

- Tvec:

  \[`integer(N)`\]  
  Number of choice occasions per decider.

- step_scale:

  \[`numeric(1)`\]  
  Scaling the variance for the Gaussian proposal.

## Value

An update for `d`.

## Examples

``` r
set.seed(1)
N <- 1000
d_true <- rnorm(2)
gamma <- c(-Inf, 0, cumsum(exp(d_true)), Inf)
X <- matrix(rnorm(N * 2L), ncol = 2L)
beta <- c(0.8, -0.5)
mu <- matrix(as.vector(X %*% beta), ncol = 1L)
U <- rnorm(N, mean = mu[, 1], sd = 1)
yvec <- as.integer(cut(U, breaks = gamma, labels = FALSE))
y <- matrix(yvec, ncol = 1L)
Tvec <- rep(1, N)
mu_d_0 <- c(0, 0)
Sigma_d_0 <- diag(2) * 5
d <- rnorm(2)
ll <- -Inf
R <- 1000
for (iter in seq_len(R)) {
  ans <- update_d(
    d = d, y = y, sys = mu, ll = ll, mu_d_0 = mu_d_0, Sigma_d_0 = Sigma_d_0,
    Tvec = Tvec
  )
  d  <- ans$d
  ll <- ans$ll
}
cbind("true" = d_true, "sample" = d) |> round(2)
#>       true      
#> [1,] -0.63 -0.83
#> [2,]  0.18  0.17
```
