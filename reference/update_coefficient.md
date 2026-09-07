# Update coefficient vector

Update coefficient vector

## Usage

``` r
update_coefficient(mu_beta_0, Sigma_beta_0_inv, XSigX, XSigU)
```

## Arguments

- mu_beta_0:

  \[`numeric(P)`\]  
  The prior mean for the coefficient vector,

- Sigma_beta_0_inv:

  \[`matrix(P, P)`\]  
  The prior precision for the coefficient vector.

- XSigX:

  \[`matrix(P, P)`\]  
  The matrix \\\sum\_{n=1}^N X_n'\Sigma^{-1}X_n\\.

- XSigU:

  \[`numeric(P)`\]  
  The vector \\\sum\_{n=1}^N X_n'\Sigma^{-1}U_n\\.

## Value

An update for the coefficient vector.

## Examples

``` r
beta_true <- matrix(c(-1, 1), ncol = 1)
Sigma <- matrix(c(1, 0.5, 0.2, 0.5, 1, 0.2, 0.2, 0.2, 2), ncol = 3)
N <- 100
X <- replicate(N, matrix(rnorm(6), ncol = 2), simplify = FALSE)
eps <- replicate(
  N, oeli::rmvnorm(n = 1, mean = c(0, 0, 0), Sigma = Sigma),
  simplify = FALSE
)
U <- mapply(
  function(X, eps) X %*% beta_true + eps, X, eps, SIMPLIFY = FALSE
)
mu_beta_0 <- c(0, 0)
Sigma_beta_0_inv <- diag(2)
XSigX <- Reduce(
  `+`, lapply(X, function(X) t(X) %*% solve(Sigma) %*% X)
)
XSigU <- Reduce(
  `+`, mapply(function(X, U) t(X) %*% solve(Sigma) %*% U, X, U,
  SIMPLIFY = FALSE)
)
R <- 10
beta_draws <- replicate(
  R, update_coefficient(mu_beta_0, Sigma_beta_0_inv, XSigX, XSigU),
  simplify = TRUE
)
rowMeans(beta_draws)
#> [1] -0.9654761  1.0150600
```
