# Update error covariance matrix

Update error covariance matrix

## Usage

``` r
update_Sigma(n_Sigma_0, V_Sigma_0, N, S)
```

## Arguments

- n_Sigma_0:

  \[`integer(1)`\]  
  The degrees of freedom of the Inverse Wishart prior for `Sigma`.

- V_Sigma_0:

  \[`matrix(J - 1, J - 1)`\]  
  The scale matrix of the Inverse Wishart prior for `Sigma`.

- N:

  \[`integer(1)`\]  
  The sample size.

- S:

  \[`matrix(J - 1, J - 1)`\]  
  The sum over the outer products of the residuals \\(\epsilon_n)\_{n =
  1, \dots, N}\\.

## Value

An update for `Sigma`.

## Examples

``` r
(Sigma_true <- matrix(c(1, 0.5, 0.2, 0.5, 1, 0.2, 0.2, 0.2, 2), ncol = 3))
#>      [,1] [,2] [,3]
#> [1,]  1.0  0.5  0.2
#> [2,]  0.5  1.0  0.2
#> [3,]  0.2  0.2  2.0
beta <- matrix(c(-1, 1), ncol = 1)
N <- 100
X <- replicate(N, matrix(rnorm(6), ncol = 2), simplify = FALSE)
eps <- replicate(
  N, oeli::rmvnorm(n = 1, mean = c(0, 0, 0), Sigma = Sigma_true),
  simplify = FALSE
)
U <- mapply(function(X, eps) X %*% beta + eps, X, eps, SIMPLIFY = FALSE)
n_Sigma_0 <- 4
V_Sigma_0 <- diag(3)
outer_prod <- function(X, U) (U - X %*% beta) %*% t(U - X %*% beta)
S <- Reduce(`+`, mapply(
  function(X, U) (U - X %*% beta) %*% t(U - X %*% beta), X, U,
  SIMPLIFY = FALSE
))
Sigma_draws <- replicate(100, update_Sigma(n_Sigma_0, V_Sigma_0, N, S))
apply(Sigma_draws, 1:2, mean)
#>           [,1]      [,2]      [,3]
#> [1,] 1.0324089 0.4835025 0.2186895
#> [2,] 0.4835025 1.2353293 0.1948379
#> [3,] 0.2186895 0.1948379 1.7397380
```
