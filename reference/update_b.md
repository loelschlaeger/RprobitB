# Update class means

Update class means

## Usage

``` r
update_b(beta, Omega, z, m, Sigma_b_0_inv, mu_b_0)
```

## Arguments

- beta:

  \[`matrix(nrow = P_r, ncol = N)`\]  
  The matrix of the decider-specific coefficient vectors.

- Omega:

  \[`matrix(nrow = P_r * P_r, ncol = C)`\]  
  The matrix of vectorized class covariance matrices as columns.

- z:

  \[`numeric(N)`\]  
  The decider class allocations.

- m:

  \[`numeric(C)`\]  
  The vector of current class frequencies.

- Sigma_b_0_inv:

  \[`matrix(P_r, P_r)`\]  
  The prior precision of the class mean.

- mu_b_0:

  \[`numeric(P_r)`\]  
  The mean vector of the normal prior for each `b_c`.

## Value

A matrix of updated means for each class in columns.

## Examples

``` r
N <- 100
b <- cbind(c(0, 0), c(1, 1))
Omega <- matrix(c(1, 0.3, 0.3, 0.5, 1, -0.3, -0.3, 0.8), ncol = 2)
z <- c(rep(1, N / 2), rep(2, N / 2))
m <- as.numeric(table(z))
beta <- sapply(
  z, function(z) oeli::rmvnorm(n = 1, b[, z], matrix(Omega[, z], 2, 2))
)
update_b(
  beta = beta, Omega = Omega, z = z, m = m,
  Sigma_b_0_inv = diag(2), mu_b_0 = c(0, 0)
)
#>              [,1]     [,2]
#> [1,] -0.003679256 1.074241
#> [2,] -0.035150893 1.253953
```
