# Update class covariances

Update class covariances

## Usage

``` r
update_Omega(beta, b, z, m, n_Omega_0, V_Omega_0)
```

## Arguments

- beta:

  \[`matrix(nrow = P_r, ncol = N)`\]  
  The matrix of the decider-specific coefficient vectors.

- b:

  \[`matrix(nrow = P_r, ncol = C)`\]  
  The matrix of class means as columns.

- z:

  \[`numeric(N)`\]  
  The decider class allocations.

- m:

  \[`numeric(C)`\]  
  The vector of current class frequencies.

- n_Omega_0:

  \[`integer(1)`\]  
  The degrees of freedom of the Inverse Wishart prior for each
  `Omega_c`.

- V_Omega_0:

  \[`matrix(P_r, P_r)`\]  
  The scale matrix of the Inverse Wishart prior for each `Omega_c`.

## Value

A matrix of updated covariance matrices for each class in columns.

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
update_Omega(
  beta = beta, b = b, z = z, m = m,
  n_Omega_0 = 4, V_Omega_0 = diag(2)
)
#>            [,1]        [,2]
#> [1,] 0.86030753  0.52976022
#> [2,] 0.04106396 -0.07224133
#> [3,] 0.04106396 -0.07224133
#> [4,] 0.40405566  0.59672697
```
