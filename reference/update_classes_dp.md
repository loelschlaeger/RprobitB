# Dirichlet process class updates

Dirichlet process class updates

## Usage

``` r
update_classes_dp(
  beta,
  z,
  b,
  Omega,
  delta,
  mu_b_0,
  Sigma_b_0,
  n_Omega_0,
  V_Omega_0,
  identify_classes = FALSE,
  Cmax = 10L
)
```

## Arguments

- beta:

  \[`matrix(nrow = P_r, ncol = N)`\]  
  The matrix of the decider-specific coefficient vectors.

- z:

  \[`numeric(N)`\]  
  The decider class allocations.

- b:

  \[`matrix(nrow = P_r, ncol = C)`\]  
  The matrix of class means as columns.

- Omega:

  \[`matrix(nrow = P_r * P_r, ncol = C)`\]  
  The matrix of vectorized class covariance matrices as columns.

- delta:

  \[`numeric(1)`\]  
  The prior concentration for `s`.

- mu_b_0:

  \[`numeric(P_r)`\]  
  The mean vector of the normal prior for each `b_c`.

- Sigma_b_0:

  \[`matrix(P_r, P_r)`\]  
  The covariance matrix of the normal prior for each `b_c`.

- n_Omega_0:

  \[`integer(1)`\]  
  The degrees of freedom of the Inverse Wishart prior for each
  `Omega_c`.

- V_Omega_0:

  \[`matrix(P_r, P_r)`\]  
  The scale matrix of the Inverse Wishart prior for each `Omega_c`.

- identify_classes:

  \[`logical(1)`\]  
  Identify classes by decreasing class weights?

- Cmax:

  \[`integer(1)`\]  
  The maximum number of classes, used to allocate space.

## Value

A list of updated values for `z`, `b`, `Omega`, and `C`.

## Examples

``` r
set.seed(1)
z <- c(rep(1, 10),rep(2, 10))
b <- matrix(c(5, 5, 5, -5), ncol = 2)
Omega <- matrix(c(1, 0.3, 0.3, 0.5, 1, -0.3, -0.3, 0.8), ncol = 2)
beta <- sapply(
  z, function(z) oeli::rmvnorm(n = 1, b[, z], matrix(Omega[, z], 2, 2))
)
beta[, 1] <- c(-10, 10)
update_classes_dp(
  beta = beta, z = z, b = b, Omega = Omega,
  delta = 1, mu_b_0 = numeric(2), Sigma_b_0 = diag(2),
  n_Omega_0 = 4, V_Omega_0 = diag(2)
)
#> $z
#>       [,1]
#>  [1,]    3
#>  [2,]    1
#>  [3,]    1
#>  [4,]    1
#>  [5,]    1
#>  [6,]    1
#>  [7,]    1
#>  [8,]    1
#>  [9,]    1
#> [10,]    1
#> [11,]    2
#> [12,]    2
#> [13,]    2
#> [14,]    2
#> [15,]    2
#> [16,]    2
#> [17,]    2
#> [18,]    2
#> [19,]    2
#> [20,]    2
#> 
#> $b
#>          [,1]      [,2]      [,3]
#> [1,] 4.271368  4.411489 -9.010132
#> [2,] 4.705080 -4.386000  6.669987
#> 
#> $Omega
#>          [,1]       [,2]       [,3]
#> [1,] 2.318979  1.3721770  0.7757253
#> [2,] 1.007656 -0.4716889 -1.9847570
#> [3,] 1.007656 -0.4716889 -1.9847570
#> [4,] 1.079144  0.5840789  7.4273879
#> 
#> $C
#> [1] 3
#> 
```
