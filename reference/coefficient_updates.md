# Update coefficient distributions

Low-level Gibbs sampler kernels for coefficient means and covariance
matrices.

## Usage

``` r
update_coefficient(mu_beta_0, Sigma_beta_0_inv, XSigX, XSigU)

update_b_c(bar_b_c, Omega_c, m_c, Sigma_b_0_inv, mu_b_0)

update_b(beta, Omega, z, m, Sigma_b_0_inv, mu_b_0)

update_Omega_c(S_c, m_c, n_Omega_0, V_Omega_0, correlated)

update_Omega(beta, b, z, m, n_Omega_0, V_Omega_0, correlated = NULL)
```

## Arguments

- mu_beta_0:

  \[`numeric(P)`\]  
  Prior mean for a coefficient vector.

- Sigma_beta_0_inv:

  \[`matrix(P, P)`\]  
  Prior precision matrix for a coefficient vector.

- XSigX:

  \[`matrix(P, P)`\]  
  Sum of design cross-products weighted by inverse error covariance.

- XSigU:

  \[`numeric(P)`\]  
  Sum of design-utility products weighted by inverse error covariance.

- bar_b_c:

  \[`numeric(P)`\]  
  Average individual coefficient vector in one class.

- Omega_c:

  \[`matrix(P, P)`\]  
  Covariance matrix of one class.

- m_c:

  \[`integer(1)`\]  
  Size of one class.

- Sigma_b_0_inv:

  \[`matrix(P, P)`\]  
  Prior precision matrix for class means.

- mu_b_0:

  \[`numeric(P)`\]  
  Prior mean for class means.

- beta:

  \[`matrix(P, N)`\]  
  Individual coefficient draws in columns.

- Omega:

  \[`matrix(P * P, C)`\]  
  Vectorized class covariance matrices in columns.

- z:

  \[`numeric(N)`\]  
  Class allocations numbered from one to `C`.

- m:

  \[`numeric(C)`\]  
  Class sizes.

- S_c:

  \[`matrix(P, P)`\]  
  Scatter matrix for one class.

- n_Omega_0:

  \[`integer(1)`\]  
  Prior degrees of freedom for class covariances.

- V_Omega_0:

  \[`matrix(P, P)`\]  
  Prior scale matrix for class covariances.

- correlated:

  \[`logical(P)` \| `NULL`\]  
  Which random effects are correlated. Covariances between the other
  effects are zero. By default (`NULL`), all random effects are
  correlated.

- b:

  \[`matrix(P, C)`\]  
  Class means in columns.

## Value

The functions return one sampler update:

- `update_b_c()`: a `P` by 1 numeric matrix containing a class mean.

- `update_b()`: a `P` by `C` matrix of class means.

- `update_Omega_c()`: a `P` by `P` class covariance matrix.

- `update_Omega()`: a `P * P` by `C` matrix of vectorized covariances.

- `update_coefficient()`: a `P` by 1 numeric coefficient matrix.

## Examples

``` r
### four deciders with one random coefficient in two classes
set.seed(1)
beta <- matrix(c(-1, -1.2, 1, 1.3), nrow = 1)
Omega <- matrix(c(0.2, 0.2), nrow = 1)
z <- c(1, 1, 2, 2)
m <- c(2, 2)

### a coefficient from its conditional posterior
update_coefficient(c(0, 0), diag(2), diag(2), c(0, 0))
#>            [,1]
#> [1,] -0.4429697
#> [2,]  0.1298554

### the class means, for one class and for all classes at once
update_b_c(
  bar_b_c = c(0, 0), Omega_c = diag(2), m_c = 4,
  Sigma_b_0_inv = diag(2), mu_b_0 = c(0, 0)
)
#>            [,1]
#> [1,] -0.3737045
#> [2,]  0.7134313
update_b(beta, Omega, z, m, Sigma_b_0_inv = diag(1), mu_b_0 = 0)
#>            [,1]     [,2]
#> [1,] -0.9006497 0.798074

### the class covariances
update_Omega_c(
  S_c = diag(2), m_c = 4, n_Omega_0 = 4, V_Omega_0 = diag(2),
  correlated = c(TRUE, TRUE)
)
#>             [,1]        [,2]
#> [1,]  0.23204571 -0.04360997
#> [2,] -0.04360997  0.22649560
update_Omega(
  beta, b = matrix(c(-1, 1), nrow = 1), z, m,
  n_Omega_0 = 4, V_Omega_0 = diag(1)
)
#>           [,1]      [,2]
#> [1,] 0.2548452 0.3903286
```
