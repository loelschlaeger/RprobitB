# Update utilities and thresholds

Low-level Gibbs sampler kernels for error covariance matrices, latent
utilities, and ordered-response thresholds.

## Usage

``` r
d_to_gamma(d)

log_likelihood_ordered(d, y, sys, Tvec)

update_Sigma(n_Sigma_0, V_Sigma_0, N, S)

update_U(U, y, sys, Sigma_inv, available = NULL)

update_U_ranked(U, sys, Sigma_inv)

update_d(d, y, sys, mu_d_0, Sigma_d_0, Tvec, step_scale)
```

## Arguments

- d:

  \[`numeric(J - 2)`\]  
  Log-increments between finite ordered thresholds.

- y:

  \[`integer(1)` \| `matrix(N, max(Tvec))`\]  
  Chosen alternative for `update_U()`, where `J` denotes the base
  alternative, or ordered responses by decider and occasion for the
  threshold functions.

- sys:

  \[`numeric(J - 1)` \| `matrix(N, max(Tvec))`\]  
  Systematic utilities matching `U` or `y`.

- Tvec:

  \[`integer(N)`\]  
  Number of observed occasions for each decider.

- n_Sigma_0:

  \[`integer(1)`\]  
  Prior degrees of freedom for the error covariance.

- V_Sigma_0:

  \[`matrix(J - 1, J - 1)`\]  
  Prior scale matrix for the error covariance.

- N:

  \[`integer(1)`\]  
  Number of independent sampling units.

- S:

  \[`matrix(J - 1, J - 1)`\]  
  Error scatter matrix.

- U:

  \[`numeric(J - 1)`\]  
  Current latent utility differences.

- Sigma_inv:

  \[`matrix(J - 1, J - 1)`\]  
  Inverse error covariance matrix.

- available:

  \[`logical(J)` \| `NULL`\]  
  Availability of the `J - 1` non-base alternatives followed by the base
  alternative. Utilities of unavailable alternatives are drawn without
  truncation. By default (`NULL`), all alternatives are available.

- mu_d_0:

  \[`numeric(J - 2)`\]  
  Prior mean for threshold log-increments.

- Sigma_d_0:

  \[`matrix(J - 2, J - 2)`\]  
  Prior covariance for threshold log-increments.

- step_scale:

  \[`numeric(J - 2)`\]  
  Random-walk proposal standard deviations, one per log-increment.

## Value

The functions return one sampler update or transformation:

- `update_Sigma()`: a `J - 1` by `J - 1` covariance matrix.

- `update_U()` and `update_U_ranked()`: `J - 1` by 1 numeric latent
  utility matrices.

- `d_to_gamma()`: a numeric column matrix containing ordered thresholds
  and their infinite bounds.

- `log_likelihood_ordered()`: one numeric log-likelihood value.

- `update_d()`: a numeric vector of updated log-increments.

## References

Robert CP (1995). “Simulation of Truncated Normal Variables.”
*Statistics and Computing*, **5**(2), 121–125.
[doi:10.1007/BF00143942](https://doi.org/10.1007/BF00143942) .

## Examples

``` r
### two deciders who choose twice on an ordered scale of four levels
set.seed(1)
d <- c(0, log(2))
y <- matrix(c(1, 2, 3, 2), nrow = 2)
sys <- matrix(0, nrow = 2, ncol = 2)
Tvec <- c(2, 2)

### the thresholds, their likelihood, and their random-walk update
d_to_gamma(d)
#>      [,1]
#> [1,] -Inf
#> [2,]    0
#> [3,]    1
#> [4,]    3
#> [5,]  Inf
log_likelihood_ordered(d, y, sys, Tvec)
#> [1] -4.692438
update_d(
  d, y, sys, mu_d_0 = c(0, 0), Sigma_d_0 = diag(2), Tvec = Tvec,
  step_scale = c(0.1, 0.1)
)
#>             [,1]
#> [1,] -0.06264538
#> [2,]  0.82612711

### the latent utilities of an unordered and of a ranked choice
update_U(
  c(0, 0), y = 1, sys = c(0, 0), Sigma_inv = diag(2),
  available = c(TRUE, TRUE, TRUE)
)
#>            [,1]
#> [1,]  0.4634427
#> [2,] -0.4874291
update_U_ranked(c(0, 0), sys = c(0, 0), Sigma_inv = diag(2))
#>            [,1]
#> [1,] -0.2615707
#> [2,] -0.9674915

### the error covariance
update_Sigma(n_Sigma_0 = 4, V_Sigma_0 = diag(2), N = 10, S = diag(2))
#>            [,1]       [,2]
#> [1,] 0.09405327 0.01900477
#> [2,] 0.01900477 0.14300195
```
