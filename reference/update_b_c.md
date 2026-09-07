# Update mean of a single class

Update mean of a single class

## Usage

``` r
update_b_c(bar_b_c, Omega_c, m_c, Sigma_b_0_inv, mu_b_0)
```

## Arguments

- bar_b_c:

  \[`numeric(P_r)`\]  
  The average observation of this class.

- Omega_c:

  \[`matrix(P_r, P_r)`\]  
  The class covariance matrix.

- m_c:

  \[`integer(1)`\]  
  The number of observations in this class.

- Sigma_b_0_inv:

  \[`matrix(P_r, P_r)`\]  
  The prior precision of the class mean.

- mu_b_0:

  \[`numeric(P_r)`\]  
  The mean vector of the normal prior for each `b_c`.

## Value

An update for `b_c`.

## Examples

``` r
update_b_c(
  bar_b_c = c(0, 0), Omega_c = diag(2), m_c = 10,
  Sigma_b_0_inv = diag(2), mu_b_0 = c(0, 0)
)
#>            [,1]
#> [1,] -0.2850403
#> [2,] -0.3408788
```
