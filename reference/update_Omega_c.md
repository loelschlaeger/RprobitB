# Update covariance of a single class

Update covariance of a single class

## Usage

``` r
update_Omega_c(S_c, m_c, n_Omega_0, V_Omega_0)
```

## Arguments

- S_c:

  \[`matrix(P_r, P_r)`\]  
  The scatter matrix of this class.

- m_c:

  \[`integer(1)`\]  
  The number of observations in this class.

- n_Omega_0:

  \[`integer(1)`\]  
  The degrees of freedom of the Inverse Wishart prior for each
  `Omega_c`.

- V_Omega_0:

  \[`matrix(P_r, P_r)`\]  
  The scale matrix of the Inverse Wishart prior for each `Omega_c`.

## Value

An update for `Omega_c`.

## Examples

``` r
update_Omega_c(S_c = diag(2), m_c = 10, n_Omega_0 = 4, V_Omega_0 = diag(2))
#>            [,1]       [,2]
#> [1,] 0.14877089 0.01266135
#> [2,] 0.01266135 0.14200016
```
