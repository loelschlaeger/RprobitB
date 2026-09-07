# Check prior parameters

This function checks the compatibility of submitted parameters for the
prior distributions and sets missing values to default values.

## Usage

``` r
check_prior(
  P_f,
  P_r,
  J,
  ordered = FALSE,
  mu_alpha_0 = numeric(P_f),
  Sigma_alpha_0 = 10 * diag(P_f),
  delta = 1,
  mu_b_0 = numeric(P_r),
  Sigma_b_0 = 10 * diag(P_r),
  n_Omega_0 = P_r + 2,
  V_Omega_0 = diag(P_r),
  n_Sigma_0 = J + 1,
  V_Sigma_0 = diag(J - 1),
  mu_d_0 = numeric(J - 2),
  Sigma_d_0 = diag(J - 2)
)
```

## Arguments

- P_f:

  \[`integer(1)`\]  
  The number of covariates connected to a fixed coefficient.

- P_r:

  \[`integer(2)`\]  
  The number of covariates connected to a random coefficient.

- J:

  \[`integer(1)`\]  
  The number \>= 2 of choice alternatives.

- ordered:

  \[`logical(1)`\]  
  If `TRUE`, the choice set `alternatives` is assumed to be ordered from
  worst to best.

- mu_alpha_0:

  \[`numeric(P_f)`\]  
  The mean vector of the normal prior for `alpha`.

- Sigma_alpha_0:

  \[`matrix(P_f, P_f)`\]  
  The covariance matrix of the normal prior for `alpha`.

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

- n_Sigma_0:

  \[`integer(1)`\]  
  The degrees of freedom of the Inverse Wishart prior for `Sigma`.

- V_Sigma_0:

  \[`matrix(J - 1, J - 1)`\]  
  The scale matrix of the Inverse Wishart prior for `Sigma`.

- mu_d_0:

  \[`numeric(J - 2)`\]  
  The mean vector of the normal prior for `d` .

- Sigma_d_0:

  \[`matrix(J - 2, J - 2)`\]  
  The covariance matrix of the normal prior for `d`.

## Value

An object of class `RprobitB_prior`, which is a `list` containing all
prior parameters.

## Details

A priori-distributions:

- \\\alpha \sim N(\mu\_{\alpha_0}, \Sigma\_{\alpha_0})\\

- \\s \sim Dir(\delta)\\

- \\b_c \sim N(\mu\_{b_0}, \Sigma\_{b_0})\\ for all \\c\\

- \\\Omega_c \sim IW(n\_{\Omega_0}, V\_{\Omega_0})\\ for all \\c\\

- \\\Sigma \sim IW(n\_{\Sigma_0}, V\_{\Sigma_0})\\

- \\d \sim N(\mu\_{d_0}, \Sigma\_{d_0})\\

## Examples

``` r
check_prior(P_f = 1, P_r = 2, J = 3, ordered = TRUE)
#> $mu_alpha_0
#> [1] 0
#> 
#> $Sigma_alpha_0
#>      [,1]
#> [1,]   10
#> 
#> $delta
#> [1] 1
#> 
#> $mu_b_0
#> [1] 0 0
#> 
#> $Sigma_b_0
#>      [,1] [,2]
#> [1,]   10    0
#> [2,]    0   10
#> 
#> $n_Omega_0
#> [1] 4
#> 
#> $V_Omega_0
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    1
#> 
#> $n_Sigma_0
#> [1] NA
#> 
#> $V_Sigma_0
#> [1] NA
#> 
#> $mu_d_0
#> [1] 0
#> 
#> $Sigma_d_0
#>      [,1]
#> [1,]    1
#> 
#> attr(,"class")
#> [1] "RprobitB_prior" "list"          
```
