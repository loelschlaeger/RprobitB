# Update ranked utility vector

Update ranked utility vector

## Usage

``` r
update_U_ranked(U, sys, Sigma_inv)
```

## Arguments

- U:

  \[`numeric(J - 1)`\]  
  The current utility vector.

- sys:

  \[`numeric(J - 1)`\]  
  The systematic utility.

- Sigma_inv:

  \[`matrix(J - 1, J - 1)`\]  
  The inverted error covariance matrix.

## Value

An update for (a single) ranked `U`.

## Examples

``` r
U <- sys <- c(0, 0)
Sigma_inv <- diag(2)
update_U_ranked(U, sys, Sigma_inv)
#>           [,1]
#> [1,] -1.821830
#> [2,] -0.589543
```
