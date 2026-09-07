# Update utility vector

Update utility vector

## Usage

``` r
update_U(U, y, sys, Sigma_inv)
```

## Arguments

- U:

  \[`numeric(J - 1)`\]  
  The current utility vector.

- y:

  \[`integer(1)`\]  
  The index of the chosen alternative, from `1` to `J`.

- sys:

  \[`numeric(J - 1)`\]  
  The systematic utility.

- Sigma_inv:

  \[`matrix(J - 1, J - 1)`\]  
  The inverted error covariance matrix.

## Value

An update for (a single) `U`.

## Examples

``` r
U <- sys <- c(0, 0, 0)
Sigma_inv <- diag(3)
lapply(1:4, function(y) update_U(U, y, sys, Sigma_inv))
#> [[1]]
#>           [,1]
#> [1,]  1.206465
#> [2,] -1.203873
#> [3,] -1.895631
#> 
#> [[2]]
#>            [,1]
#> [1,] -2.0203864
#> [2,]  1.1755390
#> [3,]  0.8345605
#> 
#> [[3]]
#>            [,1]
#> [1,] -1.6960662
#> [2,] -1.6614639
#> [3,]  0.1717138
#> 
#> [[4]]
#>            [,1]
#> [1,] -1.0096126
#> [2,] -0.1136272
#> [3,] -0.4423781
#> 
```
