# Update class allocation vector

Update class allocation vector

## Usage

``` r
update_z(s, beta, b, Omega)
```

## Arguments

- s:

  \[`numeric(C)`\]  
  The vector of class weights.

- beta:

  \[`matrix(nrow = P_r, ncol = N)`\]  
  The matrix of the decider-specific coefficient vectors.

- b:

  \[`matrix(nrow = P_r, ncol = C)`\]  
  The matrix of class means as columns.

- Omega:

  \[`matrix(nrow = P_r * P_r, ncol = C)`\]  
  The matrix of vectorized class covariance matrices as columns.

## Value

An update for `z`.

## Examples

``` r
update_z(
  s = c(0.6, 0.4), beta = matrix(c(-2, 0, 2), ncol = 3),
  b = cbind(0, 1), Omega = cbind(1, 1)
)
#>      [,1]
#> [1,]    1
#> [2,]    1
#> [3,]    1
```
