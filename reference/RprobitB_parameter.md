# Define probit model parameter

This function creates an object of class `RprobitB_parameter`, which
contains the parameters of a probit model.

If `sample = TRUE`, missing parameters are sampled. All parameters are
checked against the values of `P_f`, `P_r`, `J`, and `N`.

Note that parameters are automatically ordered with respect to a
non-ascending `s` for class identifiability.

## Usage

``` r
RprobitB_parameter(
  P_f,
  P_r,
  J,
  N,
  C = 1,
  ordered = FALSE,
  alpha = NULL,
  s = NULL,
  b = NULL,
  Omega = NULL,
  Sigma = NULL,
  Sigma_full = NULL,
  beta = NULL,
  z = NULL,
  d = NULL,
  sample = TRUE
)

# S3 method for class 'RprobitB_parameter'
print(x, ..., digits = 4)
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

- N:

  \[`integer(1)`\]  
  The number of decision makers.

- C:

  \[`integer(1)`\]  
  The number (greater or equal 1) of latent classes of decision makers.

- ordered:

  \[`logical(1)`\]  
  If `TRUE`, the choice set `alternatives` is assumed to be ordered from
  worst to best.

- alpha:

  \[`numeric(P_f)`\]  
  The fixed coefficient vector.

- s:

  \[`numeric(C)`\]  
  The vector of class weights.

- b:

  \[`matrix(nrow = P_r, ncol = C)`\]  
  The matrix of class means as columns.

- Omega:

  \[`matrix(nrow = P_r * P_r, ncol = C)`\]  
  The matrix of vectorized class covariance matrices as columns.

- Sigma:

  \[`matrix(nrow = J - 1, ncol = J - 1)` \| `numeric(1)`\]  
  The differenced (wrt. alternative `J`) error covariance matrix.

  In case of `ordered = TRUE`, the single error variance.

- Sigma_full:

  \[`matrix(nrow = J, ncol = J)`\]  
  The error covariance matrix.

  Ignored if `Sigma` is specified or `ordered = TRUE`.

  Internally, `Sigma_full` gets differenced wrt. alternative `J`.

- beta:

  \[`matrix(nrow = P_r, ncol = N)`\]  
  The matrix of the decider-specific coefficient vectors.

- z:

  \[`numeric(N)`\]  
  The decider class allocations.

- d:

  \[`numeric(J - 2)`\]  
  The logarithmic increases of the utility thresholds in the ordered
  probit case (`ordered = TRUE`).

- sample:

  \[`logical(1)`\]  
  Sample missing parameters?

- x:

  An `RprobitB_parameter` object.

- ...:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Names of parameters to be printed. If not specified, all parameters
  are printed.

- digits:

  \[`integer(1)`\]  
  The number of decimal places.

## Value

An object of class `RprobitB_parameter`, which is a named list with the
model parameters.

## Examples

``` r
RprobitB_parameter(P_f = 1, P_r = 2, J = 3, N = 10, C = 2)
#> alpha : -2.5
#> 
#> C : 2
#> 
#> s : double vector of length 2 
#> 0.94 0.06
#> 
#> b : 2 x 2 matrix of doubles 
#>      [,1] [,2]
#> [1,]  1.7 -0.1
#> [2,] -1.9  0.7
#> 
#> 
#> Omega : 4 x 2 matrix of doubles 
#>       [,1] [,2]
#> [1,]   2.2 1.43
#> [2,] -1.42 0.17
#> [3,] -1.42 0.17
#> [4,]  2.99 1.15
#> 
#> 
#> Sigma : 2 x 2 matrix of doubles 
#>      [,1] [,2]
#> [1,] 2.73 1.35
#> [2,] 1.35 2.31
#> 
#> 
#> Sigma_full : 3 x 3 matrix of doubles 
#>       [,1] [,2]  [,3]
#> [1,]  1.29 0.04 -0.12
#> [2,]  0.04 1.11     0
#> [3,] -0.12    0  1.19
#> 
#> 
#> beta : 2 x 10 matrix of doubles 
#>       [,1]  [,2]  [,3] ... [,10]
#> [1,]  2.67   1.5  1.07 ...   2.7
#> [2,] -3.26 -1.44 -3.15 ... -0.97
#> 
#> 
#> z : double vector of length 10 
#> 1 1 1 ... 1
#> 
#> d : NA
#> 
```
