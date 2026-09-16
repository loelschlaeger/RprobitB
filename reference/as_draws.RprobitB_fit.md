# Convert a fitted model to posterior draws

Returns the canonical retained posterior draws.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
as_draws(x, ...)
```

## Arguments

- x:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- ...:

  Currently not used.

## Value

A `draws_array` with dimensions iteration, chain, and variable.

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), chains = 1
)
posterior::as_draws(model)
#> # A draws_array: 500 iterations, 1 chains, and 2 variables
#> , , variable = beta[x]
#> 
#>          chain
#> iteration   1
#>         1 1.7
#>         2 1.6
#>         3 1.6
#>         4 1.6
#>         5 1.7
#> 
#> , , variable = Sigma[B,B]
#> 
#>          chain
#> iteration 1
#>         1 1
#>         2 1
#>         3 1
#>         4 1
#>         5 1
#> 
#> # ... with 495 more iterations
```
