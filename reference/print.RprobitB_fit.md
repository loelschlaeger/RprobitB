# Print a fitted choice model

Prints the model formula, data size, and retained posterior sample size.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
print(x, ...)
```

## Arguments

- x:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- ...:

  Currently not used.

## Value

`x`, invisibly.

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x, dgp_parameters = list(beta = c(x = 1, ASC_B = -0.5)),
  chains = 1
)
print(model)
#> Bayesian probit choice model
#> Formula: choice ~ x | 1 | 0 
#> Data: 100 deciders, 100 choice occasions
#> Samples: 500 retained per chain, 1 chain
```
