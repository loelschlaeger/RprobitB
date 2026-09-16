# Extract the posterior covariance matrix

Computes covariance across all retained draws of global model variables.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
vcov(object, ...)
```

## Arguments

- object:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- ...:

  Currently not used.

## Value

A symmetric numeric matrix whose rows and columns are the global
posterior variables returned by
[`coef()`](https://rdrr.io/r/stats/coef.html).

## Details

The returned matrix is the covariance of the posterior distribution, not
the sampling covariance of an estimator. It is reported through
[`stats::vcov()`](https://rdrr.io/r/stats/vcov.html) because it is the
standard way to ask a fitted model for the covariance of its parameters.

## Examples

``` r
set.seed(1)
model <- fit(choice ~ x + y | 0, chains = 1)
vcov(model)
#>          beta[x]   beta[y]
#> beta[x] 0.338998 0.5167580
#> beta[y] 0.516758 0.9846016
```
