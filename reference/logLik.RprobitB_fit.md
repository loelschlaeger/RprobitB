# Extract the fitted log-likelihood

Evaluates the decider-level log-likelihood at the posterior mean
parameters.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
logLik(object, ghk_draws = 500L, ...)
```

## Arguments

- object:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- ghk_draws:

  \[`integer(1)`\]  
  Number of draws of the GHK simulator for multivariate normal
  probabilities of more than three dimensions, see
  [`oeli::pmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md).

- ...:

  Currently not used.

## Value

A scalar object of class `logLik`. The `df` attribute counts the free
population-level parameters after normalization, and `nobs` is the
number of independent likelihood units as counted by
[`nobs()`](https://rdrr.io/r/stats/nobs.html).

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), chains = 1
)
logLik(model)
#> 'log Lik.' -36.04087 (df=1)
```
