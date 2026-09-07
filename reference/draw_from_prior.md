# Sample from prior distributions

This function returns a sample from each parameter's prior distribution.

## Usage

``` r
draw_from_prior(prior, C = 1)
```

## Arguments

- prior:

  An object of class `RprobitB_prior`, which is the output of
  [`check_prior`](https://loelschlaeger.de/RprobitB/reference/check_prior.md).

- C:

  The number of latent classes.

## Value

A list of draws for `alpha`, `s`, `b`, `Omega`, and `Sigma` (if
specified for the model).
