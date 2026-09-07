# Gibbs sample mode

This function approximates the Gibbs sample mode.

## Usage

``` r
mode_approx(samples)
```

## Arguments

- samples:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Gibbs samples.

## Value

The (approximated) mode.

## Examples

``` r
samples <- oeli::rmixnorm(
  n = 1000, mean = matrix(c(-2, 2), ncol = 2),
  Sigma = matrix(c(1, 1), ncol = 2), proportions = c(0.7, 0.3)
)
hist(samples)

mean(samples) # expected: 0.7 * (-2) + 0.3 * 2 = -0.8
#> [1] -0.7623021
mode_approx(samples) # expected: -2
#> [1] -2.100096
```
