# Extract estimated covariance matrix of mixing distribution

This helper function returns the estimated covariance matrix of the
mixing distribution.

## Usage

``` r
cov_mix(x, cor = FALSE)
```

## Arguments

- x:

  An object of class `RprobitB_fit`.

- cor:

  \[`integer(1)`\]  
  Return the correlation matrix instead?

## Value

The estimated covariance matrix of the mixing distribution. In case of
multiple classes, a list of matrices for each class.
