# Filter Gibbs samples

This is a helper function that filters Gibbs samples.

## Usage

``` r
filter_gibbs_samples(
  x,
  P_f,
  P_r,
  J,
  C,
  cov_sym,
  ordered = FALSE,
  keep_par = c("s", "alpha", "b", "Omega", "Sigma", "d"),
  drop_par = NULL
)
```

## Arguments

- x:

  An object of class `RprobitB_gibbs_samples`.

- P_f:

  \[`integer(1)`\]  
  The number of covariates connected to a fixed coefficient.

- P_r:

  \[`integer(2)`\]  
  The number of covariates connected to a random coefficient.

- J:

  \[`integer(1)`\]  
  The number \>= 2 of choice alternatives.

- cov_sym:

  Set to `TRUE` for labels of symmetric covariance elements.

- ordered:

  \[`logical(1)`\]  
  If `TRUE`, the choice set `alternatives` is assumed to be ordered from
  worst to best.

- keep_par, drop_par:

  A vector of parameter names which are kept or dropped.

## Value

An object of class `RprobitB_gibbs_samples` filtered by the labels of
[`parameter_labels`](https://loelschlaeger.de/RprobitB/reference/parameter_labels.md).
