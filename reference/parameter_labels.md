# Create parameters labels

This function creates model parameter labels.

## Usage

``` r
parameter_labels(
  P_f,
  P_r,
  J,
  C,
  cov_sym,
  ordered = FALSE,
  keep_par = c("s", "alpha", "b", "Omega", "Sigma", "d"),
  drop_par = NULL
)

create_labels_s(P_r, C)

create_labels_alpha(P_f)

create_labels_b(P_r, C)

create_labels_Omega(P_r, C, cov_sym)

create_labels_Sigma(J, cov_sym, ordered = FALSE)

create_labels_d(J, ordered)
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

- cov_sym:

  Set to `TRUE` for labels of symmetric covariance elements.

- ordered:

  \[`logical(1)`\]  
  If `TRUE`, the choice set `alternatives` is assumed to be ordered from
  worst to best.

- keep_par, drop_par:

  A vector of parameter names which are kept or dropped.

## Value

A list of labels for the selected model parameters.
