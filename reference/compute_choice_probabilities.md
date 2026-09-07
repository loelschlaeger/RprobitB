# Compute probit choice probabilities

This is a helper function for
[`choice_probabilities`](https://loelschlaeger.de/RprobitB/reference/choice_probabilities.md)
and computes the probit choice probabilities for a single choice
situation with `J` alternatives.

## Usage

``` r
compute_choice_probabilities(X, alternatives, parameter, ordered = FALSE)
```

## Arguments

- X:

  A matrix of covariates with `J` rows and `P_f + P_r` columns, where
  the first `P_f` columns are connected to fixed coefficients and the
  last `P_r` columns are connected to random coefficients.

- alternatives:

  A vector with unique integers from `1` to `J`, indicating the
  alternatives for which choice probabilities are to be computed.

- parameter:

  An object of class `RprobitB_parameter`.

- ordered:

  \[`logical(1)`\]  
  If `TRUE`, the choice set `alternatives` is assumed to be ordered from
  worst to best.

## Value

A probability vector of length `length(alternatives)`.
