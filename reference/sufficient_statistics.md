# Compute sufficient statistics

This function computes sufficient statistics from an `RprobitB_data`
object for the Gibbs sampler to save computation time.

## Usage

``` r
sufficient_statistics(data, normalization)
```

## Arguments

- data:

  An object of class `RprobitB_data`.

- normalization:

  An object of class `RprobitB_normalization`, which can be created via
  [`RprobitB_normalization`](https://loelschlaeger.de/RprobitB/reference/RprobitB_normalization.md).

## Value

A list of sufficient statistics on the data for Gibbs sampling,
containing

- the elements `N`, `T`, `J`, `P_f` and `P_r` from `data`,

- `Tvec`, the vector of choice occasions for each decider of length `N`,

- `csTvec`, a vector of length `N` with the cumulated sums of `Tvec`
  starting from `0`,

- `W`, a list of design matrices differenced with respect to alternative
  number `normalization$level$level` for each decider in each choice
  occasion with covariates that are linked to a fixed coefficient (or
  `NA` if `P_f = 0`),

- `X`, a list of design matrices differenced with respect to alternative
  number `normalization$level$level` for each decider in each choice
  occasion with covariates that are linked to a random coefficient (or
  `NA` if `P_r = 0`),

- `y`, a matrix of dimension `N` x `max(Tvec)` with the observed choices
  of deciders in rows and choice occasions in columns, decoded to
  numeric values with respect to their appearance in
  `data$alternatives`, where rows are filled with `NA` in case of an
  unbalanced panel,

- `WkW`, a matrix of dimension `P_f^2` x `(J-1)^2`, the sum over
  Kronecker products of each transposed element in `W` with itself,

- `XkX`, a list of length `N`, each element is constructed in the same
  way as `WkW` but with the elements in `X` and separately for each
  decider,

- `rdiff` (for the ranked case only), a list of matrices that reverse
  the base differencing and instead difference in such a way that the
  resulting utility vector is negative.
