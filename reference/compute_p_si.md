# Compute choice probabilities at posterior samples

This function computes the probability for each observed choice at the
(normalized, burned and thinned) samples from the posterior.

These probabilities are required to compute the
[`WAIC`](https://loelschlaeger.de/RprobitB/reference/WAIC.md) and the
marginal model likelihood
[`mml`](https://loelschlaeger.de/RprobitB/reference/mml.md).

## Usage

``` r
compute_p_si(x, ncores = parallel::detectCores() - 1, recompute = FALSE)
```

## Arguments

- x:

  An object of class `RprobitB_fit`.

- ncores:

  \[`integer(1)`\]  
  The number of cores for parallel computation.

  If set to 1, no parallel backend is used.

- recompute:

  \[`logical(1)`\]  
  Recompute the probabilities?

## Value

The object `x`, including the object `p_si`, which is a matrix of
probabilities, observations in rows and posterior samples in columns.
