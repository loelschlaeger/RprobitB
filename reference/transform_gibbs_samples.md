# Transformation of Gibbs samples

This function normalizes, burns and thins the Gibbs samples.

## Usage

``` r
transform_gibbs_samples(gibbs_samples, R, B, Q, normalization)
```

## Arguments

- gibbs_samples:

  The output of
  [`gibbs_sampler`](https://loelschlaeger.de/RprobitB/reference/gibbs_sampler.md),
  i.e. a list of Gibbs samples for

  - `Sigma`,

  - `alpha` (if `P_f>0`),

  - `s`, `z`, `b`, `Omega` (if `P_r>0`).

- R:

  \[`integer(1)`\]  
  The number of iterations of the Gibbs sampler.

- B:

  \[`integer(1)`\]  
  The length of the burn-in period.

- Q:

  \[`integer(1)`\]  
  The thinning factor for the Gibbs samples.

- normalization:

  An object of class `RprobitB_normalization`, which can be created via
  [`RprobitB_normalization`](https://loelschlaeger.de/RprobitB/reference/RprobitB_normalization.md).

## Value

A list, the first element `gibbs_sampes_raw` is the input
`gibbs_samples`, the second element is the normalized, burned, and
thinned version of `gibbs_samples` called `gibbs_samples_nbt`. The list
gets the class `RprobitB_gibbs_samples`.
