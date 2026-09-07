# Compute Gelman-Rubin statistic

This function computes the Gelman-Rubin statistic `R_hat`.

## Usage

``` r
R_hat(samples, parts = 2)
```

## Arguments

- samples:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html) \| `matrix`\]  
  Samples from a Markov chain.

  If it is a matrix, each column gives the samples for a separate chain.

- parts:

  \[`integer(1)`\]  
  The number of parts to divide each chain into sub-chains.

## Value

The Gelman-Rubin statistic.

## Details

NA values in `samples` are ignored. The degenerate case is indicated by
`NA`. The Gelman-Rubin statistic is bounded by 1 from below. Values
close to 1 indicate reasonable convergence.

## Examples

``` r
no_chains <- 2
length_chains <- 1e3
samples <- matrix(NA_real_, length_chains, no_chains)
samples[1, ] <- 1
Gamma <- matrix(c(0.8, 0.1, 0.2, 0.9), 2, 2)
for (c in 1:no_chains) {
  for (t in 2:length_chains) {
    samples[t, c] <- sample(1:2, 1, prob = Gamma[samples[t - 1, c], ])
  }
}
R_hat(samples)
#> [1] 1
```
