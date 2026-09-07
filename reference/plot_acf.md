# Autocorrelation plot of Gibbs samples

This function plots the autocorrelation of the Gibbs samples. The plots
include the total Gibbs sample size `TSS` and the effective sample size
`ESS`, see the details.

## Usage

``` r
plot_acf(gibbs_samples, par_labels)
```

## Arguments

- gibbs_samples:

  A matrix of Gibbs samples.

- par_labels:

  A character vector with labels for the Gibbs samples, of length equal
  to the number of columns of `gibbs_samples`.

## Value

No return value. Draws a plot to the current device.

## Details

The effective sample size is the value \$\$TSS / \sqrt{1 + 2\sum\_{k\geq
1} \rho_k}\$\$, where \\\rho_k\\ is the auto correlation between the
chain offset by \\k\\ positions. The auto correlations are estimated via
[`spec.ar`](https://rdrr.io/r/stats/spec.ar.html).
