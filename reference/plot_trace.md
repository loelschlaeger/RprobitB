# Visualizing the trace of Gibbs samples.

This function plots traces of the Gibbs samples.

## Usage

``` r
plot_trace(gibbs_samples, par_labels)
```

## Arguments

- gibbs_samples:

  A matrix of Gibbs samples.

- par_labels:

  A character vector of length equal to the number of columns of
  `gibbs_samples`, containing labels for the Gibbs samples.

## Value

No return value. Draws a plot to the current device.
