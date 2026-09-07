# Visualizing the number of classes during Gibbs sampling

This function plots the number of latent Glasses during Gibbs sampling
to visualize the class updating.

## Usage

``` r
plot_class_seq(class_sequence, B)
```

## Arguments

- class_sequence:

  The sequence of class numbers during Gibbs sampling of length `R`.

- B:

  \[`integer(1)`\]  
  The length of the burn-in period.

## Value

No return value. Draws a plot to the current device.
