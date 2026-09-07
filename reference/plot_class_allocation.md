# Plot class allocation (for `P_r = 2` only)

This function plots the allocation of decision-maker specific
coefficient vectors `beta` given the allocation vector `z`, the class
means `b`, and the class covariance matrices `Omega`.

## Usage

``` r
plot_class_allocation(beta, z, b, Omega, ...)
```

## Arguments

- beta:

  \[`matrix(nrow = P_r, ncol = N)`\]  
  The matrix of the decider-specific coefficient vectors.

- z:

  \[`numeric(N)`\]  
  The decider class allocations.

- b:

  \[`matrix(nrow = P_r, ncol = C)`\]  
  The matrix of class means as columns.

- Omega:

  \[`matrix(nrow = P_r * P_r, ncol = C)`\]  
  The matrix of vectorized class covariance matrices as columns.

- ...:

  Optional visualization parameters:

  - `colors`, a character vector of color specifications,

  - `perc`, a numeric between 0 and 1 to draw the `perc` percentile
    ellipsoids for the underlying Gaussian distributions (`perc = 0.95`
    per default),

  - `r`, the current iteration number of the Gibbs sampler to be
    displayed in the legend,

  - `sleep`, the number of seconds to pause after plotting.

## Value

No return value. Draws a plot to the current device.

## Details

Only applicable in the two-dimensional case, i.e. only if `P_r = 2`.
