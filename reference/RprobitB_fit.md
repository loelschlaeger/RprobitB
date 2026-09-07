# Create object of class `RprobitB_fit`

This function creates an object of class `RprobitB_fit`.

## Usage

``` r
RprobitB_fit(
  data,
  scale,
  level,
  normalization,
  R,
  B,
  Q,
  latent_classes,
  prior,
  gibbs_samples,
  class_sequence,
  comp_time
)

# S3 method for class 'RprobitB_fit'
print(x, ...)

# S3 method for class 'RprobitB_fit'
summary(object, FUN = c(mean = mean, sd = stats::sd, `R^` = R_hat), ...)

# S3 method for class 'summary.RprobitB_fit'
print(x, digits = 2, ...)
```

## Arguments

- data:

  An object of class `RprobitB_data`.

- scale:

  \[`character(1)`\]  
  A character which determines the utility scale. It is of the form
  `<parameter> := <value>`, where `<parameter>` is either the name of a
  fixed effect or `Sigma_<j>,<j>` for the `<j>`th diagonal element of
  `Sigma`, and `<value>` is the value of the fixed parameter.

- normalization:

  An object of class `RprobitB_normalization`.

- R:

  \[`integer(1)`\]  
  The number of iterations of the Gibbs sampler.

- B:

  \[`integer(1)`\]  
  The length of the burn-in period.

- Q:

  \[`integer(1)`\]  
  The thinning factor for the Gibbs samples.

- latent_classes:

  \[[`list()`](https://rdrr.io/r/base/list.html) \| `NULL`\]  
  Optionally parameters specifying the number of latent classes and
  their updating scheme. The values in brackets are the default.

  - `C` (`1`): The fixed number (greater or equal 1) of (initial)
    classes.

  - `wb_update` (`FALSE`): Set to `TRUE` for weight-based class updates.

  - `dp_update` (`FALSE`): Set to `TRUE` for Dirichlet process class
    updates.

  - `Cmax` (`10`): The maximum number of latent classes.

  The following specifications are used for the weight-based updating
  scheme:

  - `buffer` (`50`): The number of iterations to wait before the next
    update.

  - `epsmin` (`0.01`): The threshold weight for removing a latent class.

  - `epsmax` (`0.7`): The threshold weight for splitting a latent class.

  - `deltamin` (`0.1`): The minimum mean distance before merging two
    classes.

  - `deltashift` (`0.5`): The scale for shifting the class means after a
    split.

- prior:

  \[`list`\]  
  A named list of parameters for the prior distributions. See the
  documentation of
  [`check_prior`](https://loelschlaeger.de/RprobitB/reference/check_prior.md)
  for details about which parameters can be specified.

- gibbs_samples:

  An object of class `RprobitB_gibbs_samples`.

- class_sequence:

  The sequence of class numbers during Gibbs sampling of length `R`.

- comp_time:

  The time spent for Gibbs sampling.

## Value

An object of class `RprobitB_fit`.
