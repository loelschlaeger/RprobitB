# Gibbs sampler for probit models

Gibbs sampler for probit models

## Usage

``` r
gibbs_sampler(
  sufficient_statistics,
  prior,
  latent_classes,
  fixed_parameter,
  R,
  B,
  print_progress,
  ordered,
  ranked,
  save_beta_draws = FALSE
)
```

## Arguments

- sufficient_statistics:

  \[`list`\]  
  The output of
  [`sufficient_statistics`](https://loelschlaeger.de/RprobitB/reference/sufficient_statistics.md).

- prior:

  \[`list`\]  
  A named list of parameters for the prior distributions. See the
  documentation of
  [`check_prior`](https://loelschlaeger.de/RprobitB/reference/check_prior.md)
  for details about which parameters can be specified.

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

- fixed_parameter:

  \[`list`\]  
  A named list with fixed parameter values for `alpha`, `C`, `s`, `b`,
  `Omega`, `Sigma`, `Sigma_full`, `beta`, `z`, or `d` for the
  simulation.

  See [the vignette on model
  definition](https://loelschlaeger.de/RprobitB/articles/v01_model_definition.html)
  for definitions of these variables.

- R:

  \[`integer(1)`\]  
  The number of iterations of the Gibbs sampler.

- B:

  \[`integer(1)`\]  
  The length of the burn-in period.

- print_progress:

  \[`logical(1)`\]  
  Print the Gibbs sampler progress?

- ordered:

  \[`logical(1)`\]  
  If `TRUE`, the choice set `alternatives` is assumed to be ordered from
  worst to best.

- ranked:

  \[`logical(1)`\]  
  Are the alternatives ranked?

- save_beta_draws:

  \[`logical(1)`\]  
  Save draws for decider-specific coefficient vectors? Usually not
  recommended, as it requires a lot of storage space.

## Value

A list of Gibbs samples for

- `Sigma`,

- `alpha` (only if `P_f > 0`),

- `s`, `z`, `b`, `Omega` (only if `P_r > 0`),

- `d` (only if `ordered = TRUE`),

and a vector `class_sequence` of length `R`, where the `r`-th entry is
the number of classes after iteration `r`.

## Details

This function is not supposed to be called directly, but rather via
[`fit_model`](https://loelschlaeger.de/RprobitB/reference/fit_model.md).
