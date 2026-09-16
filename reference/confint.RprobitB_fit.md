# Compute posterior credible intervals

Computes equal-tailed posterior intervals for selected model variables.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- parm:

  \[[`character()`](https://rdrr.io/r/base/character.html) \| `NULL`\]  
  Variables to include. `NULL` includes every population-level posterior
  variable that is not fixed by the normalization or the model
  structure. Individual coefficients can be selected by their
  `individual[effect,decider]` names.

- level:

  \[`numeric(1)`\]  
  Probability of the equal-tailed credible intervals.

- ...:

  Currently not used.

## Value

A numeric matrix with one row per selected variable and columns for the
lower and upper credible limits.

## Details

The returned intervals are credible intervals, not confidence intervals.
They are reported through
[`stats::confint()`](https://rdrr.io/r/stats/confint.html) because it is
the standard way to ask a fitted model for interval estimates.

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), chains = 1
)
confint(model)
#>              2.5%    97.5%
#> beta[x] 0.9766132 1.900822
```
