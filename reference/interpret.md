# Interpret the estimates of a fitted choice model

This function translates the posterior draws to scales that are easier
to interpret:

- A compensation says how many units of a reference covariate, for
  example the price, are worth one unit of another covariate, leaving
  the utility and hence the choice probabilities unchanged.

- A marginal effect says by how much the choice probability of an
  alternative changes per unit of a covariate, computed by finite
  differences of the predicted probabilities.

Both are reported with their posterior uncertainty.

## Usage

``` r
interpret(
  object,
  type = c("compensation", "ame", "mea"),
  reference = NULL,
  effects = NULL,
  at = NULL,
  level = 0.95,
  progress = interactive()
)

# S3 method for class 'RprobitB_interpretation'
print(x, digits = 3L, ...)
```

## Arguments

- object:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- type:

  \[`character(1)`\]  
  The quantity to compute:

  - `"compensation"`: how many units of `reference` compensate one
    additional unit of every other effect, so that the utility stays the
    same. With the price as `reference`, this is the willingness to pay.

  - `"ame"`: the average marginal effect, that is, the derivative of the
    choice probability of an alternative with respect to a covariate,
    computed for every observed choice occasion and averaged.

  - `"mea"`: the marginal effect at the average, that is, the same
    derivative for a single occasion whose covariates equal the averages
    of the observed ones.

- reference:

  \[`character(1)` \| `NULL`\]  
  The effect in whose units compensations are measured. `NULL` uses the
  coefficient that `scale` fixed when fitting, and requires a choice
  otherwise. Only used if `type = "compensation"`.

- effects:

  \[[`character()`](https://rdrr.io/r/base/character.html) \| `NULL`\]  
  The effects to express in units of `reference`. `NULL` uses every
  effect other than `reference`. Only used if `type = "compensation"`.

- at:

  \[`named numeric()` \| `NULL`\]  
  Covariate values at which the marginal effects of `type = "mea"` are
  evaluated, see the details. `NULL` uses the average of every
  covariate.

- level:

  \[`numeric(1)`\]  
  Probability of the equal-tailed posterior credible interval.

- progress:

  \[`logical(1)`\]  
  Show progress?

- x:

  \[`RprobitB_interpretation`\]  
  Output of `interpret()`.

- digits:

  \[`integer(1)`\]  
  Number of significant digits to print.

- ...:

  Currently not used.

## Value

A `data.frame` of class `RprobitB_interpretation` with one row per
quantity and the columns `mean`, `sd`, `lower`, and `upper` of its
posterior distribution. Compensations have a column `effect` and, for a
mixture model, a column `class`; marginal effects have the columns
`covariate` and `alternative`, and for `type = "mea"` the column `at`.

## Compensations

The utility of an alternative is linear in the covariates, so an
increase of one unit in an effect with coefficient \\\beta_j\\ is
compensated by a change of \\-\beta_j / \beta_k\\ units in the reference
effect with coefficient \\\beta_k\\. For a random effect, the
coefficient of the median decider enters the ratio. Mixture models
report one compensation per class. The ratio is computed for every
posterior draw, so the reported uncertainty is the posterior uncertainty
of the ratio. Compensations are free of the utility scale normalization,
which makes them comparable across models.

## Marginal effects

Marginal effects are derivatives of choice probabilities and are
computed by finite differences of the predicted probabilities. Marginal
effects of a mixed model refer to the population distribution of the
random coefficients.

- `type = "ame"` averages the marginal effects over all observed
  choices.

- `type = "mea"` builds one artificial choice occasion whose covariates
  are the averages of the observed ones: the mean for numeric covariates
  and the most frequent value for the others. The argument `at` can be
  used to replace the average of the covariates.

## Examples

``` r
### travel mode choice where travel time has an alternative-specific effect
data("TravelMode", package = "AER")
TravelMode$choice <- TravelMode$choice == "yes"
TravelMode$vcost <- TravelMode$vcost / 1.6196 # cost in Euro
set.seed(1)
model <- fit(
  choice ~ vcost | 1 | travel,
  data = TravelMode,
  format = "long",
  column_decider = "individual",
  column_alternative = "mode",
  scale = c(vcost = -1),
  iterations = 100,
  chains = 1
)

### travel time must be compensated far more in the plane than in the bus
interpret(
  model, type = "compensation", effects = c("travel_bus", "travel_air")
)
#> 1 `travel_bus` compensates -0.301 `vcost` (95% interval -0.607 to -0.18)
#> 1 `travel_air` compensates -1.42 `vcost` (95% interval -3.11 to -0.84)

### the marginal effects at the average covariates, and for a short flight
interpret(model, type = "mea", at = c(travel_air = 40))
#> Marginal effects at the given covariate values
#> Change in the probability of the alternative per unit of the covariate, with 95% interval 
#>  covariate alternative    at      mean       sd     lower     upper
#>      vcost         air  52.6 -0.002850 0.000815 -0.004317 -0.001134
#>      vcost         bus  20.7 -0.001231 0.000669 -0.002688 -0.000240
#>      vcost         car  13.0 -0.002450 0.000769 -0.003730 -0.000874
#>      vcost       train  31.7 -0.002658 0.001196 -0.005184 -0.000690
#>     travel         air  40.0 -0.003663 0.000327 -0.004232 -0.002991
#>     travel         bus 629.5 -0.000342 0.000185 -0.000728 -0.000102
#>     travel         car 573.2 -0.000646 0.000165 -0.000957 -0.000384
#>     travel       train 608.3 -0.000756 0.000316 -0.001407 -0.000345
```
