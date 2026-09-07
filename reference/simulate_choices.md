# Simulate choice data

This function simulates choice data from a probit model.

## Usage

``` r
simulate_choices(
  form,
  N,
  T = 1,
  J,
  re = NULL,
  alternatives = NULL,
  ordered = FALSE,
  ranked = FALSE,
  base = NULL,
  covariates = NULL,
  true_parameter = list()
)
```

## Arguments

- form:

  \[`formula`\]  
  A model description with the structure `choice ~ A | B | C`, where

  - `choice` is the name of the dependent variable (the choices),

  - `A` are names of alternative and choice situation specific
    covariates with a coefficient that is constant across alternatives,

  - `B` are names of choice situation specific covariates with
    alternative specific coefficients,

  - and `C` are names of alternative and choice situation specific
    covariates with alternative specific coefficients.

  Multiple covariates (of one type) are separated by a `+` sign. By
  default, alternative specific constants (ASCs) are added to the model.
  They can be removed by adding `+0` in the second spot.

  In the ordered probit model (`ordered = TRUE`), the `formula` object
  has the simple structure `choice ~ A`. ASCs are not estimated.

- N:

  \[`integer(1)`\]  
  The number of decision makers.

- T:

  \[`integer(1)` \| `integer(N)`\]  
  The number of choice occasions or a vector of decider-specific choice
  occasions of length `N`.

- J:

  \[`integer(1)`\]  
  The number \>= 2 of choice alternatives.

- re:

  \[[`character()`](https://rdrr.io/r/base/character.html) \| `NULL`\]  
  Names of covariates with random effects. If `re = NULL` (the default),
  there are no random effects. To have random effects for the ASCs,
  include `"ASC"` in `re`.

- alternatives:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  The names of the choice alternatives. If not specified, the choice set
  is defined by the observed choices.

  If `ordered = TRUE`, `alternatives` is assumed to be specified with
  the alternatives ordered from worst to best.

- ordered:

  \[`logical(1)`\]  
  If `TRUE`, the choice set `alternatives` is assumed to be ordered from
  worst to best.

- ranked:

  \[`logical(1)`\]  
  Are the alternatives ranked?

- base:

  \[`character(1)`\]  
  The name of the base alternative for covariates that are not
  alternative specific (i.e. type 2 covariates and ASCs).

  Ignored and set to `NULL` if the model has no alternative specific
  covariates (e.g. in the ordered probit model).

  By default, `base` is the last element of `alternatives`.

- covariates:

  \[`list`\]  
  A named list of covariate values. Each element must be a vector of
  length equal to the number of choice occasions and named according to
  a covariate. Covariates for which no values are supplied are drawn
  from a standard normal distribution.

- true_parameter:

  \[`list`\]  
  A named list with true parameter values for `alpha`, `C`, `s`, `b`,
  `Omega`, `Sigma`, `Sigma_full`, `beta`, `z`, `d` for the simulation.

  See [the vignette on model
  definition](https://loelschlaeger.de/RprobitB/articles/v01_model_definition.html)
  for definitions of these variables.

## Value

An object of class `RprobitB_data`.

## Details

See [the vignette on choice
data](https://loelschlaeger.de/RprobitB/articles/v02_choice_data.html)
for more details.

## See also

- [`check_form()`](https://loelschlaeger.de/RprobitB/reference/check_form.md)
  for checking the model formula

- [`overview_effects()`](https://loelschlaeger.de/RprobitB/reference/overview_effects.md)
  for an overview of the model effects

- [`create_lagged_cov()`](https://loelschlaeger.de/RprobitB/reference/create_lagged_cov.md)
  for creating lagged covariates

- [`as_cov_names()`](https://loelschlaeger.de/RprobitB/reference/as_cov_names.md)
  for re-labelling alternative-specific covariates

- [`prepare_data()`](https://loelschlaeger.de/RprobitB/reference/prepare_data.md)
  for preparing empirical choice data

- [`train_test()`](https://loelschlaeger.de/RprobitB/reference/train_test.md)
  for splitting choice data into a train and test subset

## Examples

``` r
### simulate data from a binary probit model with two latent classes
data <- simulate_choices(
  form = choice ~ cost | income | time,
  N = 100,
  T = 10,
  J = 2,
  re = c("cost", "time"),
  alternatives = c("car", "bus"),
  true_parameter = list(
    "alpha" = c(-1, 1),
    "b" = matrix(c(-1, -1, -0.5, -1.5, 0, -1), ncol = 2),
    "C" = 2
  )
)

### simulate data from an ordered probit model
data <- simulate_choices(
  form = opinion ~ age + gender,
  N = 10,
  T = 1:10,
  J = 5,
  alternatives = c("very bad", "bad", "indifferent", "good", "very good"),
  ordered = TRUE,
  covariates = list(
    "gender" = rep(sample(c(0, 1), 10, replace = TRUE), times = 1:10)
  )
)

### simulate data from a ranked probit model
data <- simulate_choices(
  form = product ~ price,
  N = 10,
  T = 1:10,
  J = 3,
  alternatives = c("A", "B", "C"),
  ranked = TRUE
)
```
