# Check model formula

This function checks the input `form`.

## Usage

``` r
check_form(form, re = NULL, ordered = FALSE)
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

- re:

  \[[`character()`](https://rdrr.io/r/base/character.html) \| `NULL`\]  
  Names of covariates with random effects. If `re = NULL` (the default),
  there are no random effects. To have random effects for the ASCs,
  include `"ASC"` in `re`.

- ordered:

  \[`logical(1)`\]  
  If `TRUE`, the choice set `alternatives` is assumed to be ordered from
  worst to best.

## Value

A list that contains the following elements:

- The input `form`.

- The name `choice` of the dependent variable in `form`.

- The input `re`.

- A list `vars` of three character vectors of covariate names of the
  three covariate types.

- A boolean `ASC`, determining whether the model has ASCs.

## See also

[`overview_effects()`](https://loelschlaeger.de/RprobitB/reference/overview_effects.md)
for an overview of the model effects
