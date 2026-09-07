# Print effect overview

This function gives an overview of the effect names, whether the
covariate is alternative-specific, whether the coefficient is
alternative-specific, and whether it is a random effect.

## Usage

``` r
overview_effects(
  form,
  re = NULL,
  alternatives,
  base = tail(alternatives, 1),
  ordered = FALSE
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

- base:

  \[`character(1)`\]  
  The name of the base alternative for covariates that are not
  alternative specific (i.e. type 2 covariates and ASCs).

  Ignored and set to `NULL` if the model has no alternative specific
  covariates (e.g. in the ordered probit model).

  By default, `base` is the last element of `alternatives`.

- ordered:

  \[`logical(1)`\]  
  If `TRUE`, the choice set `alternatives` is assumed to be ordered from
  worst to best.

## Value

A `data.frame`, each row is a effect, columns are the effect name
`"effect"`, and booleans whether the covariate is alternative-specific
`"as_value"`, whether the coefficient is alternative-specific
`"as_coef"`, and whether it is a random effect `"random"`.

## See also

[`check_form()`](https://loelschlaeger.de/RprobitB/reference/check_form.md)
for checking the model formula specification.

## Examples

``` r
overview_effects(
  form = choice ~ price + time + comfort + change | 1,
  re = c("price", "time"),
  alternatives = c("A", "B"),
  base = "A"
)
#>    effect as_value as_coef random
#> 1 comfort     TRUE   FALSE  FALSE
#> 2  change     TRUE   FALSE  FALSE
#> 3   ASC_B    FALSE    TRUE  FALSE
#> 4   price     TRUE   FALSE   TRUE
#> 5    time     TRUE   FALSE   TRUE
```
