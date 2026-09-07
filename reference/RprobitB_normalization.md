# Utility normalization

This function creates an object of class `RprobitB_normalization`, which
defines the utility scale and level.

## Usage

``` r
RprobitB_normalization(
  level,
  scale = "Sigma_1,1 := 1",
  form,
  re = NULL,
  alternatives,
  base,
  ordered = FALSE
)

# S3 method for class 'RprobitB_normalization'
print(x, ...)
```

## Arguments

- level:

  \[`character(1)`\]  
  The alternative name with respect to which utility differences are
  computed. Currently, only differences with respect to the last
  alternative are supported.

- scale:

  \[`character(1)`\]  
  A character which determines the utility scale. It is of the form
  `<parameter> := <value>`, where `<parameter>` is either the name of a
  fixed effect or `Sigma_<j>,<j>` for the `<j>`th diagonal element of
  `Sigma`, and `<value>` is the value of the fixed parameter.

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

- x:

  An object of class `RprobitB_normalization`.

- ...:

  Currently not used.

## Value

An object of class `RprobitB_normalization`, which is a list of

- `level`, a list with the elements `level` (the number of the
  alternative specified by the input `level`) and `name` (the name of
  the alternative, i.e. the input `level`), or alternatively `NA` in the
  ordered probit case,

- and `scale`, a list with the elements `parameter` (either `"s"` for an
  element of `Sigma` or `"a"` for an element of `alpha`), the parameter
  `index`, and the fixed `value`. If `parameter = "a"`, also the `name`
  of the fixed effect.

## Details

Utility models require normalization with respect to level and scale.

- For level normalization, `{RprobitB}` takes utility differences with
  respect to one alternative. For the ordered model where only one
  utility is modelled, `{RprobitB}` fixes the first utility threshold to
  0.

- For scale normalization, `{RprobitB}` fixes one model parameter. Per
  default, the first error-term variance is fixed to `1`. This is
  specified via `scale = "Sigma_1,1 := 1"`. Alternatively, any
  error-term variance or any non-random coefficient can be fixed.
