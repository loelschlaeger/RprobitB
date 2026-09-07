# Compare fitted models

This function returns a table with several criteria for model
comparison.

## Usage

``` r
model_selection(
  ...,
  criteria = c("npar", "LL", "AIC", "BIC"),
  add_form = FALSE
)

# S3 method for class 'RprobitB_model_selection'
print(x, digits = 2, ...)
```

## Arguments

- ...:

  One or more objects of class `RprobitB_fit`.

- criteria:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  One or more of the following:

  - `"npar"` for the number of model parameters (see
    [`npar`](https://loelschlaeger.de/RprobitB/reference/npar.md)),

  - `"LL"` for the log-likelihood value (see
    [`logLik`](https://rdrr.io/r/stats/logLik.html)),

  - `"AIC"` for the AIC value (see
    [`AIC`](https://rdrr.io/r/stats/AIC.html)),

  - `"BIC"` for the BIC value (see
    [`BIC`](https://rdrr.io/r/stats/AIC.html)),

  - `"WAIC"` for the WAIC value (also shows its standard error
    `sd(WAIC)` and the number `pWAIC` of effective model parameters, see
    [`WAIC`](https://loelschlaeger.de/RprobitB/reference/WAIC.md)),

  - `"MMLL"` for the marginal model log-likelihood,

  - `"BF"` for the Bayes factor,

  - `"pred_acc"` for the prediction accuracy (see
    [`pred_acc`](https://loelschlaeger.de/RprobitB/reference/pred_acc.md)).

- add_form:

  \[`logical(1)`\]  
  Add the model formulas?

- x:

  An object of class `RprobitB_model_selection`.

- digits:

  \[`integer(1)`\]  
  The number of digits.

## Value

A `data.frame`, criteria in columns, models in rows.

## Details

See the vignette on model selection for more details.
