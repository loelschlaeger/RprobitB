# Compute prediction accuracy

This function computes the prediction accuracy of an `RprobitB_fit`
object. Prediction accuracy means the share of choices that are
correctly predicted by the model, where prediction is based on the
maximum choice probability.

## Usage

``` r
pred_acc(x, ...)
```

## Arguments

- x:

  An object of class `RprobitB_fit`.

- ...:

  Optionally specify more `RprobitB_fit` objects.

## Value

A numeric.
