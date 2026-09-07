# Transformation of parameter values

This function transforms parameter values based on `normalization`.

## Usage

``` r
transform_parameter(parameter, normalization, ordered = FALSE)
```

## Arguments

- parameter:

  An object of class `RprobitB_parameter`.

- normalization:

  An object of class `RprobitB_normalization`.

- ordered:

  \[`logical(1)`\]  
  If `TRUE`, the choice set `alternatives` is assumed to be ordered from
  worst to best.

## Value

An object of class `RprobitB_parameter`.
