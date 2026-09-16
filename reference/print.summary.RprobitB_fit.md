# Print a fitted model summary

Prints sampling information followed by the posterior summary table.

## Usage

``` r
# S3 method for class 'summary.RprobitB_fit'
print(x, digits = 3L, ...)
```

## Arguments

- x:

  \[`summary.RprobitB_fit`\]  
  Model summary returned by
  [`summary()`](https://rdrr.io/r/base/summary.html).

- digits:

  \[`integer(1)`\]  
  Number of significant digits to print.

- ...:

  Further arguments passed to
  [`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html).

## Value

`x`, invisibly.
