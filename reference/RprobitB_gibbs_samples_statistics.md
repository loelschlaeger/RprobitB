# Create object of class `RprobitB_gibbs_samples_statistics`

This function creates an object of class
`RprobitB_gibbs_samples_statistics`.

## Usage

``` r
RprobitB_gibbs_samples_statistics(gibbs_samples, FUN = list(mean = mean))

# S3 method for class 'RprobitB_gibbs_samples_statistics'
print(x, true = NULL, digits = 2, ...)
```

## Arguments

- gibbs_samples:

  An object of class `RprobitB_gibbs_samples`, which generally is
  located as object `gibbs_samples` in an `RprobitB_fit` object.

- FUN:

  A (preferably named) list of functions that compute parameter
  statistics from the Gibbs samples, for example

  - `mean` for the mean,

  - `sd` for the standard deviation,

  - `min` for the minimum,

  - `max` for the maximum,

  - `median` for the median,

  - `function(x) quantile(x, p)` for the `p`th quantile,

  - `R_hat` for the Gelman-Rubin statistic.

- true:

  Either `NULL` or an object of class `RprobitB_parameter`.

## Value

An object of class `RprobitB_gibbs_samples_statistics`, which is a list
of statistics from `gibbs_samples` obtained by applying the elements of
`FUN`.
