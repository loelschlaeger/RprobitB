# Extract choice residuals

Computes observed choice indicators minus posterior mean occasion-level
choice probabilities.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
residuals(object, ...)
```

## Arguments

- object:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- ...:

  Further arguments passed to
  [`predict()`](https://rdrr.io/r/stats/predict.html).

## Value

A numeric matrix with one row per choice occasion and one column per
alternative. Rows with a missing response contain `NA`. For ranked data,
the indicator represents the first-ranked alternative.

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), chains = 1
)
head(residuals(model))
#>              A            B
#> 1 -0.132366558  0.132366558
#> 2 -0.001426936  0.001426936
#> 3  0.060007647 -0.060007647
#> 4  0.637347133 -0.637347133
#> 5 -0.886678596  0.886678596
#> 6  0.064335453 -0.064335453
```
