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
#> 1 -0.133084065  0.133084065
#> 2 -0.001489686  0.001489686
#> 3  0.060605347 -0.060605347
#> 4  0.636980501 -0.636980501
#> 5 -0.885971294  0.885971294
#> 6  0.064948279 -0.064948279
```
