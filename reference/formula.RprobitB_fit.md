# Extract the fitted formula

Returns the normalized three-part model formula stored in a fitted
model.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
formula(x, ...)
```

## Arguments

- x:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- ...:

  Currently not used.

## Value

A `formula` object.

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x, dgp_parameters = list(beta = c(x = 1, ASC_B = -0.5)),
  chains = 1
)
formula(model)
#> choice ~ x | 1 | 0
```
