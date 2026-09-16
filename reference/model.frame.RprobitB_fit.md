# Extract the fitted data

Returns the choice data stored in a fitted model as a `data.frame`.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
model.frame(formula, ...)
```

## Arguments

- formula:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- ...:

  Currently not used.

## Value

A `data.frame` containing the fitted choice data.

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x, dgp_parameters = list(beta = c(x = 1, ASC_B = -0.5)),
  chains = 1
)
head(model.frame(model))
#>   deciderID choice        x_A        x_B
#> 1         1      B -0.6264538  0.1836433
#> 2         2      B -0.8356286  1.5952808
#> 3         3      A  0.3295078 -0.8204684
#> 4         4      A  0.4874291  0.7383247
#> 5         5      B  0.5757814 -0.3053884
#> 6         6      A  1.5117812  0.3898432
```
