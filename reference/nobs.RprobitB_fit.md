# Count independent likelihood units

Counts the observed choice occasions if the model has neither random
effects nor latent classes, because the likelihood then factorizes over
the occasions. Otherwise the occasions of a decider are dependent
through the random coefficients or the class membership, and the
deciders with at least one observed response are counted.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
nobs(object, ...)
```

## Arguments

- object:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- ...:

  Currently not used.

## Value

An `integer(1)` count.

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x, dgp_parameters = list(beta = c(x = 1, ASC_B = -0.5)),
  chains = 1
)
nobs(model)
#> [1] 100
```
