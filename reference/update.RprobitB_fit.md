# Update and refit a choice model

Refits a choice model with a modified specification.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
update(object, formula., ..., evaluate = TRUE)
```

## Arguments

- object:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- formula.:

  \[`formula`\]  
  Changes to the model formula, see the details.

- ...:

  Arguments of
  [`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md) that
  replace the ones of `object`.

- evaluate:

  \[`logical(1)`\]  
  Refit the model? If `FALSE`, the updated call is returned, where
  `data` stands for the choice data of `object`.

## Value

An object of class `RprobitB_fit`, or the updated `call` if `evaluate`
is `FALSE`.

## Details

Arguments that are not specified are taken from `object`.

The model formula is updated part by part, so `. ~ . + income` extends
the covariates that are constant across alternatives and leaves the
other two formula parts unchanged.

The choice data of `object` are reused, also if they were simulated,
which makes the updated model comparable to `object`. Supply `data` to
fit the updated model to other choice data.

## Examples

``` r
### simulate choice data and fit a model with two covariates
set.seed(1)
model <- fit(
  choice ~ x + y | 0, dgp_parameters = list(beta = c(x = 1, y = -0.5)),
  chains = 1
)
summary(model)
#> Bayesian probit choice model
#> Formula: choice ~ x + y | 0 | 0 
#> Samples: 500 retained per chain, 1 chain
#>  variable  dgp   mean   mode    sd  rhat ess_bulk
#>   beta[x]  1.0  0.913  0.950 0.174 1.002     55.3
#>   beta[y] -0.5 -0.573 -0.518 0.138 0.998     80.2

### drop `y` from the formula, the other formula parts stay as they are
model_2 <- update(model, . ~ . - y)
summary(model_2)
#> Bayesian probit choice model
#> Formula: choice ~ x | 0 | 0 
#> Samples: 500 retained per chain, 1 chain
#>  variable  mean mode   sd rhat ess_bulk
#>   beta[x] 0.754 0.76 0.13 1.06     50.4

### let the coefficient of `x` vary across deciders instead
model_3 <- update(model, random_effects = "x")
summary(model_3)
#> Bayesian probit choice model
#> Formula: choice ~ x + y | 0 | 0 
#> Samples: 500 retained per chain, 1 chain
#>    variable   mean   mode    sd rhat ess_bulk
#>     beta[y] -0.692 -0.672 0.176 1.01    48.98
#>       mu[x]  1.186  1.116 0.279 1.26     3.44
#>  Omega[x,x]  0.618  0.347 0.507 1.25     4.42
```
