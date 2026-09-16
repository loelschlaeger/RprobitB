# Compare models with a Bayes factor

Estimates both marginal likelihoods with bridge sampling and compares
the first model with the second model using
[`bridgesampling::bridge_sampler()`](https://rdrr.io/pkg/bridgesampling/man/bridge_sampler.html)
and
[`bridgesampling::bf()`](https://rdrr.io/pkg/bridgesampling/man/bf.html).

## Usage

``` r
bayes_factor(
  model1,
  model2,
  log = FALSE,
  repetitions = 1L,
  ghk_draws = 500L,
  progress = interactive()
)
```

## Arguments

- model1, model2:

  \[`RprobitB_fit`\]  
  Fitted models to compare.

- log:

  \[`logical(1)`\]  
  Return the logarithm of the Bayes factor?

- repetitions:

  \[`integer(1)`\]  
  Number of independent bridge-sampling repetitions.

- ghk_draws:

  \[`integer(1)`\]  
  Number of draws of the GHK simulator for multivariate normal
  probabilities of more than three dimensions, see
  [`oeli::pmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md).

- progress:

  \[`logical(1)`\]  
  Show progress?

## Value

A `bf_bridge` object from
[**bridgesampling**](https://CRAN.R-project.org/package=bridgesampling).
Values greater than one favor `model1`; values below one favor `model2`.

## References

Gronau QF, Singmann H, Wagenmakers E (2020). “bridgesampling: An R
Package for Estimating Normalizing Constants.” *Journal of Statistical
Software*, **92**(10), 1–29.
[doi:10.18637/jss.v092.i10](https://doi.org/10.18637/jss.v092.i10) .

## Examples

``` r
### Simulate and fit the correctly specified model
set.seed(1)
correct_model <- fit(
  choice ~ x + z | 0,
  dgp_parameters = list(beta = c(x = 1, z = 0.5)),
  chains = 1
)
simulated_data <- as.data.frame(correct_model$data)

### Fit the same data again, but omit the relevant regressor z
misspecified_model <- fit(
  choice ~ x | 0,
  data = simulated_data,
  chains = 1
)

### A Bayes factor greater than one favors the correct first model
bayes_factor(correct_model, misspecified_model)
#> Estimated Bayes factor in favor of model1 over model2: 7.55958
```
