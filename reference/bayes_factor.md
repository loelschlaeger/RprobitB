# Compare models with a Bayes factor

Estimates both marginal likelihoods with bridge sampling (Meng and Wong
1996; Frühwirth-Schnatter 2004) and compares the first model with the
second model using
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
  Number of bridge-sampling repetitions.

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
Values greater than one (zero if `log = TRUE`) favor `model1`; smaller
values favor `model2`.

## References

Meng X, Wong WH (1996). “Simulating Ratios of Normalizing Constants via
a Simple Identity: A Theoretical Exploration.” *Statistica Sinica*,
**6**(4), 831–860.
<https://www3.stat.sinica.edu.tw/statistica/j6n4/j6n43/j6n43.htm>.

Frühwirth-Schnatter S (2004). “Estimating Marginal Likelihoods for
Mixture and Markov Switching Models Using Bridge Sampling Techniques.”
*The Econometrics Journal*, **7**(1), 143–167.
[doi:10.1111/j.1368-423X.2004.00125.x](https://doi.org/10.1111/j.1368-423X.2004.00125.x)
.

Gronau QF, Singmann H, Wagenmakers E (2020). “bridgesampling: An R
Package for Estimating Normalizing Constants.” *Journal of Statistical
Software*, **92**(10), 1–29.
[doi:10.18637/jss.v092.i10](https://doi.org/10.18637/jss.v092.i10) .

Celeux G, Frühwirth-Schnatter S, Robert CP (2019). “Model Selection for
Mixture Models – Perspectives and Strategies.” In Frühwirth-Schnatter S,
Celeux G, Robert CP (eds.), *Handbook of Mixture Analysis*, 117–154.
Chapman and Hall/CRC.
[doi:10.1201/9780429055911-7](https://doi.org/10.1201/9780429055911-7) .

Frühwirth-Schnatter S (2019). “Keeping the Balance – Bridge Sampling for
Marginal Likelihood Estimation in Finite Mixture, Mixture of Experts and
Markov Mixture Models.” *Brazilian Journal of Probability and
Statistics*, **33**(4), 706–733.
[doi:10.1214/19-BJPS446](https://doi.org/10.1214/19-BJPS446) .

## Examples

``` r
### Simulate and fit the correctly specified model
set.seed(1)
correct_model <- fit(
  choice ~ x + z | 0,
  dgp_parameters = list(beta = c(x = 1, z = 0.5)),
  iterations = 500,
  chains = 1
)
simulated_data <- as.data.frame(correct_model$data)

### Fit the same data again, but omit the relevant regressor z
misspecified_model <- fit(
  choice ~ x | 0,
  data = simulated_data,
  iterations = 500,
  chains = 1
)

### A Bayes factor greater than one favors the correct first model
bayes_factor(correct_model, misspecified_model)
#> Estimated Bayes factor in favor of model1 over model2: 8.12778
```
