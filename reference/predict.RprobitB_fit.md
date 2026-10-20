# Predict choices

Computes occasion-level choice probabilities over the retained posterior
draws and predicts the alternative with the largest mean probability.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
predict(
  object,
  newdata = NULL,
  type = c("population", "conditional"),
  uncertainty = FALSE,
  level = 0.95,
  ghk_draws = 500L,
  progress = interactive(),
  ...
)
```

## Arguments

- object:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- newdata:

  \[`data.frame` \| `NULL`\]  
  Data for prediction. `NULL` uses the fitted data. A response column is
  optional. In wide format, missing decider and occasion identifiers
  make every row a choice occasion of its own decider.

- type:

  \[`character(1)`\]  
  Which coefficients to predict with:

  - `"population"` integrates over the estimated population distribution
    of the random coefficients and latent classes and applies to any
    decider.

  - `"conditional"` uses the posterior random coefficients and class
    allocations of the deciders that were observed when fitting the
    model, which `newdata` must then name.

- uncertainty:

  \[`logical(1)`\]  
  Add posterior standard deviations and credible intervals?

- level:

  \[`numeric(1)`\]  
  Probability of the credible intervals.

- ghk_draws:

  \[`integer(1)`\]  
  Number of draws of the GHK simulator for multivariate normal
  probabilities of more than three dimensions, see
  [`oeli::pmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md).

- progress:

  \[`logical(1)`\]  
  Show progress?

- ...:

  Currently not used.

## Value

A `data.frame` with one row per choice occasion, identifier columns,
`.prediction`, and one `probability_*` column per alternative. If
`uncertainty = TRUE`, `sd_*`, `lower_*`, and `upper_*` columns are
added.

## Details

Conditional prediction is based on the individual-level parameters
(Train, 2009, Chapters 11 and 12): the posterior distribution of a
decider's coefficients given their observed choices, which the Gibbs
sampler provides as draws when the model is fitted with
`save_individual_draws = TRUE`.

## References

Train KE (2009). *Discrete Choice Methods with Simulation*, 2 edition.
Cambridge University Press, Cambridge.
[doi:10.1017/CBO9780511805271](https://doi.org/10.1017/CBO9780511805271)
.

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x | 0,
  random_effects = "x",
  dgp_parameters = list(beta = c(x = 1), Omega = matrix(0.5)),
  n_deciders = 20,
  iterations = 300,
  warmup = 150,
  chains = 1,
  save_individual_draws = TRUE
)
head(predict(model))
#>   deciderID .prediction probability_A probability_B
#> 1         1           B     0.2339449     0.7660551
#> 2         2           B     0.1165867     0.8834133
#> 3         3           A     0.8158130     0.1841870
#> 4         4           B     0.3933205     0.6066795
#> 5         5           A     0.7786822     0.2213178
#> 6         6           A     0.8126050     0.1873950
head(predict(model, type = "conditional"))
#>   deciderID .prediction probability_A probability_B
#> 1         1           B    0.33861304     0.6613870
#> 2         2           B    0.04162912     0.9583709
#> 3         3           A    0.87322328     0.1267767
#> 4         4           B    0.38083540     0.6191646
#> 5         5           A    0.83490952     0.1650905
#> 6         6           A    0.87691676     0.1230832
head(predict(model, uncertainty = TRUE))
#>   deciderID .prediction probability_A probability_B       sd_A       sd_B
#> 1         1           B     0.2339449     0.7660551 0.09173230 0.09173230
#> 2         2           B     0.1165867     0.8834133 0.10253290 0.10253290
#> 3         3           A     0.8158130     0.1841870 0.09909139 0.09909139
#> 4         4           B     0.3933205     0.6066795 0.04492932 0.04492932
#> 5         5           A     0.7786822     0.2213178 0.09398348 0.09398348
#> 6         6           A     0.8126050     0.1873950 0.09874129 0.09874129
#>       lower_A    lower_B   upper_A   upper_B
#> 1 0.077389421 0.57827805 0.4217219 0.9226106
#> 2 0.003576592 0.61116874 0.3888313 0.9964234
#> 3 0.591543731 0.03369682 0.9663032 0.4084563
#> 4 0.301614977 0.52965708 0.4703429 0.6983850
#> 5 0.581647759 0.06451891 0.9354811 0.4183522
#> 6 0.590683220 0.03593066 0.9640693 0.4093168

### new choice occasions
new_data <- data.frame(deciderID = 21:22, x_A = c(1, -1), x_B = c(0, 0))
predict(model, newdata = new_data)
#>   deciderID .prediction probability_A probability_B
#> 1        21           A      0.796969      0.203031
#> 2        22           B      0.203031      0.796969
```
