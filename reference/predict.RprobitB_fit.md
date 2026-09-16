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
    of the random coefficients and applies to any decider.

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
#> 1         1           B     0.2716447     0.7283553
#> 2         2           B     0.1381003     0.8618997
#> 3         3           A     0.7816489     0.2183511
#> 4         4           B     0.4160989     0.5839011
#> 5         5           A     0.7414271     0.2585729
#> 6         6           A     0.7780650     0.2219350
head(predict(model, type = "conditional"))
#>   deciderID .prediction probability_A probability_B
#> 1         1           B     0.3317774     0.6682226
#> 2         2           B     0.0743488     0.9256512
#> 3         3           A     0.8276006     0.1723994
#> 4         4           B     0.4118027     0.5881973
#> 5         5           A     0.7659119     0.2340881
#> 6         6           A     0.8195899     0.1804101
head(predict(model, uncertainty = TRUE))
#>   deciderID .prediction probability_A probability_B       sd_A       sd_B
#> 1         1           B     0.2716447     0.7283553 0.08554722 0.08554722
#> 2         2           B     0.1381003     0.8618997 0.11074690 0.11074690
#> 3         3           A     0.7816489     0.2183511 0.09953263 0.09953263
#> 4         4           B     0.4160989     0.5839011 0.03427251 0.03427251
#> 5         5           A     0.7414271     0.2585729 0.08931334 0.08931334
#> 6         6           A     0.7780650     0.2219350 0.09871652 0.09871652
#>      lower_A    lower_B   upper_A   upper_B
#> 1 0.13559732 0.57757557 0.4224244 0.8644027
#> 2 0.01138985 0.61909101 0.3809090 0.9886102
#> 3 0.59434396 0.07155514 0.9284449 0.4056560
#> 4 0.35737357 0.52629718 0.4737028 0.6426264
#> 5 0.58216748 0.11876899 0.8812310 0.4178325
#> 6 0.59327933 0.07548036 0.9245196 0.4067207

### new choice occasions
new_data <- data.frame(deciderID = 21:22, x_A = c(1, -1), x_B = c(0, 0))
predict(model, newdata = new_data)
#>   deciderID .prediction probability_A probability_B
#> 1        21           A     0.7608952     0.2391048
#> 2        22           B     0.2391048     0.7608952
```
