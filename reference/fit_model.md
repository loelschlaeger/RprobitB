# Fit probit model to choice data

This function performs MCMC simulation for fitting different types of
probit models (binary, multivariate, mixed, latent class, ordered,
ranked) to discrete choice data.

## Usage

``` r
fit_model(
  data,
  scale = "Sigma_1,1 := 1",
  R = 1000,
  B = R/2,
  Q = 1,
  print_progress = getOption("RprobitB_progress", default = TRUE),
  prior = NULL,
  latent_classes = NULL,
  fixed_parameter = list(),
  save_beta_draws = FALSE
)
```

## Arguments

- data:

  An object of class `RprobitB_data`.

- scale:

  \[`character(1)`\]  
  A character which determines the utility scale. It is of the form
  `<parameter> := <value>`, where `<parameter>` is either the name of a
  fixed effect or `Sigma_<j>,<j>` for the `<j>`th diagonal element of
  `Sigma`, and `<value>` is the value of the fixed parameter.

- R:

  \[`integer(1)`\]  
  The number of iterations of the Gibbs sampler.

- B:

  \[`integer(1)`\]  
  The length of the burn-in period.

- Q:

  \[`integer(1)`\]  
  The thinning factor for the Gibbs samples.

- print_progress:

  \[`logical(1)`\]  
  Print the Gibbs sampler progress?

- prior:

  \[`list`\]  
  A named list of parameters for the prior distributions. See the
  documentation of
  [`check_prior`](https://loelschlaeger.de/RprobitB/reference/check_prior.md)
  for details about which parameters can be specified.

- latent_classes:

  \[[`list()`](https://rdrr.io/r/base/list.html) \| `NULL`\]  
  Optionally parameters specifying the number of latent classes and
  their updating scheme. The values in brackets are the default.

  - `C` (`1`): The fixed number (greater or equal 1) of (initial)
    classes.

  - `wb_update` (`FALSE`): Set to `TRUE` for weight-based class updates.

  - `dp_update` (`FALSE`): Set to `TRUE` for Dirichlet process class
    updates.

  - `Cmax` (`10`): The maximum number of latent classes.

  The following specifications are used for the weight-based updating
  scheme:

  - `buffer` (`50`): The number of iterations to wait before the next
    update.

  - `epsmin` (`0.01`): The threshold weight for removing a latent class.

  - `epsmax` (`0.7`): The threshold weight for splitting a latent class.

  - `deltamin` (`0.1`): The minimum mean distance before merging two
    classes.

  - `deltashift` (`0.5`): The scale for shifting the class means after a
    split.

- fixed_parameter:

  \[`list`\]  
  A named list with fixed parameter values for `alpha`, `C`, `s`, `b`,
  `Omega`, `Sigma`, `Sigma_full`, `beta`, `z`, or `d` for the
  simulation.

  See [the vignette on model
  definition](https://loelschlaeger.de/RprobitB/articles/v01_model_definition.html)
  for definitions of these variables.

- save_beta_draws:

  \[`logical(1)`\]  
  Save draws for decider-specific coefficient vectors? Usually not
  recommended, as it requires a lot of storage space.

## Value

An object of class `RprobitB_fit`.

## See also

- [`prepare_data()`](https://loelschlaeger.de/RprobitB/reference/prepare_data.md)
  and
  [`simulate_choices()`](https://loelschlaeger.de/RprobitB/reference/simulate_choices.md)
  for building an `RprobitB_data` object

- [`update()`](https://rdrr.io/r/stats/update.html) for estimating
  nested models

- [`transform()`](https://rdrr.io/r/base/transform.html) for
  transforming a fitted model

## Examples

``` r
set.seed(1)
form <- choice ~ var | 0
data <- simulate_choices(form = form, N = 100, T = 10, J = 3, re = "var")
model <- fit_model(data = data, R = 1000)
#> Computing sufficient statistics - 0 of 4  
#> Computing sufficient statistics - 1 of 4  
#> Computing sufficient statistics - 2 of 4  
#> Computing sufficient statistics - 3 of 4  
#> Computing sufficient statistics - 4 of 4  
#> Gibbs sampler - 1 of 1000 iterations 
#> Gibbs sampler - 10 of 1000 iterations 
#> Gibbs sampler - 20 of 1000 iterations 
#> Gibbs sampler - 30 of 1000 iterations 
#> Gibbs sampler - 40 of 1000 iterations 
#> Gibbs sampler - 50 of 1000 iterations 
#> Gibbs sampler - 60 of 1000 iterations 
#> Gibbs sampler - 70 of 1000 iterations 
#> Gibbs sampler - 80 of 1000 iterations 
#> Gibbs sampler - 90 of 1000 iterations 
#> Gibbs sampler - 100 of 1000 iterations 
#> Gibbs sampler - 110 of 1000 iterations 
#> Gibbs sampler - 120 of 1000 iterations 
#> Gibbs sampler - 130 of 1000 iterations 
#> Gibbs sampler - 140 of 1000 iterations 
#> Gibbs sampler - 150 of 1000 iterations 
#> Gibbs sampler - 160 of 1000 iterations 
#> Gibbs sampler - 170 of 1000 iterations 
#> Gibbs sampler - 180 of 1000 iterations 
#> Gibbs sampler - 190 of 1000 iterations 
#> Gibbs sampler - 200 of 1000 iterations 
#> Gibbs sampler - 210 of 1000 iterations 
#> Gibbs sampler - 220 of 1000 iterations 
#> Gibbs sampler - 230 of 1000 iterations 
#> Gibbs sampler - 240 of 1000 iterations 
#> Gibbs sampler - 250 of 1000 iterations 
#> Gibbs sampler - 260 of 1000 iterations 
#> Gibbs sampler - 270 of 1000 iterations 
#> Gibbs sampler - 280 of 1000 iterations 
#> Gibbs sampler - 290 of 1000 iterations 
#> Gibbs sampler - 300 of 1000 iterations 
#> Gibbs sampler - 310 of 1000 iterations 
#> Gibbs sampler - 320 of 1000 iterations 
#> Gibbs sampler - 330 of 1000 iterations 
#> Gibbs sampler - 340 of 1000 iterations 
#> Gibbs sampler - 350 of 1000 iterations 
#> Gibbs sampler - 360 of 1000 iterations 
#> Gibbs sampler - 370 of 1000 iterations 
#> Gibbs sampler - 380 of 1000 iterations 
#> Gibbs sampler - 390 of 1000 iterations 
#> Gibbs sampler - 400 of 1000 iterations 
#> Gibbs sampler - 410 of 1000 iterations 
#> Gibbs sampler - 420 of 1000 iterations 
#> Gibbs sampler - 430 of 1000 iterations 
#> Gibbs sampler - 440 of 1000 iterations 
#> Gibbs sampler - 450 of 1000 iterations 
#> Gibbs sampler - 460 of 1000 iterations 
#> Gibbs sampler - 470 of 1000 iterations 
#> Gibbs sampler - 480 of 1000 iterations 
#> Gibbs sampler - 490 of 1000 iterations 
#> Gibbs sampler - 500 of 1000 iterations 
#> Gibbs sampler - 510 of 1000 iterations 
#> Gibbs sampler - 520 of 1000 iterations 
#> Gibbs sampler - 530 of 1000 iterations 
#> Gibbs sampler - 540 of 1000 iterations 
#> Gibbs sampler - 550 of 1000 iterations 
#> Gibbs sampler - 560 of 1000 iterations 
#> Gibbs sampler - 570 of 1000 iterations 
#> Gibbs sampler - 580 of 1000 iterations 
#> Gibbs sampler - 590 of 1000 iterations 
#> Gibbs sampler - 600 of 1000 iterations 
#> Gibbs sampler - 610 of 1000 iterations 
#> Gibbs sampler - 620 of 1000 iterations 
#> Gibbs sampler - 630 of 1000 iterations 
#> Gibbs sampler - 640 of 1000 iterations 
#> Gibbs sampler - 650 of 1000 iterations 
#> Gibbs sampler - 660 of 1000 iterations 
#> Gibbs sampler - 670 of 1000 iterations 
#> Gibbs sampler - 680 of 1000 iterations 
#> Gibbs sampler - 690 of 1000 iterations 
#> Gibbs sampler - 700 of 1000 iterations 
#> Gibbs sampler - 710 of 1000 iterations 
#> Gibbs sampler - 720 of 1000 iterations 
#> Gibbs sampler - 730 of 1000 iterations 
#> Gibbs sampler - 740 of 1000 iterations 
#> Gibbs sampler - 750 of 1000 iterations 
#> Gibbs sampler - 760 of 1000 iterations 
#> Gibbs sampler - 770 of 1000 iterations 
#> Gibbs sampler - 780 of 1000 iterations 
#> Gibbs sampler - 790 of 1000 iterations 
#> Gibbs sampler - 800 of 1000 iterations 
#> Gibbs sampler - 810 of 1000 iterations 
#> Gibbs sampler - 820 of 1000 iterations 
#> Gibbs sampler - 830 of 1000 iterations 
#> Gibbs sampler - 840 of 1000 iterations 
#> Gibbs sampler - 850 of 1000 iterations 
#> Gibbs sampler - 860 of 1000 iterations 
#> Gibbs sampler - 870 of 1000 iterations 
#> Gibbs sampler - 880 of 1000 iterations 
#> Gibbs sampler - 890 of 1000 iterations 
#> Gibbs sampler - 900 of 1000 iterations 
#> Gibbs sampler - 910 of 1000 iterations 
#> Gibbs sampler - 920 of 1000 iterations 
#> Gibbs sampler - 930 of 1000 iterations 
#> Gibbs sampler - 940 of 1000 iterations 
#> Gibbs sampler - 950 of 1000 iterations 
#> Gibbs sampler - 960 of 1000 iterations 
#> Gibbs sampler - 970 of 1000 iterations 
#> Gibbs sampler - 980 of 1000 iterations 
#> Gibbs sampler - 990 of 1000 iterations 
#> Gibbs sampler - 1000 of 1000 iterations 
summary(model)
#> Probit model
#> Formula: choice ~ var | 0 
#> R: 1000, B: 500, Q: 1
#> Level: Utility differences with respect to alternative 'C'.
#> Scale: Coefficient of the 1. error term variance fixed to 1.
#> 
#> Latent classes
#> C = 1 
#> 
#> Gibbs sample statistics
#>           true    mean      sd      R^
#>  b
#>                                       
#>    1.1    0.47    0.62    0.09    1.01
#> 
#>  Omega
#>                                       
#>  1.1,1    0.35    0.52    0.15    1.01
#> 
#>  Sigma
#>                                       
#>    1,1    1.00    1.00    0.00    1.00
#>    1,2    1.30    1.20    0.13    1.01
#>    2,2    2.79    2.86    0.55    1.04
```
