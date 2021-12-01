
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RprobitB <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/RprobitB)](https://www.r-pkg.org/badges/version-last-release/RprobitB)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/RprobitB)](https://cranlogs.r-pkg.org/badges/grand-total/RprobitB)
[![R-CMD-check](https://github.com/loelschlaeger/RprobitB/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/RprobitB/actions)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/RprobitB/branch/main/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/RprobitB?branch=main)
<!-- badges: end -->

The goal of RprobitB is to fit mixed probit models to choice data. The
package differs from comparable packages in two ways: Bayesian
estimation and a focus on taste heterogeneity.

## Installation

You can install the released version of RprobitB from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("RprobitB")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("loelschlaeger/RprobitB")
```

## Documentation

The package is documented in several vignettes:

``` r
browseVignettes("RprobitB")
```

## Example

This is a basic example to show how to fit a mixed probit model and make
choice predictions:

First, load the package.

``` r
library(RprobitB)
#> Thanks for using RprobitB 1.0.0.9000, happy choice modeling!
#> See https://loelschlaeger.github.io/RprobitB for help.
#> Type 'citation("RprobitB")' for citing this R package.
#> 
#> Attache Paket: 'RprobitB'
#> Das folgende Objekt ist maskiert 'package:stats':
#> 
#>     simulate
```

Then, prepare choice data, for example the Train dataset from the mlogit
package. Here, we put 30 percent of data aside for later out-of-sample
prediction.

``` r
data("Train", package = "mlogit")
data = prepare(form = choice ~ price | 0 | time + comfort + change,
               choice_data = Train,
               re = "price",
               standardize = "all",
               test_prop = 0.3)
```

Call the `mcmc` function to estimate the model.

``` r
model = mcmc(data$train)
#> Iteration Info                   ETA (min)
#>         0 started Gibbs sampling          
#>      1000                                3
#>      2000                                3
#>      3000                                2
#>      4000                                2
#>      5000                                2
#>      6000                                2
#>      7000                                1
#>      8000                                1
#>      9000                                1
#>     10000 done, total time: 3 min
```

The summary method gives an overview over the estimation.

``` r
summary(model)
#> Probit model 'choice ~ price | 0 | time + comfort + change'.
#> 
#> MCMC settings:
#> - R: 10000 
#> - B: 5000 
#> - Q: 1 
#> 
#> Normalization:
#> - Level: Utility differences with respect to alternative 2.
#> - Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
#> 
#> Legend of alternatives:
#>   name
#> 1    A
#> 2    B
#> 
#> Legend of linear coefficients:
#>        name    re
#> 1    time_A FALSE
#> 2    time_B FALSE
#> 3 comfort_A FALSE
#> 4 comfort_B FALSE
#> 5  change_A FALSE
#> 6  change_B FALSE
#> 7     price  TRUE
#> 
#> Latent classes: 1 
#> - Update: FALSE 
#> 
#> Parameter statistics:
#>           mean      sd      R^
#>  alpha
#>                               
#>      1   -0.83    0.08    1.00
#>      2   -0.86    0.09    1.00
#>      3   -0.51    0.05    1.00
#>      4   -0.49    0.05    1.00
#>      5   -0.27    0.05    1.00
#>      6   -0.22    0.05    1.00
#> 
#>  s
#>                               
#>      1    1.00    0.00     NaN
#> 
#>  b
#>                               
#>    1.1   -2.17    0.26    1.00
#> 
#>  Omega
#>                               
#>  1.1,1    3.09    0.88    1.02
#> 
#>  Sigma
#>                               
#>    1,1    1.00    0.00    1.00
```

Let’s visualize the estimated mixture distribution for the price
coefficient.

``` r
plot(model, type = "mixture")
```

![](man/figures/README-plot-1.png)<!-- -->

The `predict` function makes choice predictions and compares the
prediction to the actual choices.

``` r
predict_choices(model, data$test)
#>     predicted
#> true   A   B
#>    A 306 128
#>    B 147 309
```
