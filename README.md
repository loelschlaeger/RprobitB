# RprobitB <img src="man/figures/logo.png" align="right" height=136 />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/RprobitB)](https://www.r-pkg.org/badges/version-last-release/RprobitB)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/RprobitB)](https://cranlogs.r-pkg.org/badges/grand-total/RprobitB)
[![R-CMD-check](https://github.com/loelschlaeger/RprobitB/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/RprobitB/actions)
<!-- badges: end -->

The goal of RprobitB is to fit mixed probit models to choice data. 
The package differs from comparable packages in two ways: Bayesian estimation and a focus on taste heterogeneity.


## Installation

You can install the released version of RprobitB from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("RprobitB")
```

## Documentation

The package is documented in several vignettes:

```r
browseVignettes("RprobitB")
```

## Example

This is a basic example to show how to fit a mixed probit model and make choice predictions:

``` r
library(RprobitB)
data("Train", package = "mlogit")
data = prepare(form = choice ~ price | 0 | time + comfort + change,
               choice_data = Train,
               re = "price",
               standardize = "all",
               test_prop = 0.5)
model = mcmc(data$train)
summary(model)
plot(model, type = "mixture")
predict(model, data$test)
```

