# RprobitB <img src="man/figures/logo.png" align="right" height=136 />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/RprobitB)](https://www.r-pkg.org/badges/version-last-release/RprobitB)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/RprobitB)](https://cranlogs.r-pkg.org/badges/grand-total/RprobitB)
[![R-CMD-check](https://github.com/loelschlaeger/RprobitB/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/RprobitB/actions)
<!-- badges: end -->

The goal of RprobitB is to fit (latent class) (mixed) (multinomial) probit models to simulated or empirical choice data via Bayesian estimation.

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

This is a basic example which shows how to fit a mixed multinomial probit model:

``` r
library(RprobitB)
data("Train", package = "mlogit")
data = prepare(form = choice ~ price | 0 | time + comfort + change,
               choice_data = Train,
               re = c("price","time"),
               standardize = "all")
model = mcmc(data)
summary(model)
plot(model)
choice_probs(model)
predict(model)
```

