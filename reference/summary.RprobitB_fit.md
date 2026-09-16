# Summarize a fitted choice model

Summarizes the marginal posterior distributions of the fitted model
parameters with selectable statistics.

## Usage

``` r
# S3 method for class 'RprobitB_fit'
summary(
  object,
  variables = NULL,
  statistics = c("mean", "mode", "sd", "rhat", "ess_bulk"),
  probs = NULL,
  ...
)
```

## Arguments

- object:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- variables:

  \[[`character()`](https://rdrr.io/r/base/character.html) \| `NULL`\]  
  Posterior variables to summarize.

- statistics:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Posterior statistics to report, in this order. Available are `"mean"`,
  `"median"`, `"mode"`, `"sd"`, `"mcse_mean"`, `"mcse_median"`,
  `"mcse_sd"`, `"rhat"`, `"ess_bulk"`, and `"ess_tail"`, see the
  details.

- probs:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html) \| `NULL`\]  
  Optional unique probabilities for posterior quantiles.

- ...:

  Currently not used.

## Value

A `summary.RprobitB_fit` object.

## Details

Every statistic describes the marginal posterior of one variable and is
computed from the retained draws of all chains:

- `mean`: the average of the draws, the usual point estimate.

- `median`: the median value of the draws. If it differs clearly from
  the mean, the posterior is skewed.

- `mode`: the most probable value. For continuous draws, it is the peak
  of a kernel density estimate; for integer-valued draws, such as the
  active class count, it is the most frequent value.

- `sd`: the standard deviation of the draws, the posterior uncertainty
  of the parameter.

- `q<100 * p>`: the quantile of probability `p`. With
  `probs = c(0.025, 0.975)`, the two columns are the limits of the 95%
  credible interval.

- `mcse_mean`, `mcse_median`, `mcse_sd`: the Monte Carlo standard errors
  of the mean, median, and standard deviation, the sampling error of
  these estimates that more iterations would reduce. Good values are
  below a tenth of `sd`; larger values mean that the reported digits are
  not yet reliable and the sampler should run longer.

- `rhat`: the rank-normalized, folded split-R-hat of Vehtari et al.
  (2021). It compares the variance between the halves of all chains with
  the variance within them. With a single chain, it compares the two
  halves of that chain. Good values are at most 1.01; larger values mean
  that the chains have not mixed and the sampler should run longer, see
  `plot(type = "trace")`.

- `ess_bulk`: the effective sample size for the center of the posterior,
  the number of independent draws that carry the same information as the
  correlated draws. Good values are at least 100 per chain; smaller
  values mean that the sampler should run longer.

- `ess_tail`: the smaller of the effective sample sizes of the 5% and
  95% quantiles. It governs the precision of quantiles, credible
  intervals, and the standard deviation. The same rule applies: at least
  100 per chain is good.

## References

Vehtari A, Gelman A, Simpson D, Carpenter B, Bürkner P (2021).
“Rank-Normalization, Folding, and Localization: An Improved
\\\widehat{R}\\ for Assessing Convergence of MCMC.” *Bayesian Analysis*,
**16**(2), 667–718.
[doi:10.1214/20-BA1221](https://doi.org/10.1214/20-BA1221) .

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), chains = 1
)
summary(model)
#> Bayesian probit choice model
#> Formula: choice ~ x | 0 | 0 
#> Samples: 500 retained per chain, 1 chain
#>  variable dgp mean mode    sd rhat ess_bulk
#>   beta[x]   1  1.4  1.5 0.243 1.11     21.2
```
