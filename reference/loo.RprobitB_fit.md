# Compute approximate leave-one-out cross-validation

Computes Pareto-smoothed importance-sampling leave-one-out
cross-validation using
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html).

## Usage

``` r
loo(x, ...)

# S3 method for class 'RprobitB_fit'
loo(x, ghk_draws = 500L, progress = interactive(), ...)
```

## Arguments

- x:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- ...:

  Further arguments passed to
  [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html).

- ghk_draws:

  \[`integer(1)`\]  
  Number of draws of the GHK simulator for multivariate normal
  probabilities of more than three dimensions, see
  [`oeli::pmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md).

- progress:

  \[`logical(1)`\]  
  Show progress?

## Value

A `psis_loo` object from [**loo**](https://mc-stan.org/loo/). It
contains estimates and standard errors as well as one Pareto-k
diagnostic per independent likelihood unit.

## References

Vehtari A, Gelman A, Gabry J (2017). “Practical Bayesian Model
Evaluation Using Leave-One-Out Cross-Validation and WAIC.” *Statistics
and Computing*, **27**(5), 1413–1432.
[doi:10.1007/s11222-016-9696-4](https://doi.org/10.1007/s11222-016-9696-4)
.

Vehtari A, Simpson D, Gelman A, Yao Y, Gabry J (2024). “Pareto Smoothed
Importance Sampling.” *Journal of Machine Learning Research*,
**25**(72), 1–58. <https://www.jmlr.org/papers/v25/19-556.html>.

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), n_occasions = 5,
  chains = 1
)
loo(model)
#> 
#> Computed from 500 by 100 log-likelihood matrix.
#> 
#>          Estimate   SE
#> elpd_loo   -185.5 12.2
#> p_loo         0.8  0.2
#> looic       371.0 24.3
#> ------
#> MCSE of elpd_loo is 0.0.
#> MCSE and ESS estimates assume independent draws (r_eff=1).
#> 
#> All Pareto k estimates are good (k < 0.63).
#> See help('pareto-k-diagnostic') for details.
```
