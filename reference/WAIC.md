# Compute the widely applicable information criterion

Computes WAIC from posterior log-likelihood draws using
[`loo::waic()`](https://mc-stan.org/loo/reference/waic.html).

## Usage

``` r
WAIC(object, ghk_draws = 500L, progress = interactive(), ...)
```

## Arguments

- object:

  \[`RprobitB_fit`\]  
  Fitted choice model.

- ghk_draws:

  \[`integer(1)`\]  
  Number of draws of the GHK simulator for multivariate normal
  probabilities of more than three dimensions, see
  [`oeli::pmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md).

- progress:

  \[`logical(1)`\]  
  Show progress?

- ...:

  Further arguments passed to
  [`loo::waic()`](https://mc-stan.org/loo/reference/waic.html).

## Value

A `waic` object from [**loo**](https://mc-stan.org/loo/). Its
`estimates` matrix contains WAIC, effective parameter counts, and their
standard errors.

## References

Watanabe S (2010). “Asymptotic Equivalence of Bayes Cross Validation and
Widely Applicable Information Criterion in Singular Learning Theory.”
*Journal of Machine Learning Research*, **11**, 3571–3594.
<https://www.jmlr.org/papers/v11/watanabe10a.html>.

Vehtari A, Gelman A, Gabry J (2017). “Practical Bayesian Model
Evaluation Using Leave-One-Out Cross-Validation and WAIC.” *Statistics
and Computing*, **27**(5), 1413–1432.
[doi:10.1007/s11222-016-9696-4](https://doi.org/10.1007/s11222-016-9696-4)
.

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), n_occasions = 5,
  chains = 1
)
WAIC(model)
#> 
#> Computed from 500 by 100 log-likelihood matrix.
#> 
#>           Estimate   SE
#> elpd_waic   -185.5 12.2
#> p_waic         0.8  0.2
#> waic         371.0 24.3
```
