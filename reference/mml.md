# Approximate marginal model likelihood

This function approximates the model's marginal likelihood.

## Usage

``` r
mml(x, S = 0, ncores = parallel::detectCores() - 1, recompute = FALSE)

# S3 method for class 'RprobitB_mml'
print(x, log = FALSE, ...)

# S3 method for class 'RprobitB_mml'
plot(x, log = FALSE, ...)
```

## Arguments

- x:

  An object of class `RprobitB_fit`.

- S:

  \[`integer(1)`\]  
  The number of prior samples for the prior arithmetic mean estimate.
  Per default, `S = 0`. In this case, only the posterior samples are
  used for the approximation via the posterior harmonic mean estimator,
  see the details section.

- ncores:

  \[`integer(1)`\]  
  The number of cores for parallel computation.

  If set to 1, no parallel backend is used.

- recompute:

  \[`logical(1)`\]  
  Recompute the probabilities?

- log:

  \[`logical(1)`\]  
  Return the logarithm of the marginal model likelihood?

- ...:

  Currently not used.

## Value

The object `x`, including the object `mml`, which is the model's
approximated marginal likelihood value.

## Details

The model's marginal likelihood \\p(y\mid M)\\ for a model \\M\\ and
data \\y\\ is required for the computation of Bayes factors. In general,
the term has no closed form and must be approximated numerically.

This function uses the posterior Gibbs samples to approximate the
model's marginal likelihood via the posterior harmonic mean estimator.
To check the convergence, call `plot(x$mml)`, where `x` is the output of
this function. If the estimation does not seem to have converged, you
can improve the approximation by combining the value with the prior
arithmetic mean estimator. The final approximation of the model's
marginal likelihood than is a weighted sum of the posterior harmonic
mean estimate and the prior arithmetic mean estimate, where the weights
are determined by the sample sizes.
