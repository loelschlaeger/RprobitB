# Changelog

## RprobitB 2.0.0

- Replaced the former multi-step workflow with
  [`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md), which
  accepts an empirical data frame or simulates data when `data = NULL`.

- Added correlated and uncorrelated normal, positive log-normal, and
  negative log-normal random effects through named `random_effects`
  specifications.

- Added `latent_class_effects` to
  [`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md), which
  names the effects that differ between the latent classes.

- Added
  [`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md),
  which reads the estimates as compensations between effects, such as
  the willingness to pay, and as average marginal effects or marginal
  effects at the average covariate values, each with posterior
  uncertainty.

- Added the [`update()`](https://rdrr.io/r/stats/update.html) method for
  fitted models, which refits with a modified specification.

- Added decider-level WAIC and PSIS-LOO and replaced the harmonic-mean
  marginal-likelihood estimator with bridge-sampling Bayes factors.

- Reworked [`predict()`](https://rdrr.io/r/stats/predict.html) and
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) around
  posterior uncertainty.

- Added posterior diagnostics and standard `bayesplot` displays through
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- Improved numerical efficiency of the Gibbs sampler.

- Rewrote the vignettes: getting started, model variants, preference
  heterogeneity, posterior prediction, and model evaluation.

## RprobitB 1.2.0

CRAN release: 2025-08-25

- Fix in `npar.RprobitB_fit()`. Parameters for the covariance matrices
  of the mixing distributions have been miscounted.

- Fix in `sufficient_statistics()`. There was a bug in calculating
  sufficient statistics in the ordered case.

- Added function `mode_approx()` to approximate conditional posterior
  modes.

- Documentation improvements.

## RprobitB 1.1.4

CRAN release: 2024-02-26

- Small fixes and documentation improvements.

## RprobitB 1.1.3

CRAN release: 2024-02-08

- Small fixes.

- Added `train_choice` data set.

## RprobitB 1.1.2

CRAN release: 2022-11-06

- [`logLik()`](https://rdrr.io/r/stats/logLik.html) method now returns a
  `logLik` object that can be passed to
  [`AIC()`](https://rdrr.io/r/stats/AIC.html) and
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) methods from {stats}.

- Small bug fixes for the ordered probit model case.

## RprobitB 1.1.1

CRAN release: 2022-08-11

- Small documentation fixes.

## RprobitB 1.1.0

CRAN release: 2022-07-22

### New functionality

- Ranked probit models

- Ordered probit models

### Updated functionality

- Print method for `RprobitB_parameter`.

- Splitting the data set into a train and test part can now be done by
  the function
  [`train_test()`](https://loelschlaeger.de/choicedata/reference/train_test.html).
  Consequently, the argument `test_prob` was removed in `prepare_data()`
  and `simulate_choices()`.

- The function `simulate_choices()` does not have the argument `distr`
  anymore. Instead, covariates can be supplied via the `covariates`
  argument. Consequently, the argument `standardize` was removed as
  well.

- Simplified specifying the utility `scale`, see the help page of
  `RprobitB_normalization()`.

### Renamed functions

- The function `compare` is now called `model_selection()`.

- The function `prepare` is now called `prepare_data()`.

- The function `simulate` is now called `simulate_choices()` to not mask
  [`stats::simulate()`](https://rdrr.io/r/stats/simulate.html).

- The function `mcmc` is now called `fit_model()`.

- The function `choice_probs` is now called `choice_probabilities()`.

## RprobitB 1.0.0

CRAN release: 2021-11-12

- Integrated S3 classes and methods.

- Several new functionalities.

## RprobitB 0.1.0

CRAN release: 2021-05-15

- Initial version.
