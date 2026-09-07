# Changelog

## RprobitB 1.2.0

CRAN release: 2025-08-25

- Fix in
  [`npar.RprobitB_fit()`](https://loelschlaeger.de/RprobitB/reference/npar.md).
  Parameters for the covariance matrices of the mixing distributions
  have been miscounted.

- Fix in
  [`sufficient_statistics()`](https://loelschlaeger.de/RprobitB/reference/sufficient_statistics.md).
  There was a bug in calculating sufficient statistics in the ordered
  case.

- Added function
  [`mode_approx()`](https://loelschlaeger.de/RprobitB/reference/mode_approx.md)
  to approximate conditional posterior modes.

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
  [`train_test()`](https://loelschlaeger.de/RprobitB/reference/train_test.md).
  Consequently, the argument `test_prob` was removed in
  [`prepare_data()`](https://loelschlaeger.de/RprobitB/reference/prepare_data.md)
  and
  [`simulate_choices()`](https://loelschlaeger.de/RprobitB/reference/simulate_choices.md).

- The function
  [`simulate_choices()`](https://loelschlaeger.de/RprobitB/reference/simulate_choices.md)
  does not have the argument `distr` anymore. Instead, covariates can be
  supplied via the `covariates` argument. Consequently, the argument
  `standardize` was removed as well.

- Simplified specifying the utility `scale`, see the help page of
  [`RprobitB_normalization()`](https://loelschlaeger.de/RprobitB/reference/RprobitB_normalization.md).

### Renamed functions

- The function `compare` is now called
  [`model_selection()`](https://loelschlaeger.de/RprobitB/reference/model_selection.md).

- The function `prepare` is now called
  [`prepare_data()`](https://loelschlaeger.de/RprobitB/reference/prepare_data.md).

- The function `simulate` is now called
  [`simulate_choices()`](https://loelschlaeger.de/RprobitB/reference/simulate_choices.md)
  to not mask
  [`stats::simulate()`](https://rdrr.io/r/stats/simulate.html).

- The function `mcmc` is now called
  [`fit_model()`](https://loelschlaeger.de/RprobitB/reference/fit_model.md).

- The function `choice_probs` is now called
  [`choice_probabilities()`](https://loelschlaeger.de/RprobitB/reference/choice_probabilities.md).

## RprobitB 1.0.0

CRAN release: 2021-11-12

- Integrated S3 classes and methods.

- Several new functionalities.

## RprobitB 0.1.0

CRAN release: 2021-05-15

- Initial version.
