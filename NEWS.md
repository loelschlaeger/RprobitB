# RprobitB 1.2.0

* Fix in `npar.RprobitB_fit()`. Parameters for the covariance matrices of the mixing distributions have been miscounted.

* Fix in `sufficient_statistics()`. There was a bug in calculating sufficient statistics in the ordered case.

* Added function `mode_approx()` to approximate conditional posterior modes.

* Documentation improvements.

# RprobitB 1.1.4

* Small fixes and documentation improvements.

# RprobitB 1.1.3

* Small fixes.

* Added `train_choice` data set.

# RprobitB 1.1.2

* `logLik()` method now returns a `logLik` object that can be passed to `AIC()` and `BIC()` methods from {stats}.

* Small bug fixes for the ordered probit model case.

# RprobitB 1.1.1

* Small documentation fixes.

# RprobitB 1.1.0

## New functionality

* Ranked probit models

* Ordered probit models

## Updated functionality

* Print method for `RprobitB_parameter`.

* Splitting the data set into a train and test part can now be done by the function `train_test()`. Consequently, the argument `test_prob` was removed in `prepare_data()` and `simulate_choices()`.

* The function `simulate_choices()` does not have the argument `distr` anymore. Instead, covariates can be supplied via the `covariates` argument. Consequently, the argument `standardize` was removed as well.

* Simplified specifying the utility `scale`, see the help page of `RprobitB_normalization()`.

## Renamed functions

* The function `compare` is now called `model_selection()`.

* The function `prepare` is now called `prepare_data()`.

* The function `simulate` is now called `simulate_choices()` to not mask `stats::simulate()`.

* The function `mcmc` is now called `fit_model()`.

* The function `choice_probs` is now called `choice_probabilities()`.

# RprobitB 1.0.0

* Integrated S3 classes and methods.

* Several new functionalities.

# RprobitB 0.1.0

* Initial version.
