# version 1.1.3

* Small fixes.

# version 1.1.2

* `logLik()` method now returns a `logLik` object that can be passed to `AIC()` and `BIC()` methods from {stats}.
* Small bug fixes for the ordered probit model case.

# version 1.1.1

* Small documentation fixes.

# version 1.1.0

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

# version 1.0.0

* Integrated S3 classes and methods.

* Several new functionalities.

# version 0.1.0

* Initial version.
