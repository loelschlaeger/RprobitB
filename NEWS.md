# RprobitB 1.0.0.9000

* Added print method for `RprobitB_parameter`.

* The function `choice_probs` is now called `choice_probabilities()` to make its functionality clearer.

* Splitting the data set into a train and test part can now be done by the function `train_test()`. Consequently, the argument `test_prob` was removed in `prepare_data()` and `simulate_choices()`.

* The function `simulate_choices()` does not have the argument `distr` anymore. Instead, covariates can be supplied via the `covariates` argument. Consequently, the argument `standardize` was removed as well.

* The function `compare` is now called `model_selection()` to make its functionality clearer.

* The function `prepare` is now called `prepare_data()` to make its functionality clearer.

* The function `simulate` is now called `simulate_choices()` to not mask `stats::simulate()`.

* The function `mcmc` is now called `fit_model()` to make its functionality clearer.

* The README file is now in R Markdown format.

* Simplified specifying the utility `scale`, see the help page of `RprobitB_normalization()`.

# RprobitB 1.0.0

* Integrated S3 classes and methods.

* Several new functionalities.

# RprobitB 0.1.0

* Initial version.
