# Gibbs sampler

- [x] Implement `update_classes_dp()` in C++.
- [x] Implement progress bar.
- [ ] Check and document R_hat implementation (How is chain splitted?).

# Model selection

- [x] WAIC
- [ ] Prediction accuracy
- [ ] Bayes factor
- [x] Marginal model likelihood approximation
  - [x] Prior arithmetic mean estimator
  - [ ] Importance Sampling
  - [x] Posterior harmonic mean estimator
  - [ ] Bridge Sampling
  - [ ] Thermodynamic integration
  
# Missingness

- [ ] Missing data
  - [ ] complete-case analysis
  - [ ] zero-out
  - [ ] mean
  - [ ] Bayesian imputation
- [ ] Variable choice set

# Console outputs

- [ ] Make summary output of `RprobitB_fit` more compact. Provide option to show different model aspects.

# Visualization

- [ ] Visualize beta sampling and clustering.
  - [x] Export beta and z draws from Gibbs sampler.
  - [x] Revise Gibbs sampling transformations for beta and z.
  - [x] Revise true parameter transformations for beta and z.
  - [ ] Implement plot function for beta visualization (with sleep?).
  - [x] Revise class allocation: based on last assignment or argmax of z? Update in `preference_classification()`.

# Prediction

- [ ] Implement prediction based on single predictor value.

# Ordered Probit

- [ ] Implement ordered probit model.

# Documentation

- [ ] Document Dirichlet Process in `update_classes_dp()`.

# Bugs

- [ ] `gibbs_sampling()` with low data fails with "Evaluation error: too few positive probabilities".
- [ ] Sometimes error in printing `RprobitB_parameter` objects.
- [ ] In `simulate_choices()`, specifying `s` without `seed` fails (due to partial match of ellipsis argument?).
- [ ] Error in `mml()` for `method == "pame"`: Bad prior draws.

# Tests

- [ ] Add tests for `update_classes_dp()`.

# Package ingredients

- [ ] Add precompiled models.
- [ ] `rpb()` for demonstration.
