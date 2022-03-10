# RprobitB TODOs

The following tasks are on our agenda and will be tackled as time permits.

## Functionality

### Gibbs sampler

- [x] Implement `update_classes_dp()` in C++.
- [x] Implement progress bar.
- [ ] Check and document `R_hat()` implementation (How is chain splitted?).

### Model selection

- [x] WAIC
- [x] Prediction accuracy in `model_selection()` output.
- [x] Marginal model likelihood approximation for Bayes factor
  - [x] Prior arithmetic mean estimator
  - [ ] Importance Sampling
  - [x] Posterior harmonic mean estimator
  - [ ] Bridge Sampling
  - [ ] Thermodynamic integration
  - [x] Pairwise Bayes factor in `model_selection()` output.
  
### Missingness

- [x] Missing data
  - [x] complete-case analysis
  - [x] zero-out
  - [x] mean
  - [ ] Bayesian imputation
- [ ] Variable choice set

### Prediction

- [ ] Implement prediction based on single predictor value.
- [ ] ROC curve (in case of two alternatives).

### Ordered Probit

- [ ] Implement ordered probit model.

## Outputs

### Console outputs

- [ ] Make summary output of `RprobitB_fit` more compact. Provide option to show different model aspects.
- [ ] Print model formula with `check_form()`.
- [ ] (Standardized) Progress bar for all computations that potentially last longer.

### Visualization

- [x] Visualize beta sampling and clustering.
  - [x] Export beta and z draws from Gibbs sampler.
  - [x] Revise Gibbs sampling transformations for beta and z.
  - [x] Revise true parameter transformations for beta and z.
  - [x] Implement plot function for beta visualization (with sleep?).
  - [x] Revise class allocation: based on last assignment or argmax of z? Update in `preference_classification()`.
- [ ] ACF of Gibbs samples
  - [ ] Exclude parameter wrt which the model is normalized.

## Performance

### Bottlenecks

- [ ] It seems that speed of `mml()` and `waic()` could be improved.

### Bugs

- [ ] `gibbs_sampling()` with low data fails with "Evaluation error: too few positive probabilities".
- [x] Error in printing `RprobitB_parameter` objects when they have matrices with one row because `ramify::pprint` cannot handle this case. Idea: Implement function that abbreviates matrix and vector printing myself.
- [ ] In `simulate_choices()`, specifying `s` without `seed` fails (due to partial match of ellipsis argument?).
- [x] Error in `mml()` for `method == "pame"`: Bad prior draws. Fixed: Prior draws had to depend on `C`.
- [ ] Error in `mml()` for `method == "pame"` and `C > 1`: Bad `s` draws.

### Tests

- [ ] Add tests for `update_classes_dp()`.

## References

### Vignettes

- [x] Introductory vignette
- [x] Model definition
- [x] Choice data
- [x] Model fitting
- [ ] Modeling heterogeneity
- [ ] Choice prediction
- [ ] Model selection

### Demos

- [ ] Add pre-compiled models.
- [ ] `rpb()` for demonstration with console feedback.

### Website

- [x] Does not find NEWS.
- [x] Does not find TODOs.

### Function documentation

- [ ] Document Dirichlet Process in `update_classes_dp()`.
