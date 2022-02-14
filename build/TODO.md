# Gibbs sampler

- [x] Implement `update_classes_dp()` in C++.
- [x] Implement progress bar.
- [ ] Check and document R_hat implementation (How is chain splitted?).

# Model selection criteria

# Visualization

- [ ] Visualize beta sampling and clustering.
  - [x] Export beta and z draws from Gibbs sampler.
  - [x] Revise Gibbs sampling transformations for beta and z.
  - [x] Revise true parameter transformations for beta and z.
  - [ ] Implement plot function for beta visualization (with sleep?).
  - [x] Revise class allocation: based on last assignment or argmax of z? Update in `preference_classification()`.

# Prediction

- [ ] Implement prediction based on single predictor value.

# Documentation

- [] Document Dirichlet Process in `update_classes_dp()`.

# Bugs

- [] `gibbs_sampling()` with low data fails with "Evaluation error: too few positive probabilities".

# Test

- [] Add tests for `update_classes_dp()`.

# Package ingredients

- [] Add precompiled models.
