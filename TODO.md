# RprobitB TODOs

The following tasks are on our agenda and will be tackled as time permits.

## Functionality

### Gibbs sampler

- [ ] Make setting of `scale` in `mcmc` more intuitive.
- [ ] Use default progress bar for Gibbs sampling or show at least ETA.
- [ ] Check and document `R_hat()` implementation (How is chain splitted?).
- [ ] Fixed parameters: After each update, set fixed parameters. 
- [ ] Use `transform()` to keep on Gibbs sampling, i.e. change `R`.
- [ ] Don't save all Gibbs samples in `RprobitB_fit` but only the raw ones.
- [ ] Show average covariates of classes after classification via `preference_classification()`.
  
### Data

- [ ] Variable choice set
- [ ] Function that generates covariates. For example lagged terms or transformations.

### Ordered Probit

- [ ] Implement ordered probit model.

## Outputs

### Console outputs

- [ ] Print model formula with `check_form()`.

### Visualization

- [ ] ACF of Gibbs samples
  - [ ] Exclude parameter wrt which the model is normalized.

## Performance

### Bottlenecks

### Bugs

- [ ] `gibbs_sampling()` with low data fails with "Evaluation error: too few positive probabilities".
- [ ] In `simulate_choices()`, specifying `s` without `seed` fails (due to partial match of ellipsis argument?).

### Tests

- [ ] Add tests for `update_classes_dp()`.

## References

### Demos

- [ ] `rpb()` for demonstration with console feedback.

### Function documentation

- [ ] Document Dirichlet Process in `update_classes_dp()`.
- [ ] Document `RprobitB_latent_classes`.
