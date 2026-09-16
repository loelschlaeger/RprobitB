# Package index

## Fit models

Fit empirical data or simulate and fit a specified model.

- [`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md) : Fit a
  Bayesian probit choice model
- [`update(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/update.RprobitB_fit.md)
  : Update and refit a choice model

## Posterior analysis

Summarize, extract, and visualize posterior draws.

- [`print(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/print.RprobitB_fit.md)
  : Print a fitted choice model
- [`summary(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/summary.RprobitB_fit.md)
  : Summarize a fitted choice model
- [`print(`*`<summary.RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/print.summary.RprobitB_fit.md)
  : Print a fitted model summary
- [`latent_class_diagnostics()`](https://loelschlaeger.de/RprobitB/reference/latent_class_diagnostics.md)
  : Diagnose latent-class occupancy and membership
- [`coef(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/coef.RprobitB_fit.md)
  : Extract posterior coefficient summaries
- [`vcov(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/vcov.RprobitB_fit.md)
  : Extract the posterior covariance matrix
- [`confint(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/confint.RprobitB_fit.md)
  : Compute posterior credible intervals
- [`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
  [`print(`*`<RprobitB_interpretation>`*`)`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
  : Interpret the estimates of a fitted choice model
- [`as_draws(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/as_draws.RprobitB_fit.md)
  : Convert a fitted model to posterior draws
- [`plot(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/plot.RprobitB_fit.md)
  : Plot posterior draws

## Prediction and accessors

Predict choices, inspect residuals, and extract fitted model
information.

- [`predict(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/predict.RprobitB_fit.md)
  : Predict choices
- [`residuals(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/residuals.RprobitB_fit.md)
  : Extract choice residuals
- [`reexports`](https://loelschlaeger.de/RprobitB/reference/reexports.md)
  [`train_test`](https://loelschlaeger.de/RprobitB/reference/reexports.md)
  : Objects exported from other packages
- [`formula(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/formula.RprobitB_fit.md)
  : Extract the fitted formula
- [`model.frame(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/model.frame.RprobitB_fit.md)
  : Extract the fitted data
- [`nobs(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/nobs.RprobitB_fit.md)
  : Count independent likelihood units
- [`logLik(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/logLik.RprobitB_fit.md)
  : Extract the fitted log-likelihood

## Bayesian model evaluation

Compare predictive performance and model evidence.

- [`WAIC()`](https://loelschlaeger.de/RprobitB/reference/WAIC.md) :
  Compute the widely applicable information criterion
- [`loo()`](https://loelschlaeger.de/RprobitB/reference/loo.RprobitB_fit.md)
  : Compute approximate leave-one-out cross-validation
- [`bayes_factor()`](https://loelschlaeger.de/RprobitB/reference/bayes_factor.md)
  : Compare models with a Bayes factor

## Sampler kernels

Low-level functions for extending or testing the Gibbs sampler.

- [`sample_allocation()`](https://loelschlaeger.de/RprobitB/reference/class_updates.md)
  [`update_s()`](https://loelschlaeger.de/RprobitB/reference/class_updates.md)
  [`update_z()`](https://loelschlaeger.de/RprobitB/reference/class_updates.md)
  [`update_m()`](https://loelschlaeger.de/RprobitB/reference/class_updates.md)
  [`update_classes_wb()`](https://loelschlaeger.de/RprobitB/reference/class_updates.md)
  [`update_classes_dp()`](https://loelschlaeger.de/RprobitB/reference/class_updates.md)
  : Update latent classes
- [`update_coefficient()`](https://loelschlaeger.de/RprobitB/reference/coefficient_updates.md)
  [`update_b_c()`](https://loelschlaeger.de/RprobitB/reference/coefficient_updates.md)
  [`update_b()`](https://loelschlaeger.de/RprobitB/reference/coefficient_updates.md)
  [`update_Omega_c()`](https://loelschlaeger.de/RprobitB/reference/coefficient_updates.md)
  [`update_Omega()`](https://loelschlaeger.de/RprobitB/reference/coefficient_updates.md)
  : Update coefficient distributions
- [`d_to_gamma()`](https://loelschlaeger.de/RprobitB/reference/utility_updates.md)
  [`log_likelihood_ordered()`](https://loelschlaeger.de/RprobitB/reference/utility_updates.md)
  [`update_Sigma()`](https://loelschlaeger.de/RprobitB/reference/utility_updates.md)
  [`update_U()`](https://loelschlaeger.de/RprobitB/reference/utility_updates.md)
  [`update_U_ranked()`](https://loelschlaeger.de/RprobitB/reference/utility_updates.md)
  [`update_d()`](https://loelschlaeger.de/RprobitB/reference/utility_updates.md)
  : Update utilities and thresholds
