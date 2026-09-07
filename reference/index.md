# Package index

## Data management

Use these functions for the choice data preparation or simulation.

- [`check_form()`](https://loelschlaeger.de/RprobitB/reference/check_form.md)
  : Check model formula

- [`overview_effects()`](https://loelschlaeger.de/RprobitB/reference/overview_effects.md)
  : Print effect overview

- [`create_lagged_cov()`](https://loelschlaeger.de/RprobitB/reference/create_lagged_cov.md)
  : Create lagged choice covariates

- [`as_cov_names()`](https://loelschlaeger.de/RprobitB/reference/as_cov_names.md)
  : Re-label alternative specific covariates

- [`prepare_data()`](https://loelschlaeger.de/RprobitB/reference/prepare_data.md)
  : Prepare choice data for estimation

- [`RprobitB_parameter()`](https://loelschlaeger.de/RprobitB/reference/RprobitB_parameter.md)
  [`print(`*`<RprobitB_parameter>`*`)`](https://loelschlaeger.de/RprobitB/reference/RprobitB_parameter.md)
  : Define probit model parameter

- [`simulate_choices()`](https://loelschlaeger.de/RprobitB/reference/simulate_choices.md)
  : Simulate choice data

- [`train_test()`](https://loelschlaeger.de/RprobitB/reference/train_test.md)
  : Split choice data into train and test subset

- [`RprobitB_data()`](https://loelschlaeger.de/RprobitB/reference/RprobitB_data.md)
  [`print(`*`<RprobitB_data>`*`)`](https://loelschlaeger.de/RprobitB/reference/RprobitB_data.md)
  [`summary(`*`<RprobitB_data>`*`)`](https://loelschlaeger.de/RprobitB/reference/RprobitB_data.md)
  [`print(`*`<summary.RprobitB_data>`*`)`](https://loelschlaeger.de/RprobitB/reference/RprobitB_data.md)
  [`plot(`*`<RprobitB_data>`*`)`](https://loelschlaeger.de/RprobitB/reference/RprobitB_data.md)
  :

  Create object of class `RprobitB_data`

## Model fitting

Use these function for fitting a probit model to choice data.

- [`check_prior()`](https://loelschlaeger.de/RprobitB/reference/check_prior.md)
  : Check prior parameters
- [`fit_model()`](https://loelschlaeger.de/RprobitB/reference/fit_model.md)
  : Fit probit model to choice data
- [`update(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/update.RprobitB_fit.md)
  : Update and re-fit probit model
- [`transform(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/transform.md)
  : Transform fitted probit model

## Model evaluation

Use these functions for model evaluation.

- [`coef(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/coef.RprobitB_fit.md)
  [`print(`*`<RprobitB_coef>`*`)`](https://loelschlaeger.de/RprobitB/reference/coef.RprobitB_fit.md)
  [`plot(`*`<RprobitB_coef>`*`)`](https://loelschlaeger.de/RprobitB/reference/coef.RprobitB_fit.md)
  : Extract model effects

- [`cov_mix()`](https://loelschlaeger.de/RprobitB/reference/cov_mix.md)
  : Extract estimated covariance matrix of mixing distribution

- [`point_estimates()`](https://loelschlaeger.de/RprobitB/reference/point_estimates.md)
  : Compute point estimates

- [`choice_probabilities()`](https://loelschlaeger.de/RprobitB/reference/choice_probabilities.md)
  : Compute choice probabilities

- [`classification()`](https://loelschlaeger.de/RprobitB/reference/classification.md)
  : Preference-based classification of deciders

- [`get_cov()`](https://loelschlaeger.de/RprobitB/reference/get_cov.md)
  : Extract covariates of choice occasion

- [`predict(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/predict.RprobitB_fit.md)
  : Predict choices

- [`plot(`*`<RprobitB_fit>`*`)`](https://loelschlaeger.de/RprobitB/reference/plot.RprobitB_fit.md)
  : Visualize fitted probit model

- [`plot_roc()`](https://loelschlaeger.de/RprobitB/reference/plot_roc.md)
  : Plot ROC curve

- [`plot_mixture_contour()`](https://loelschlaeger.de/RprobitB/reference/plot_mixture_contour.md)
  : Plot bivariate contour of mixing distributions

- [`plot_class_allocation()`](https://loelschlaeger.de/RprobitB/reference/plot_class_allocation.md)
  :

  Plot class allocation (for `P_r = 2` only)

- [`R_hat()`](https://loelschlaeger.de/RprobitB/reference/R_hat.md) :
  Compute Gelman-Rubin statistic

- [`mode_approx()`](https://loelschlaeger.de/RprobitB/reference/mode_approx.md)
  : Gibbs sample mode

## Model selection

Use these functions for model selection.

- [`model_selection()`](https://loelschlaeger.de/RprobitB/reference/model_selection.md)
  [`print(`*`<RprobitB_model_selection>`*`)`](https://loelschlaeger.de/RprobitB/reference/model_selection.md)
  : Compare fitted models
- [`npar()`](https://loelschlaeger.de/RprobitB/reference/npar.md) :
  Extract number of model parameters
- [`mml()`](https://loelschlaeger.de/RprobitB/reference/mml.md)
  [`print(`*`<RprobitB_mml>`*`)`](https://loelschlaeger.de/RprobitB/reference/mml.md)
  [`plot(`*`<RprobitB_mml>`*`)`](https://loelschlaeger.de/RprobitB/reference/mml.md)
  : Approximate marginal model likelihood
- [`compute_p_si()`](https://loelschlaeger.de/RprobitB/reference/compute_p_si.md)
  : Compute choice probabilities at posterior samples
- [`pred_acc()`](https://loelschlaeger.de/RprobitB/reference/pred_acc.md)
  : Compute prediction accuracy

## Datasets

The following datasets are included in the package.

- [`train_choice`](https://loelschlaeger.de/RprobitB/reference/train_choice.md)
  : Stated Preferences for Train Traveling

## Posterior samplers

These functions define the Gibbs sampler.

- [`d_to_gamma()`](https://loelschlaeger.de/RprobitB/reference/d_to_gamma.md)
  : Transform increments to thresholds
- [`gibbs_sampler()`](https://loelschlaeger.de/RprobitB/reference/gibbs_sampler.md)
  : Gibbs sampler for probit models
- [`ll_ordered()`](https://loelschlaeger.de/RprobitB/reference/ll_ordered.md)
  : Compute ordered probit log-likelihood
- [`sample_allocation()`](https://loelschlaeger.de/RprobitB/reference/sample_allocation.md)
  : Sample allocation
- [`update_Omega()`](https://loelschlaeger.de/RprobitB/reference/update_Omega.md)
  : Update class covariances
- [`update_Omega_c()`](https://loelschlaeger.de/RprobitB/reference/update_Omega_c.md)
  : Update covariance of a single class
- [`update_Sigma()`](https://loelschlaeger.de/RprobitB/reference/update_Sigma.md)
  : Update error covariance matrix
- [`update_U()`](https://loelschlaeger.de/RprobitB/reference/update_U.md)
  : Update utility vector
- [`update_U_ranked()`](https://loelschlaeger.de/RprobitB/reference/update_U_ranked.md)
  : Update ranked utility vector
- [`update_b()`](https://loelschlaeger.de/RprobitB/reference/update_b.md)
  : Update class means
- [`update_b_c()`](https://loelschlaeger.de/RprobitB/reference/update_b_c.md)
  : Update mean of a single class
- [`update_classes_dp()`](https://loelschlaeger.de/RprobitB/reference/update_classes_dp.md)
  : Dirichlet process class updates
- [`update_classes_wb()`](https://loelschlaeger.de/RprobitB/reference/update_classes_wb.md)
  : Weight-based class updates
- [`update_coefficient()`](https://loelschlaeger.de/RprobitB/reference/update_coefficient.md)
  : Update coefficient vector
- [`update_d()`](https://loelschlaeger.de/RprobitB/reference/update_d.md)
  : Update utility threshold increments
- [`update_m()`](https://loelschlaeger.de/RprobitB/reference/update_m.md)
  : Update class sizes
- [`update_s()`](https://loelschlaeger.de/RprobitB/reference/update_s.md)
  : Update class weight vector
- [`update_z()`](https://loelschlaeger.de/RprobitB/reference/update_z.md)
  : Update class allocation vector
