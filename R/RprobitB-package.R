#' RprobitB: Bayesian probit choice modeling
#'
#' @description
#' Fits Bayesian probit models for binary, multinomial, ordered, and ranked
#' choices in cross-sectional and panel data.
#'
#' @details
#' ## Model
#' Decider `n` faces `J` alternatives on choice occasion `t`. Alternative `j`
#' has the latent utility `U[n,t,j] = X[n,t,j] %*% beta[n] + epsilon[n,t,j]`,
#' where the covariate vector `X[n,t,j]` follows from `formula` and the error
#' vector across alternatives is multivariate normal with covariance `Sigma`.
#' Unordered choices select the alternative with the largest utility, ranked
#' choices order all utilities, and ordered choices compare one utility with
#' increasing thresholds `gamma`.
#'
#' Fixed coefficients are identical for all deciders. Random coefficients named
#' in `random_effects` vary across deciders and follow a multivariate normal
#' distribution with mean `mu` and covariance `Omega`. Effects named in
#' `latent_class_effects` differ between `classes` latent classes with weights
#' `weight`: a random effect then has class-specific means and covariances,
#' another coefficient one value per class. Log-normal effects apply `exp()`
#' or `-exp()` to the latent normal coefficient. Utilities are identified only
#' up to level and scale, see the normalization details in [fit()].
#'
#' ## Estimation
#' Posterior draws come from a Gibbs sampler that augments the latent utilities
#' (Albert and Chib 1993; McCulloch and Rossi 1994; Imai and van Dyk 2005).
#' Heterogeneity can follow a finite mixture, a sparse overfitted finite
#' mixture (Rousseau and Mengersen 2011; Frühwirth-Schnatter and Malsiner-Walli
#' 2019), or a Dirichlet process mixture with Neal's (2000) auxiliary-parameter
#' allocation update and the precision update of Escobar and West (1995).
#' Independent chains run through the
#' [**future**](https://future.futureverse.org/) framework, and retained
#' draws use the [**posterior**](https://mc-stan.org/posterior/) format.
#'
#' ## Evaluation
#' [predict()], [residuals()], [logLik()], [WAIC()], [loo()], and
#' [bayes_factor()] evaluate choice probabilities and likelihoods with
#' [**choicedata**](https://loelschlaeger.de/choicedata/). Panel likelihoods
#' of mixed models integrate over the random coefficients with the GHK
#' simulator (Train 2009). Information criteria follow Vehtari, Gelman, and
#' Gabry (2017), and Bayes factors use bridge sampling (Gronau et al. 2017).
#'
#' @references
#' \insertRef{Albert1993}{RprobitB}
#'
#' \insertRef{Escobar1995}{RprobitB}
#'
#' \insertRef{FruehwirthSchnatter2019}{RprobitB}
#'
#' \insertRef{Gronau2017}{RprobitB}
#'
#' \insertRef{Imai2005a}{RprobitB}
#'
#' \insertRef{McCulloch1994}{RprobitB}
#'
#' \insertRef{Neal2000}{RprobitB}
#'
#' \insertRef{Oelschlaeger2021}{RprobitB}
#'
#' \insertRef{Oelschlaeger2026c}{RprobitB}
#'
#' \insertRef{Rousseau2011}{RprobitB}
#'
#' \insertRef{Train2009}{RprobitB}
#'
#' \insertRef{Vehtari2017}{RprobitB}
#' @useDynLib RprobitB, .registration=TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom Rdpack reprompt
#' @importFrom choicedata choice_data train_test
#' @importFrom loo loo
#' @importFrom posterior as_draws
#' @importFrom stats coef confint formula logLik model.frame nobs predict
#' @importFrom stats residuals update vcov
#' @keywords internal

"_PACKAGE"

# This dummy function definition ensures that the native routine registration
# includes the 'run_testthat_tests' symbol of the compiled unit tests.
(function() {
  .Call("run_testthat_tests", FALSE, PACKAGE = "RprobitB")
})

#' @rdname loo.RprobitB_fit
#' @export

loo::loo

#' @export

choicedata::train_test
