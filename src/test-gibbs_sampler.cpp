#include <testthat.h>
#include "gibbs_sampler.h"

context("sampler helpers") {

test_that("symmetric positive definite matrices are inverted") {
  arma::mat matrix = {{4.0, 1.0}, {1.0, 3.0}};
  arma::mat inverse = inv_spd(matrix);
  expect_true(arma::approx_equal(
    matrix * inverse, arma::eye(2, 2), "absdiff", 1e-10
  ));
  arma::mat indefinite = {{1.0, 2.0}, {2.0, 1.0}};
  expect_true(arma::approx_equal(
    indefinite * inv_spd(indefinite), arma::eye(2, 2), "absdiff", 1e-10
  ));
}

test_that("random coefficients are transformed to the utility scale") {
  arma::vec latent = {0.5, 0.5, 0.5};
  arma::ivec distribution = {0, 1, -1};
  arma::vec utility = transform_random_effects(latent, distribution);
  expect_true(std::abs(utility[0] - 0.5) < 1e-12);
  expect_true(std::abs(utility[1] - std::exp(0.5)) < 1e-12);
  expect_true(std::abs(utility[2] + std::exp(0.5)) < 1e-12);
}

test_that("the class concentration density is a gamma posterior kernel") {
  arma::vec weights = {0.5, 0.5};
  const double value = log_density_delta(2.0, weights, 1.0, 1.0);
  const double expected = std::lgamma(4.0) - 2.0 * std::lgamma(2.0) +
    std::log(0.5) + std::log(0.5) - 2.0;
  expect_true(std::abs(value - expected) < 1e-10);
  expect_true(log_density_delta(0.0, weights, 1.0, 1.0) ==
    -std::numeric_limits<double>::infinity());
}

test_that("the class concentration is updated") {
  Rcpp::RNGScope scope;
  arma::vec weights = {0.4, 0.35, 0.25};
  const double finite = update_delta_finite(0.5, weights, 1.0, 200.0);
  const double process = update_delta_dp(0.5, 23, 3, 2.0, 4.0);
  expect_true(std::isfinite(finite));
  expect_true(finite > 0.0);
  expect_true(std::isfinite(process));
  expect_true(process > 0.0);
}

test_that("choice probabilities integrate the latent utilities out") {
  arma::vec systematic = {0.5};
  arma::mat covariance = arma::eye(1, 1);
  arma::uvec available = arma::ones<arma::uvec>(2);
  const double binary = log_probability_choice(
    1, systematic, covariance, available, false, false, arma::vec(),
    arma::mat()
  );
  expect_true(std::abs(binary - R::pnorm(0.5, 0.0, 1.0, 1, 1)) < 1e-10);
  arma::vec zero = {0.0};
  arma::vec thresholds = {
    -std::numeric_limits<double>::infinity(), 0.0,
    std::numeric_limits<double>::infinity()
  };
  const double ordered = log_probability_choice(
    1, zero, covariance, arma::uvec(), true, false, thresholds, arma::mat()
  );
  expect_true(std::abs(ordered - std::log(0.5)) < 1e-10);
}

test_that("the mixture log-likelihood evaluates the class densities") {
  arma::mat beta = {{-1.0, 1.0}};
  arma::mat means = {{0.0, 2.0}};
  arma::mat covariances = {{1.0, 4.0}};
  arma::mat loglik = log_likelihood_mixture(
    beta, means, covariances, arma::mat(0, 2), ClassLogLikelihood(), 2, 2
  );
  expect_true(loglik.n_rows == 2);
  expect_true(loglik.n_cols == 2);
  expect_true(std::abs(loglik(0, 0) - R::dnorm(-1.0, 0.0, 1.0, 1)) < 1e-10);
  expect_true(std::abs(loglik(1, 1) - R::dnorm(1.0, 2.0, 2.0, 1)) < 1e-10);
}

test_that("the random coefficient likelihood is a quadratic form") {
  arma::vec latent = {0.25};
  arma::mat design(1, 1);
  design(0, 0) = 2.0;
  arma::mat target(1, 1);
  target(0, 0) = 1.0;
  arma::mat precision = arma::eye(1, 1);
  arma::ivec normal = {0};
  const double value = log_likelihood_random_effect(
    latent, normal, design, target, precision
  );
  expect_true(std::abs(value + 0.5 * std::pow(1.0 - 0.5, 2)) < 1e-12);
  arma::ivec lognormal = {1};
  const double transformed = log_likelihood_random_effect(
    latent, lognormal, design, target, precision
  );
  const double residual = 1.0 - 2.0 * std::exp(0.25);
  expect_true(std::abs(transformed + 0.5 * residual * residual) < 1e-12);
}

test_that("class allocations follow the weights and the likelihoods") {
  Rcpp::RNGScope scope;
  arma::vec weights = {1.0, 0.0};
  arma::mat loglik(3, 2, arma::fill::zeros);
  arma::vec allocation = {1.0, 2.0, 2.0};
  arma::vec update = allocate_classes(weights, loglik, allocation, false);
  expect_true(update.n_elem == 3);
  expect_true(arma::all(update == 1.0));
  arma::vec kept = allocate_classes(weights, loglik, allocation, true);
  expect_true(kept[0] == 1.0);
  expect_true(kept[2] == 2.0);
}

}
