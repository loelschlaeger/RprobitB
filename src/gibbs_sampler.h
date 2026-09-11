#ifndef GIBBS_SAMPLER_H
#define GIBBS_SAMPLER_H

#include <oeli.h>
#include <RcppArmadillo.h>
#include <Rmath.h>
#include <functional>

typedef std::function<double(int, const arma::vec&)> ClassLogLikelihood;

arma::mat inv_spd(
    arma::mat const& x
);

arma::vec transform_random_effects(
    arma::vec const& latent, arma::ivec const& distribution
);

double log_density_delta(
    double delta, arma::vec const& s, double shape, double rate
);

double log_probability_choice(
    int y, arma::vec const& sys, arma::mat const& Sigma,
    arma::uvec const& available, bool ordered, bool ranked,
    arma::vec const& gamma, arma::mat const& rdiff
);

arma::mat log_likelihood_mixture(
    arma::mat const& beta, arma::mat const& b, arma::mat const& Omega,
    arma::mat const& lambda, ClassLogLikelihood const& log_likelihood_class,
    int N, int C
);

double log_likelihood_random_effect(
    arma::vec const& latent, arma::ivec const& distribution,
    arma::mat const& X, arma::mat const& target, arma::mat const& Sigma_inv
);

double update_delta_finite(
    double delta, arma::vec const& s, double shape, double rate,
    double step_scale = 0.2
);

double update_delta_dp(
    double delta, int N, int C, double shape, double rate
);

arma::vec allocate_classes(
    arma::vec const& s, arma::mat const& loglik, arma::vec z, bool nonempty
);

#endif
