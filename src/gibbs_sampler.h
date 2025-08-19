#ifndef GIBBS_SAMPLER_H
#define GIBBS_SAMPLER_H

#include <algorithm>
#include <cmath>
#include <limits>
#include <oeli.h>
#include <RcppArmadillo.h>
#include <Rmath.h>
#include <unordered_map>
#include <vector>

int sample_allocation(
    arma::vec const& prob
);

arma::vec update_s (
    int delta, arma::vec m
);

arma::vec update_z (
    arma::vec s, arma::mat beta, arma::mat b, arma::mat Omega
);

arma::vec update_m (
    int C, arma::vec z, bool non_zero = false
);

arma::mat update_b_c (
    arma::vec bar_b_c, arma::mat Omega_c, int m_c,
    arma::mat Sigma_b_0_inv, arma::vec mu_b_0
);

arma::mat update_b (
    arma::mat beta, arma::mat Omega, arma::vec z, arma::vec m,
    arma::mat Sigma_b_0_inv, arma::vec mu_b_0
);

arma::mat update_Omega_c (
    arma::mat S_c, int m_c, int n_Omega_0, arma::mat V_Omega_0
);

arma::mat update_Omega (
    arma::mat beta, arma::mat b, arma::vec z, arma::vec m,
    int n_Omega_0, arma::mat V_Omega_0
);

arma::vec update_coefficient (
    arma::vec mu_beta_0, arma::mat Sigma_beta_0_inv,
    arma::mat XSigX, arma::vec XSigU
);

arma::mat update_Sigma (
    int n_Sigma_0, arma::mat V_Sigma_0, int N, arma::mat S
);

arma::vec update_U (
    arma::vec U, int y, arma::vec sys, arma::mat Sigma_inv
);

arma::vec update_U_ranked (
    arma::vec U, arma::vec sys, arma::mat Sigma_inv
);

arma::vec d_to_gamma (
    arma::vec const& d
);

double ll_ordered(
    arma::vec const& d, arma::mat const& y, arma::mat const& sys,
    arma::vec const& Tvec
);

Rcpp::List update_d(
    arma::vec d, arma::mat const& y, arma::mat const& sys, double ll,
    arma::vec const& mu_d_0, arma::mat const& Sigma_d_0, arma::vec const& Tvec,
    double step_scale = 0.1
);

Rcpp::List update_classes_wb (
    arma::vec s, arma::mat b, arma::mat Omega,
    double epsmin = 0.01, double epsmax = 0.7, double deltamin = 0.1,
    double deltashift = 0.5, bool identify_classes = false, int Cmax = 10
);

Rcpp::List update_classes_dp(
    arma::mat beta, arma::vec z, arma::mat b, arma::mat Omega,
    double delta, arma::vec mu_b_0, arma::mat Sigma_b_0, int n_Omega_0,
    arma::mat V_Omega_0, bool identify_classes = false, int Cmax = 10
);

Rcpp::List gibbs_sampler (
    Rcpp::List sufficient_statistics, Rcpp::List prior,
    Rcpp::List latent_classes, Rcpp::List fixed_parameter,
    int R, int B, bool print_progress, bool ordered, bool ranked
);

#endif
