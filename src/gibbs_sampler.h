#ifndef GIBBS_SAMPLER_H
#define GIBBS_SAMPLER_H

#include <oeli.h>
#include <RcppArmadillo.h>
#include <Rmath.h>

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

arma::vec update_reg (
    arma::vec mu0, arma::mat Tau0, arma::mat XSigX, arma::vec XSigU
);

arma::mat update_Sigma (
    int kappa, arma::mat E, int N, arma::mat S
);

arma::vec update_U (
    arma::vec U, int y, arma::vec sys, arma::mat Sigmainv
);

arma::vec d_to_gamma (
    arma::vec d
);

double ll_ordered (
    arma::vec d, arma::mat y, arma::mat mu, arma::vec Tvec, arma::vec csTvec
);

Rcpp::List update_d (
    arma::vec d, int y, double mu, double ll, arma::vec zeta, arma::mat Z,
    arma::vec Tvec, arma::vec csTvec
);

Rcpp::List update_classes_wb (
    double epsmin, double epsmax, double deltamin,
    arma::vec s, arma::mat b, arma::mat Omega,
    int Cmax = 10, bool identify_classes = false
);

Rcpp::List update_classes_dp (
  arma::mat beta, arma::vec z, arma::mat b, arma::mat Omega,
  double delta, arma::vec mu_b_0, arma::mat Sigma_b_0, int n_Omega_0,
  arma::mat V_Omega_0, bool identify_classes = false, int Cmax = 10
);

Rcpp::List gibbs_sampler (
    Rcpp::List sufficient_statistics, Rcpp::List prior,
    Rcpp::List latent_classes, Rcpp::List init,
    int R, int B, bool print_progress
);

#endif
