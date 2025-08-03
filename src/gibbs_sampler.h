#ifndef GIBBS_SAMPLER_H
#define GIBBS_SAMPLER_H

#include <RcppArmadillo.h>
#include <Rmath.h>
#include <oeli.h>

arma::vec update_s (
    int delta, arma::vec m
);

arma::vec update_z (
    arma::vec s, arma::mat beta, arma::mat b, arma::mat Omega
);

arma::vec update_m (
    int C, arma::vec z, bool non_zero = false
);

arma::mat update_b (
    arma::mat beta, arma::mat Omega, arma::vec z, arma::vec m,
    arma::vec xi, arma::mat Dinv
);

arma::mat update_Omega (
    arma::mat beta, arma::mat b, arma::vec z, arma::vec m,
    int nu, arma::mat Theta
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
    double delta, arma::vec xi, arma::mat D, int nu, arma::mat Theta,
    int Cmax = 10, bool identify_classes = false
);

Rcpp::List update_classes_dp2 (int Cmax, arma::mat beta, arma::vec z, arma::mat b, arma::mat Omega,
                               double delta, arma::vec xi, arma::mat D, int nu, arma::mat Theta,
                               bool s_desc = true);

Rcpp::List gibbs_sampler (
    Rcpp::List sufficient_statistics, Rcpp::List prior,
    Rcpp::List latent_classes, Rcpp::List init,
    int R, int B, bool print_progress
);

#endif
