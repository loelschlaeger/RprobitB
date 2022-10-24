#ifndef DENSITIES_H
#define DENSITIES_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

double ddirichlet_cpp(arma::vec const& x, arma::vec const& concentration, bool log = false);

double dmvnorm_cpp(arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma, bool log = false);

double dwishart_cpp(arma::mat const& x, int const& df, arma::mat const& scale, bool log = false, bool inv = false);

#endif
