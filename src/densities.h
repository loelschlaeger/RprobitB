#ifndef DENSITIES_H
#define DENSITIES_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

double dmvnorm(arma::vec const& x, arma::vec const& mean,
               arma::mat const& Sigma, bool log = false);

#endif
