#ifndef RANDOMDRAWS_H
#define RANDOMDRAWS_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

arma::vec rdirichlet_cpp(arma::vec concentration);

arma::vec rmvnorm_cpp(arma::vec mean, arma::mat const& Sigma, bool log = false);

double rtnorm_cpp(double mean, double sig, double point, bool above, bool log = false);

double rttnorm_cpp(double mean, double sig, double lower, double upper, bool log = false);

arma::mat rwishart_cpp(double df, arma::mat const& scale, bool inv = false);

#endif
