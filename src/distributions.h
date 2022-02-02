#ifndef DISTRIBUTIONS_H
#define DISTRIBUTIONS_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <Rcpp.h>
#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
using namespace arma;
using namespace Rcpp;

double dmvnorm(arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma, bool log = false);

arma::vec rdirichlet(arma::vec alpha);

List rwishart(double nu, arma::mat const& V);

#endif
