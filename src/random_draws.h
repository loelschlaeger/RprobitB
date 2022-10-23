#ifndef RANDOMDRAWS_H
#define RANDOMDRAWS_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
using namespace arma;
using namespace Rcpp;

double dexpr(double const& a);

double invCdfNorm(double const& a);

double dnr(double const& a);

double trunNormBelow(double const& a);

double rtnorm(double mu, double sig, double trunpt, bool above);

double rttnorm(double mu, double sig, double lower_bound, double upper_bound);

arma::vec rdirichlet(arma::vec delta);

List rwishart(double nu, arma::mat const& V);

arma::vec rmvnorm(arma::vec mu, arma::mat const& Sigma);

#endif
