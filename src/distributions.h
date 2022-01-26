#ifndef DISTRIBUTIONS_H
#define DISTRIBUTIONS_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <Rcpp.h>
#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
#include <time.h>
using namespace arma;
using namespace Rcpp;

double dexpr(double const& a);
double invCdfNorm(double const& a);
double dnr(double const& a);
double trunNormBelow(double const& a);
double trunNorm(double mu, double sig, double trunpt, bool above);
double mvnpdf(arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma);
arma::vec rdirichlet(arma::vec alpha);
List rwishart(double nu, arma::mat const& V);

#endif
