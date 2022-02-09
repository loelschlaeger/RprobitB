#ifndef CLASS_UPDATE_H
#define CLASS_UPDATE_H

#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
using namespace arma;
using namespace Rcpp;

double euc_dist (arma::vec a, arma::vec b);

Rcpp::List update_classes_wb (int Cmax, double epsmin, double epsmax, double distmin,
                              arma::vec s, arma::mat b, arma::mat Omega);

#endif
