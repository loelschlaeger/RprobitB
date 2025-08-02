#ifndef CLASS_UPDATE_H
#define CLASS_UPDATE_H

#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
#include <oeli.h>
using namespace arma;
using namespace Rcpp;

Rcpp::List update_classes_wb (double epsmin, double epsmax, double deltamin,
                              arma::vec s, arma::mat b, arma::mat Omega, int Cmax);

Rcpp::List update_classes_dp (int Cmax, arma::mat beta, arma::vec z, arma::mat b,
                              arma::mat Omega, double delta, arma::vec xi, arma::mat D,
                              int nu, arma::mat Theta, bool s_desc);

#endif
