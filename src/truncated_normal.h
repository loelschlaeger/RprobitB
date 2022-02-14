#ifndef TRUNCATEDNORMAL_H
#define TRUNCATEDNORMAL_H

#include <Rcpp.h>

double dexpr(double const& a);

double invCdfNorm(double const& a);

double dnr(double const& a);

double trunNormBelow(double const& a);

double rtnorm(double mu, double sig, double trunpt, bool above);

#endif
