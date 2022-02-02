#ifndef TIMER_H
#define TIMER_H

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

void start_timer();

void update_timer(int rep, int R);

void end_timer(int R);

#endif
