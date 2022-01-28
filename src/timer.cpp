// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
#include <time.h>
using namespace arma;
using namespace Rcpp;

// TRACK COMPUTATION TIME
time_t itime;

void start_timer() {
  // Function to initialize the timer for Gibbs sampling
  itime = time(NULL);
  Rcout << "Iteration Info                   ETA (min)\n";
  Rcout << "        0 started Gibbs sampling          \n";
}

void update_timer(int rep, int R) {
  // Function to update the timer for Gibbs sampling
  time_t ctime = time(NULL);
  char buf[50];
  double time_to_end = difftime(ctime, itime) / 60.0 * (R - rep - 1) / (rep+1);
  sprintf(buf, "%9d                        %9.0f\n", rep+1, ceil(time_to_end));
  Rcout <<  buf;
}

void end_timer(int R) {
  // Function to terminate the timer for Gibbs sampling
  time_t ctime = time(NULL);
  char buf[50];
  sprintf(buf, "%9d done, total time: %.0f min\n", R,
          ceil(difftime(ctime, itime)/60.0));
  Rcout << buf;
  itime = 0;
}
