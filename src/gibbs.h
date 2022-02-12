#ifndef GIBBS_H
#define GIBBS_H

#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
#include <time.h>
#include "timer.h"
#include "distributions.h"
#include "truncated_normal.h"
#include "class_update.h"
using namespace arma;
using namespace Rcpp;

arma::vec update_s (int delta, arma::vec m);

arma::vec update_z (arma::vec s, arma::mat beta, arma::mat b, arma::mat Omega);

arma::vec update_m (int C, arma::vec z, bool nozero = false);

arma::mat update_b (arma::mat beta, arma::mat Omega, arma::vec z, arma::vec m, arma::vec xi, arma::mat Dinv);

arma::mat update_Omega (arma::mat beta, arma::mat b, arma::vec z, arma::vec m, int nu, arma::mat Theta);

arma::vec update_reg (arma::vec mu0, arma::mat Tau0, arma::mat XSigX, arma::vec XSigU);

arma::mat update_Sigma (int kappa, arma::mat E, int N, arma::mat S);

arma::vec update_U (arma::vec U, int y, arma::vec sys, arma::mat Sigmainv);

List gibbs_sampling (List sufficient_statistics, List prior, List latent_classes, List init, int R, int B, bool print_progress);

#endif
