#include <testthat.h>
#include "densities.h"

context("densities") {

  test_that("multivariate normal density can be computed") {
    arma::vec x = arma::zeros<arma::vec>(2);
    arma::vec mean = arma::zeros<arma::vec>(2);
    arma::mat Sigma = arma::eye<arma::mat>(2,2);
    double density = dmvnorm(x, mean, Sigma);
    int factor = 1000;
    double density_rounded = round(density * factor) / factor;
    expect_true(density_rounded == 0.159);
    bool log = true;
    double density_log = dmvnorm(x, mean, Sigma, log);
    double density_log_rounded = round(density_log * factor) / factor;
    expect_true(density_log_rounded == -1.838);
  }

}
