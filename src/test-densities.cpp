#include <testthat.h>
#include "densities.h"

context("densities") {

  test_that("Dirichlet density can be computed") {
    arma::vec x = arma::zeros<arma::vec>(3);
    x(0) = 0.5; x(1) = 0.3; x(2) = 0.2;
    arma::vec concentration = arma::zeros<arma::vec>(3);
    concentration(0) = 1; concentration(1) = 2; concentration(2) = 3;
    double density = ddirichlet_cpp(x, concentration, false);
    int factor = 1000;
    double density_rounded = round(density * factor) / factor;
    expect_true(density_rounded == 0.720);
    double density_log = ddirichlet_cpp(x, concentration, true);
    double density_log_rounded = round(density_log * factor) / factor;
    expect_true(density_log_rounded == -0.329);
  }

  test_that("multivariate normal density can be computed") {
    arma::vec x = arma::zeros<arma::vec>(2);
    arma::vec mean = arma::zeros<arma::vec>(2);
    arma::mat Sigma = arma::eye<arma::mat>(2,2);
    double density = dmvnorm_cpp(x, mean, Sigma, false);
    int factor = 1000;
    double density_rounded = round(density * factor) / factor;
    expect_true(density_rounded == 0.159);
    double density_log = dmvnorm_cpp(x, mean, Sigma, true);
    double density_log_rounded = round(density_log * factor) / factor;
    expect_true(density_log_rounded == -1.838);
  }

  test_that("univariate normal density can be computed") {
    arma::vec x = arma::zeros<arma::vec>(1);
    arma::vec mean = arma::zeros<arma::vec>(1);
    arma::mat Sigma = arma::eye<arma::mat>(1,1);
    double density = dmvnorm_cpp(x, mean, Sigma, false);
    int factor = 1000;
    double density_rounded = round(density * factor) / factor;
    expect_true(density_rounded == 0.399);
    double density_log = dmvnorm_cpp(x, mean, Sigma, true);
    double density_log_rounded = round(density_log * factor) / factor;
    expect_true(density_log_rounded == -0.919);
  }

  test_that("Wishart density can be computed") {
    arma::mat x = arma::eye<arma::mat>(2,2);
    int df = 4;
    arma::mat scale = arma::eye<arma::mat>(2,2);
    scale(1) = 0.2; scale(2) = 0.2; scale(3) = 2.0;
    double density = dwishart_cpp(x, df, scale, false, false);
    int factor = 1000;
    double density_rounded = round(density * factor) / factor;
    expect_true(density_rounded == 0.005);
    double density_log = dwishart_cpp(x, df, scale, true, false);
    double density_log_rounded = round(density_log * factor) / factor;
    expect_true(density_log_rounded == -5.335);
  }

  test_that("Inverse-Wishart density can be computed") {
    arma::mat x = arma::eye<arma::mat>(2,2);
    int df = 4;
    arma::mat scale = arma::eye<arma::mat>(2,2);
    scale(1) = 0.2; scale(2) = 0.2; scale(3) = 2.0;
    double density = dwishart_cpp(x, df, scale, false, true);
    int factor = 1000;
    double density_rounded = round(density * factor) / factor;
    expect_true(density_rounded == 0.034);
    double density_log = dwishart_cpp(x, df, scale, true, true);
    double density_log_rounded = round(density_log * factor) / factor;
    expect_true(density_log_rounded == -3.378);
  }

}
