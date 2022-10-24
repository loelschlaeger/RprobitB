#include <testthat.h>
#include "random_draws.h"

context("random_draws") {

  test_that("Dirichlet can be drawn") {
    arma::vec concentration = arma::zeros<arma::vec>(3);
    concentration(0) = 1; concentration(1) = 2; concentration(2) = 3;
    arma::vec draw = rdirichlet_cpp(concentration);
    expect_true(draw.size() == 3);
    expect_true(abs(sum(draw) - 1) < 0.001);
  }

  test_that("Multivariate normal can be drawn") {
    arma::vec mean = arma::zeros<arma::vec>(2);
    arma::mat Sigma = arma::eye<arma::mat>(2,2);
    arma::vec draw = rmvnorm_cpp(mean, Sigma, false);
    expect_true(draw.size() == 2);
    arma::vec draw_log = rmvnorm_cpp(mean, Sigma, true);
    expect_true(draw_log.size() == 2);
    expect_true(draw_log(0) > 0);
    expect_true(draw_log(1) > 0);
  }

  test_that("Univariate truncated normal can be drawn") {
    double mean = 0.0;
    double sd = 1.0;
    double point = 0.0;
    double draw_tb = rtnorm_cpp(mean, sd, point, false, false);
    expect_true(draw_tb > 0.0);
    double draw_ta = rtnorm_cpp(mean, sd, point, true, false);
    expect_true(draw_ta < 0.0);
    double lower = -1.0;
    double upper = 1.0;
    double draw = rttnorm_cpp(mean, sd, lower, upper);
    expect_true(draw > -1.0);
    expect_true(draw < 1.0);
  }

  test_that("(Inverse-) Wishart can be drawn") {
    double df = 3.0;
    arma::mat scale = arma::eye<arma::mat>(2,2);
    arma::mat draw = rwishart_cpp(df, scale, false);
    expect_true(draw.n_rows == 2);
    expect_true(draw.n_cols == 2);
    arma::mat draw_log = rwishart_cpp(df, scale, true);
    expect_true(draw_log.n_rows == 2);
    expect_true(draw_log.n_cols == 2);
  }

}
