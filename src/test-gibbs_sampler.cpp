#include <testthat.h>
#include "gibbs_sampler.h"

context("update_s") {

  test_that("concentration s can be updated") {
    int delta = 1;
    arma::vec m(3);
    m[0] = 10;
    m[1] = 8;
    m[2] = 5;
    arma::vec s_update = update_s(delta, m);
    expect_true(s_update.size() == 3);
  }

}

context("update_z") {

  test_that("class allocation z can be updated") {
    arma::vec s(2);
    s[0] = 0.5;
    s[1] = 0.5;
    arma::mat beta = arma::mat(1, 3);
    beta(0, 0) = -2;
    beta(0, 1) =  0;
    beta(0, 2) =  2;
    arma::mat b = arma::mat(1, 2);
    b(0, 0) = 0;
    b(0, 1) = 1;
    arma::mat Omega = arma::mat(1, 2);
    Omega(0, 0) = 1;
    Omega(0, 1) = 1;
    arma::vec z_update = update_z(s, beta, b, Omega);
    expect_true(z_update.size() == 3);
  }

}

context("update_m") {

  test_that("class sizes m can be updated") {

    // TODO

  }

}

context("update_classes_wb") {

  test_that("weight-based updating scheme works") {
    arma::vec s(2);
    s[0] = 0.8;
    s[1] = 0.2;
    arma::mat b = arma::mat(2, 2);
    b(0, 0) =  1;
    b(0, 1) =  1;
    b(1, 0) =  1;
    b(1, 1) = -1;
    arma::mat Omega(4, 2);
    Omega.col(0) = arma::vec({0.5, 0.3, 0.3, 0.5});
    Omega.col(1) = arma::vec({1.0, -0.1, -0.1, 0.8});

    // remove class
    Rcpp::List update_remove = update_classes_wb(
      0.3, 0.9, 1, s, b, Omega, 10, true
    );
    arma::vec s_update_remove = update_remove["s"];
    expect_true(s_update_remove.size() == 1);

    // split class
    Rcpp::List update_split = update_classes_wb(
      0.1, 0.7, 1, s, b, Omega
    );
    arma::vec s_update_split = update_split["s"];
    expect_true(s_update_split.size() == 3);

    // merge classes
    Rcpp::List update_merge = update_classes_wb(
      0.1, 0.9, 3, s, b, Omega
    );
    arma::vec s_update_merge = update_merge["s"];
    expect_true(s_update_merge.size() == 1);
  }

}
