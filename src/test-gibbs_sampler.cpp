#include <testthat.h>
#include "gibbs_sampler.h"

context("gibbs_sampler") {

test_that("concentration s can be updated") {
  int delta = 1;
  arma::vec m(3);
  m[0] = 10;
  m[1] = 8;
  m[2] = 5;
  arma::vec s_update = update_s(delta, m);
  expect_true(s_update.size() == 3);
}

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

test_that("class sizes m can be updated") {
  int C = 4;
  arma::vec z(6);
  z(0) = 1;
  z(1) = 1;
  z(2) = 1;
  z(3) = 2;
  z(4) = 2;
  z(5) = 3;
  arma::vec m_update = update_m(C, z);
  expect_true(m_update.n_elem == 4);
  expect_true(
    arma::approx_equal(m_update, arma::vec({3, 2, 1, 0}), "absdiff", 1e-8)
  );
  arma::vec m_update2 = update_m(C, z, true);
  expect_true(m_update2.n_elem == 4);
  expect_true(
    arma::approx_equal(m_update2, arma::vec({3, 2, 1, 1}), "absdiff", 1e-8)
  );
}

test_that("class means b can be updated") {
  // single class
  arma::mat Omega_c = arma::eye<arma::mat>(2, 2);
  arma::vec bar_b_c = arma::vec({0, 0});
  int m_c = 10;
  arma::mat Sigma_b_0_inv = arma::eye<arma::mat>(2, 2);
  arma::vec mu_b_0 = arma::vec({0, 0});
  arma::vec b_c_update = update_b_c(
    bar_b_c, Omega_c, m_c, Sigma_b_0_inv, mu_b_0
  );
  expect_true(b_c_update.n_elem == 2);

  // multiple classes
  int N = 100;
  arma::mat b(2, 2);
  b.col(0) = arma::vec({0, 0});
  b.col(1) = arma::vec({1, 1});
  arma::mat Omega(4, 2);
  Omega.col(0) = arma::vec({1, 0.3, 0.3, 0.5});
  Omega.col(1) = arma::vec({1, -0.3, -0.3, 0.8});
  arma::vec z(N);
  z.subvec(0, N / 2 - 1).fill(1);
  z.subvec(N / 2, N - 1).fill(2);
  arma::vec m(2);
  m(0) = 50;
  m(1) = 50;
  arma::mat beta = arma::eye<arma::mat>(2, N);
  for (int n = 0; n < N; n++) {
    arma::vec mean = b.col(z(n) - 1);
    arma::mat cov = arma::reshape(Omega.col(z(n) - 1), 2, 2);
    beta.col(n) = oeli::rmvnorm(mean, cov);
  }
  arma::mat b_update = update_b(
    beta, Omega, z, m, arma::eye<arma::mat>(2, 2), arma::vec({0, 0})
  );
  expect_true(b_update.n_rows == 2);
  expect_true(b_update.n_cols == 2);
}

test_that("class covariances Omega can be updated") {
  // single class
  arma::mat S_c = arma::eye<arma::mat>(2, 2);
  int m_c = 10;
  arma::mat Omega_c_update = update_Omega_c(
    S_c, m_c, 4, arma::eye<arma::mat>(2, 2)
  );
  expect_true(Omega_c_update.n_rows == 2);
  expect_true(Omega_c_update.n_cols == 2);

  // multiple classes
  int N = 100;
  arma::mat b(2, 2);
  b.col(0) = arma::vec({0, 0});
  b.col(1) = arma::vec({1, 1});
  arma::mat Omega(4, 2);
  Omega.col(0) = arma::vec({1, 0.3, 0.3, 0.5});
  Omega.col(1) = arma::vec({1, -0.3, -0.3, 0.8});
  arma::vec z(N);
  z.subvec(0, N / 2 - 1).fill(1);
  z.subvec(N / 2, N - 1).fill(2);
  arma::vec m(2);
  m(0) = 50;
  m(1) = 50;
  arma::mat beta = arma::eye<arma::mat>(2, N);
  for (int n = 0; n < N; n++) {
    arma::vec mean = b.col(z(n) - 1);
    arma::mat cov = arma::reshape(Omega.col(z(n) - 1), 2, 2);
    beta.col(n) = oeli::rmvnorm(mean, cov);
  }
  arma::mat Omega_update = update_Omega(
    beta, b, z, m, 4, arma::eye<arma::mat>(2, 2)
  );
  expect_true(Omega_update.n_rows == 4);
  expect_true(Omega_update.n_cols == 2);
}

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
