#include <testthat.h>
#include "gibbs_sampler.h"

context("gibbs_sampler") {

test_that("allocation can be sampled") {
  arma::vec p(3);
  p[0] = 0.5;
  p[1] = 0.3;
  p[2] = 0.2;
  int C = sample_allocation(p);
  expect_true(C >= 1);
  expect_true(C <= 3);
}

test_that("concentration can be updated") {
  int delta = 1;
  arma::vec m(3);
  m[0] = 10;
  m[1] = 8;
  m[2] = 5;
  arma::vec s_update = update_s(delta, m);
  expect_true(s_update.size() == 3);
}

test_that("class allocation can be updated") {
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

test_that("class sizes can be updated") {
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

test_that("class means can be updated") {
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

test_that("class covariances can be updated") {
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

test_that("coefficient vector can be updated") {
  arma::vec beta_true = { -1.0, 1.0 };
  beta_true(0) = -1.0;
  beta_true(1) =  1.0;
  arma::mat Sigma(3, 3);
  Sigma(0,0) = 1.0; Sigma(0,1) = 0.5; Sigma(0,2) = 0.2;
  Sigma(1,0) = 0.5; Sigma(1,1) = 1.0; Sigma(1,2) = 0.2;
  Sigma(2,0) = 0.2; Sigma(2,1) = 0.2; Sigma(2,2) = 2.0;
  const int N = 100;
  arma::mat Sigma_inv = arma::inv(Sigma);
  arma::mat XSigX(2, 2, arma::fill::zeros);
  arma::vec XSigU(2, arma::fill::zeros);
  for (int i = 0; i < N; ++i) {
    arma::mat X_i(3, 2);
    for (arma::uword k = 0; k < X_i.n_elem; ++k) {
      X_i[k] = R::rnorm(0.0, 1.0);
    }
    arma::vec mean3(3, arma::fill::zeros);
    arma::vec eps_i = oeli::rmvnorm(mean3, Sigma, false);
    arma::vec U_i = X_i * beta_true + eps_i;
    XSigX += X_i.t() * Sigma_inv * X_i;
    XSigU += X_i.t() * Sigma_inv * U_i;
  }
  arma::vec mu_beta_0(2, arma::fill::zeros);
  arma::mat Sigma_beta_0_inv = arma::eye<arma::mat>(2, 2);
  const int R = 10;
  arma::mat beta_draws(2, R);
  for (int r = 0; r < R; ++r) {
    beta_draws.col(r) = update_coefficient(
      mu_beta_0, Sigma_beta_0_inv, XSigX, XSigU);
  }
  expect_true(beta_draws.n_rows == 2);
  expect_true(beta_draws.n_cols == 10);
}

test_that("error covariance matrix can be updated") {
  arma::mat Sigma_true(3, 3);
  Sigma_true(0,0) = 1.0; Sigma_true(0,1) = 0.5; Sigma_true(0,2) = 0.2;
  Sigma_true(1,0) = 0.5; Sigma_true(1,1) = 1.0; Sigma_true(1,2) = 0.2;
  Sigma_true(2,0) = 0.2; Sigma_true(2,1) = 0.2; Sigma_true(2,2) = 2.0;
  arma::vec beta(2);
  beta(0) = -1.0;
  beta(1) =  1.0;
  const int N = 100;
  std::vector<arma::mat> X_list;
  std::vector<arma::vec> U_list;
  X_list.reserve(N);
  U_list.reserve(N);
  for (int i = 0; i < N; ++i) {
    arma::mat X_i(3, 2);
    for (arma::uword k = 0; k < X_i.n_elem; ++k) {
      X_i[k] = R::rnorm(0.0, 1.0);
    }
    arma::vec mean3(3, arma::fill::zeros);
    arma::vec eps_i = oeli::rmvnorm(mean3, Sigma_true, false);
    arma::vec U_i = X_i * beta + eps_i;
    X_list.push_back(X_i);
    U_list.push_back(U_i);
  }
  const int n_Sigma_0 = 4;
  arma::mat V_Sigma_0 = arma::eye<arma::mat>(3, 3);
  arma::mat S(3, 3, arma::fill::zeros);
  for (int i = 0; i < N; ++i) {
    const arma::mat& X_i = X_list[static_cast<std::size_t>(i)];
    const arma::vec& U_i = U_list[static_cast<std::size_t>(i)];
    arma::vec resid = U_i - X_i * beta;
    S += resid * resid.t();
  }
  const int R = 100;
  arma::cube Sigma_draws(3, 3, R);
  for (int r = 0; r < R; ++r) {
    Sigma_draws.slice(static_cast<arma::uword>(r)) =
      update_Sigma(n_Sigma_0, V_Sigma_0, N, S);
  }
  arma::mat Sigma_mean = arma::mean(Sigma_draws, 2);
  expect_true(Sigma_draws.n_rows == 3);
  expect_true(Sigma_draws.n_cols == 3);
  expect_true(Sigma_draws.n_slices == static_cast<arma::uword>(R));
  expect_true(Sigma_mean.n_rows == 3);
  expect_true(Sigma_mean.n_cols == 3);
}

test_that("utility vector can be updated") {
  // multinomial case
  arma::vec U = arma::zeros<arma::vec>(3);
  arma::vec sys = arma::zeros<arma::vec>(3);
  arma::mat Sigma_inv = arma::eye<arma::mat>(3, 3);
  arma::vec U_sample = update_U(U, 1, sys, Sigma_inv);
  expect_true(U_sample.size() == 3);

  // ranked case
  arma::vec U_ranked_sample = update_U_ranked(U, sys, Sigma_inv);
  expect_true(U_ranked_sample.size() == 3);
}

test_that("threshold increments can be transformed to thresholds") {
  arma::vec d = arma::zeros<arma::vec>(3);
  arma::vec gamma = d_to_gamma(d);
  expect_true(gamma.size() == 6);
}

test_that("ordered probit log-likelihood value can be computed") {
  arma::vec d(3, arma::fill::zeros);
  arma::mat y(2, 2);
  y(0, 0) = 1.0;
  y(1, 0) = 2.0;
  y(0, 1) = 1.0;
  y(1, 1) = NA_REAL;
  arma::mat sys(2, 2);
  sys(0, 0) = 0.0;
  sys(1, 0) = 0.0;
  sys(0, 1) = 0.0;
  sys(1, 1) = NA_REAL;
  arma::vec Tvec(2);
  Tvec[0] = 2.0;
  Tvec[1] = 1.0;
  double ll = ll_ordered(d, y, sys, Tvec);
  expect_true(ll > -2.5);
  expect_true(ll < -2.4);
}

test_that("threshold increments can be updated") {
  const int N = 1000;
  arma::vec d_true = arma::randn<arma::vec>(2);
  arma::vec gamma(5);
  gamma[0] = -std::numeric_limits<double>::infinity();
  gamma[1] = 0.0;
  arma::vec inc = arma::exp(d_true);
  arma::vec cum = arma::cumsum(inc);
  gamma[2] = cum[0];
  gamma[3] = cum[1];
  gamma[4] = std::numeric_limits<double>::infinity();
  arma::mat X = arma::randn<arma::mat>(N, 2);
  arma::vec beta(2);
  beta[0] = 0.8;
  beta[1] = -0.5;
  arma::mat mu(N, 1);
  mu.col(0) = X * beta;
  arma::vec U(N);
  for (int i = 0; i < N; ++i) {
    U[i] = R::rnorm(mu(i, 0), 1.0);
  }
  arma::mat y(N, 1);
  const int K = static_cast<int>(gamma.n_elem) - 1;
  for (int i = 0; i < N; ++i) {
    const double u = U[i];
    int c = K;
    for (int k = 1; k <= K; ++k) {
      if (u <= gamma[static_cast<arma::uword>(k)]) {
        c = k;
        break;
      }
    }
    y(i, 0) = static_cast<double>(c);
  }
  arma::vec Tvec(N);
  Tvec.fill(1.0);
  arma::vec mu_d_0(2, arma::fill::zeros);
  arma::mat Sigma_d_0 = 5.0 * arma::eye<arma::mat>(2, 2);
  arma::vec d = arma::randn<arma::vec>(2);
  const arma::vec csum_init = arma::cumsum(arma::exp(d));
  const arma::vec csum_true = arma::cumsum(arma::exp(d_true));
  const double dist0 = arma::norm(csum_init - csum_true, 2);
  double ll = -std::numeric_limits<double>::infinity();
  const int R = 1000;
  for (int iter = 0; iter < R; ++iter) {
    Rcpp::List ans = update_d(d, y, mu, ll, mu_d_0, Sigma_d_0, Tvec);
    d  = Rcpp::as<arma::vec>(ans["d"]);
    ll = Rcpp::as<double>(ans["ll"]);
  }
  expect_true(std::isfinite(ll));
}

test_that("weight-based updating scheme works") {
  arma::vec s(2);
  s[0] = 0.7;
  s[1] = 0.3;
  arma::mat b = arma::mat(2, 2);
  b(0, 0) = 1;
  b(0, 1) = 1;
  b(1, 0) = 1;
  b(1, 1) = -1;
  arma::mat Omega(4, 2);
  Omega.col(0) = arma::vec({0.5, 0.3, 0.3, 0.5});
  Omega.col(1) = arma::vec({1.0, -0.1, -0.1, 0.8});

  // no update
  Rcpp::List update_no_update = update_classes_wb(s, b, Omega);
  arma::vec s_update_no_update = update_no_update["s"];
  expect_true(s_update_no_update.size() == 2);
  int update_type_no_update = update_no_update["update_type"];
  expect_true(update_type_no_update == 0);

  // remove class
  Rcpp::List update_remove = update_classes_wb(s, b, Omega, 0.31);
  arma::vec s_update_remove = update_remove["s"];
  expect_true(s_update_remove.size() == 1);
  int update_type_remove  = update_remove["update_type"];
  expect_true(update_type_remove  == 1);

  // split class
  Rcpp::List update_split = update_classes_wb(s, b, Omega, 0.01, 0.69);
  arma::vec s_update_split = update_split["s"];
  expect_true(s_update_split.size() == 3);
  int update_type_split  = update_split["update_type"];
  expect_true(update_type_split  == 2);

  // merge classes
  Rcpp::List update_merge = update_classes_wb(s, b, Omega, 0.01, 0.7, 3);
  arma::vec s_update_merge = update_merge["s"];
  expect_true(s_update_merge.size() == 1);
  int update_type_merge  = update_merge["update_type"];
  expect_true(update_type_merge  == 3);
}

test_that("Dirichlet process class updates can be computed") {
  const int P = 2;
  const int N = 20;
  arma::vec z(N);
  for (int i = 0; i < 10; ++i) z[i] = 1.0;
  for (int i = 10; i < N; ++i) z[i] = 2.0;
  arma::mat b(P, 2);
  b(0,0) = 5.0; b(1,0) =  5.0;
  b(0,1) = 5.0; b(1,1) = -5.0;
  arma::mat Omega(P*P, 2);
  Omega(0,0)=1.0;  Omega(1,0)=0.3;  Omega(2,0)=0.3;  Omega(3,0)=0.5;
  Omega(0,1)=1.0;  Omega(1,1)=-0.3; Omega(2,1)=-0.3; Omega(3,1)=0.8;
  arma::mat beta(P, N);
  for (int n = 0; n < N; ++n) {
    int zi = static_cast<int>(z[n]) - 1; // 0-based
    arma::vec mean = b.col(zi);
    arma::mat Sig  = arma::reshape(Omega.col(zi), P, P);
    beta.col(n) = oeli::rmvnorm(mean, Sig);
  }
  beta(0, 0) = -10.0;
  beta(1, 0) = 10.0;
  double delta = 1.0;
  arma::vec mu_b_0(P, arma::fill::zeros);
  arma::mat Sigma_b_0 = arma::eye<arma::mat>(P, P);
  int n_Omega_0 = 4;
  arma::mat V_Omega_0 = arma::eye<arma::mat>(P, P);
  bool identify_classes = false;
  int Cmax = 10;
  Rcpp::List out = update_classes_dp(
    beta, z, b, Omega,
    delta, mu_b_0, Sigma_b_0, n_Omega_0, V_Omega_0,
    identify_classes, Cmax
  );
  arma::vec z_out = out["z"];
  arma::mat b_out = out["b"];
  arma::mat Omega_out = out["Omega"];
  int C_out = Rcpp::as<int>(out["C"]);
  expect_true(C_out >= 1);
  expect_true(C_out <= Cmax);
  expect_true(z_out.n_elem == static_cast<arma::uword>(N));
  expect_true(b_out.n_rows == static_cast<arma::uword>(P));
  expect_true(b_out.n_cols == static_cast<arma::uword>(C_out));
  expect_true(Omega_out.n_rows == static_cast<arma::uword>(P*P));
  expect_true(Omega_out.n_cols == static_cast<arma::uword>(C_out));
}

}
