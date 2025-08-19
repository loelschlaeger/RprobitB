#ifndef ARMA_DONT_PRINT_ERRORS
#define ARMA_DONT_PRINT_ERRORS
#endif

// [[Rcpp::depends("RcppArmadillo")]]
#include <algorithm>
#include <cmath>
#include <limits>
#include <oeli.h>
#include <RcppArmadillo.h>
#include <Rmath.h>
#include <unordered_map>
#include <vector>

arma::mat safe_symmetrize (const arma::mat& M) {
  arma::mat A = M;
  A.elem(arma::find_nonfinite(A)).zeros();
  return 0.5 * (A + A.t());
}

arma::mat clamp_pd (const arma::mat& A, double floor = 1e-8) {
  arma::mat S = safe_symmetrize(A);
  arma::vec w; arma::mat V;
  if (!arma::eig_sym(w, V, S)) {
    arma::mat I = arma::eye(S.n_rows, S.n_cols);
    return floor * I;
  }
  for (arma::uword i = 0; i < w.n_elem; ++i) {
    if (!std::isfinite(w[i]) || w[i] < floor) w[i] = floor;
  }
  arma::mat out = V * arma::diagmat(w) * V.t();
  return safe_symmetrize(out);
}

arma::mat safe_inv_sympd (const arma::mat& M, double floor = 1e-8) {
  arma::mat A = clamp_pd(M, floor);
  arma::mat out;
  if (!arma::inv_sympd(out, A)) {
    out = arma::pinv(A);
  }
  return safe_symmetrize(out);
}

arma::mat safe_chol (const arma::mat& M, double floor = 1e-8) {
  arma::mat A = clamp_pd(M, floor);
  arma::mat R;
  if (!arma::chol(R, A)) {
    A = clamp_pd(A, std::max(floor, 1e-6));
    if (!arma::chol(R, A)) {
      R = arma::eye(A.n_rows, A.n_cols);
    }
  }
  return R;
}

//' Sample allocation
//'
//' @param prob \[`numeric(C)`\]\cr
//' The vector of class probabilities.
//'
//' @return
//' An integer \code{1:C}.
//'
//' @examples
//' sample_allocation(c(0.5, 0.3, 0.2))
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
int sample_allocation(arma::vec const& prob) {
  const arma::uword n = prob.n_elem;
  arma::vec p = prob;
  for (arma::uword i = 0; i < n; ++i) {
    double v = p[i];
    if (!std::isfinite(v) || v < 0.0) p[i] = 0.0;
  }
  double s = arma::accu(p);
  if (!(s > 0.0)) {
    p.fill(1.0 / static_cast<double>(n));
  } else {
    p /= s;
  }
  arma::vec cdf = arma::cumsum(p);
  cdf[n - 1] = 1.0;
  const double u = R::runif(0.0, 1.0);
  arma::uword idx = static_cast<arma::uword>(
    std::upper_bound(cdf.begin(), cdf.end(), u) - cdf.begin()
  );
  if (idx >= n) idx = n - 1;
  return static_cast<int>(idx + 1);
}

//' Update class weight vector
//'
//' @param m \[`numeric(C)`\]\cr
//' The vector of current class frequencies.
//'
//' @inheritParams check_prior
//'
//' @return
//' An update for \code{s}.
//'
//' @examples
//' update_s(delta = 1, m = 4:1)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
arma::vec update_s (int delta, arma::vec m) {
  return oeli::rdirichlet(delta * arma::ones(m.size()) + m);
}

//' Update class allocation vector
//'
//' @inheritParams RprobitB_parameter
//'
//' @return
//' An update for \code{z}.
//'
//' @examples
//' update_z(
//'   s = c(0.6, 0.4), beta = matrix(c(-2, 0, 2), ncol = 3),
//'   b = cbind(0, 1), Omega = cbind(1, 1)
//' )
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
arma::vec update_z (
   arma::vec s, arma::mat beta, arma::mat b, arma::mat Omega
) {
  const int N = beta.n_cols;
  const int C = s.size();
  const int P_r = b.n_rows;
  arma::vec z = arma::zeros<arma::vec>(N);
  arma::vec logp(C);
  std::vector<arma::vec> means(C);
  std::vector<arma::mat> covs(C);
  for (int c = 0; c < C; ++c) {
     means[c] = b.col(c);
    arma::mat Oc = clamp_pd(arma::reshape(Omega.col(c), P_r, P_r), 1e-10);
     covs[c]  = Oc;
    }
    for (int n = 0; n < N; ++n) {
     for (int c = 0; c < C; ++c) {
       const double logs = std::log(std::max(1e-300, static_cast<double>(s[c])));
       const double loglik = oeli::dmvnorm(beta.col(n), means[c], covs[c], true);
       logp[c] = logs + loglik;
     }
     const double m = logp.max();
     arma::vec p = arma::exp(logp - m);
     double ps = arma::accu(p);
     if (!std::isfinite(ps) || ps <= 0.0) {
       p.fill(1.0 / static_cast<double>(C));
     } else {
       p /= ps;
     }
     z[n] = sample_allocation(p);
  }
  return z;
}

//' Update class sizes
//'
//' @param non_zero \[`logical(1)`\]\cr
//' Enforce strictly positive values in \code{m} (for numerical stability)?
//'
//' @inheritParams RprobitB_parameter
//'
//' @return
//' An update for \code{m}.
//'
//' @examples
//' update_m(C = 4, z = c(1, 1, 1, 2, 2, 3))
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
arma::vec update_m (int C, arma::vec z, bool non_zero = false) {
  arma::vec m(C, arma::fill::zeros);
  const int N = z.size();
  for (int n = 0; n < N; ++n) {
    int lbl = static_cast<int>(std::lround(z[n])) - 1;
    if (lbl >= 0 && lbl < C) m[lbl] += 1.0;
  }
  if (non_zero) {
    for (int c = 0; c < C; ++c) if (m[c] == 0.0) m[c] = 1.0;
  }
  return m;
}

//' Update mean of a single class
//'
//' @param Omega_c \[`matrix(P_r, P_r)`\]\cr
//' The class covariance matrix.
//'
//' @param bar_b_c \[`numeric(P_r)`\]\cr
//' The average observation of this class.
//'
//' @param m_c \[`integer(1)`\]\cr
//' The number of observations in this class.
//'
//' @param Sigma_b_0_inv \[`matrix(P_r, P_r)`\]\cr
//' The prior precision of the class mean.
//'
//' @inheritParams check_prior
//'
//' @return
//' An update for \code{b_c}.
//'
//' @examples
//' update_b_c(
//'   bar_b_c = c(0, 0), Omega_c = diag(2), m_c = 10,
//'   Sigma_b_0_inv = diag(2), mu_b_0 = c(0, 0)
//' )
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
arma::mat update_b_c (
   arma::vec bar_b_c, arma::mat Omega_c, int m_c,
   arma::mat Sigma_b_0_inv, arma::vec mu_b_0
) {
  arma::mat Omega_c_inv = safe_inv_sympd(Omega_c);
  arma::mat Sigma_b_c = safe_inv_sympd(Sigma_b_0_inv + m_c * Omega_c_inv);
  arma::vec mu_b_c = Sigma_b_c *
   (Sigma_b_0_inv * mu_b_0 + m_c * Omega_c_inv * bar_b_c);
  return oeli::rmvnorm(mu_b_c, Sigma_b_c);
}

//' Update class means
//'
//' @inheritParams RprobitB_parameter
//' @inheritParams update_s
//' @inheritParams update_b_c
//' @inheritParams check_prior
//'
//' @return
//' A matrix of updated means for each class in columns.
//'
//' @examples
//' N <- 100
//' b <- cbind(c(0, 0), c(1, 1))
//' Omega <- matrix(c(1, 0.3, 0.3, 0.5, 1, -0.3, -0.3, 0.8), ncol = 2)
//' z <- c(rep(1, N / 2), rep(2, N / 2))
//' m <- as.numeric(table(z))
//' beta <- sapply(
//'   z, function(z) oeli::rmvnorm(n = 1, b[, z], matrix(Omega[, z], 2, 2))
//' )
//' update_b(
//'   beta = beta, Omega = Omega, z = z, m = m,
//'   Sigma_b_0_inv = diag(2), mu_b_0 = c(0, 0)
//' )
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
arma::mat update_b (
   arma::mat beta, arma::mat Omega, arma::vec z, arma::vec m,
   arma::mat Sigma_b_0_inv, arma::vec mu_b_0
) {
  int P_r = beta.n_rows;
  int C = m.size();
  int N = beta.n_cols;
  arma::mat b_draw = arma::zeros<arma::mat>(P_r, C);
  for (int c = 0; c < C; ++c) {
   arma::vec bar_b_c(P_r, arma::fill::zeros);
   for (int n = 0; n < N; ++n) {
     if (static_cast<int>(std::lround(z[n])) == c + 1) {
       bar_b_c += beta(arma::span::all, n);
     }
   }
   const int mc = static_cast<int>(std::lround(m[c]));
   if (mc <= 0) {
     b_draw(arma::span::all, c) = oeli::rmvnorm(
       mu_b_0, safe_inv_sympd(Sigma_b_0_inv)
     );
     continue;
   }
   bar_b_c /= static_cast<double>(mc);
   arma::mat Omega_c = arma::reshape(Omega(arma::span::all, c), P_r, P_r);
   b_draw(arma::span::all, c) = update_b_c(
     bar_b_c, Omega_c, mc, Sigma_b_0_inv, mu_b_0
   );
  }
  return b_draw;
}

//' Update covariance of a single class
//'
//' @param S_c \[`matrix(P_r, P_r)`\]\cr
//' The scatter matrix of this class.
//'
//' @inheritParams update_b_c
//' @inheritParams check_prior
//'
//' @return
//' An update for \code{Omega_c}.
//'
//' @examples
//' update_Omega_c(S_c = diag(2), m_c = 10, n_Omega_0 = 4, V_Omega_0 = diag(2))
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
arma::mat update_Omega_c (
    arma::mat S_c, int m_c, int n_Omega_0, arma::mat V_Omega_0
) {
  arma::mat scale = clamp_pd(V_Omega_0 + safe_symmetrize(S_c), 1e-8);
  const int P = static_cast<int>(scale.n_rows);
  const int dof = std::max(n_Omega_0 + std::max(m_c, 0), P + 1);
  arma::mat draw = oeli::rwishart(dof, scale, true);
  return clamp_pd(draw, 1e-8);
}

//' Update class covariances
//'
//' @inheritParams RprobitB_parameter
//' @inheritParams update_s
//' @inheritParams check_prior
//'
//' @return
//' A matrix of updated covariance matrices for each class in columns.
//'
//' @examples
//' N <- 100
//' b <- cbind(c(0, 0), c(1, 1))
//' Omega <- matrix(c(1, 0.3, 0.3, 0.5, 1, -0.3, -0.3, 0.8), ncol = 2)
//' z <- c(rep(1, N / 2), rep(2, N / 2))
//' m <- as.numeric(table(z))
//' beta <- sapply(
//'   z, function(z) oeli::rmvnorm(n = 1, b[, z], matrix(Omega[, z], 2, 2))
//' )
//' update_Omega(
//'   beta = beta, b = b, z = z, m = m,
//'   n_Omega_0 = 4, V_Omega_0 = diag(2)
//' )
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
arma::mat update_Omega (
   arma::mat beta, arma::mat b, arma::vec z, arma::vec m,
   int n_Omega_0, arma::mat V_Omega_0
) {
  int P_r = beta.n_rows;
  int C = m.size();
  int N = beta.n_cols;
  arma::mat Omega_draw = arma::zeros<arma::mat>(P_r * P_r, C);
  for (int c = 0; c < C; ++c) {
   const int mc = static_cast<int>(std::lround(m[c]));
   arma::mat S_c = arma::zeros<arma::mat>(P_r, P_r);
   for (int n = 0; n < N; ++n) {
     if (static_cast<int>(std::lround(z[n])) == c + 1) {
       arma::vec beta_centered = beta.col(n) - b.col(c);
       S_c += beta_centered * beta_centered.t();
     }
   }
   Omega_draw(arma::span::all, c) = arma::reshape(
     update_Omega_c(S_c, mc, n_Omega_0, V_Omega_0), P_r * P_r, 1
   );
  }
  return Omega_draw;
}

//' Update coefficient vector
//'
//' @param mu_beta_0 \[`numeric(P)`\]\cr
//' The prior mean for the coefficient vector,
//'
//' @param Sigma_beta_0_inv \[`matrix(P, P)`\]\cr
//' The prior precision for the coefficient vector.
//'
//' @param XSigX \[`matrix(P, P)`\]\cr
//' The matrix \eqn{\sum_{n=1}^N X_n'\Sigma^{-1}X_n}.
//'
//' @param XSigU \[`numeric(P)`\]\cr
//' The vector \eqn{\sum_{n=1}^N X_n'\Sigma^{-1}U_n}.
//'
//' @return
//' An update for the coefficient vector.
//'
//' @examples
//' beta_true <- matrix(c(-1, 1), ncol = 1)
//' Sigma <- matrix(c(1, 0.5, 0.2, 0.5, 1, 0.2, 0.2, 0.2, 2), ncol = 3)
//' N <- 100
//' X <- replicate(N, matrix(rnorm(6), ncol = 2), simplify = FALSE)
//' eps <- replicate(
//'   N, oeli::rmvnorm(n = 1, mean = c(0, 0, 0), Sigma = Sigma),
//'   simplify = FALSE
//' )
//' U <- mapply(
//'   function(X, eps) X %*% beta_true + eps, X, eps, SIMPLIFY = FALSE
//' )
//' mu_beta_0 <- c(0, 0)
//' Sigma_beta_0_inv <- diag(2)
//' XSigX <- Reduce(
//'   `+`, lapply(X, function(X) t(X) %*% solve(Sigma) %*% X)
//' )
//' XSigU <- Reduce(
//'   `+`, mapply(function(X, U) t(X) %*% solve(Sigma) %*% U, X, U,
//'   SIMPLIFY = FALSE)
//' )
//' R <- 10
//' beta_draws <- replicate(
//'   R, update_coefficient(mu_beta_0, Sigma_beta_0_inv, XSigX, XSigU),
//'   simplify = TRUE
//' )
//' rowMeans(beta_draws)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
arma::vec update_coefficient (
    arma::vec mu_beta_0, arma::mat Sigma_beta_0_inv,
    arma::mat XSigX, arma::vec XSigU
) {
  arma::mat prior_prec = clamp_pd(Sigma_beta_0_inv, 1e-12);
  arma::mat XSigX_clean = safe_symmetrize(XSigX);
  XSigX_clean.elem(arma::find_nonfinite(XSigX_clean)).zeros();
  arma::vec XSigU_clean = XSigU;
  XSigU_clean.elem(arma::find_nonfinite(XSigU_clean)).zeros();
  arma::mat Sigma_beta = safe_inv_sympd(prior_prec + XSigX_clean, 1e-12);
  arma::vec mu_beta    = Sigma_beta * (prior_prec * mu_beta_0 + XSigU_clean);
  return oeli::rmvnorm(mu_beta, clamp_pd(Sigma_beta, 1e-12));
}

//' Update error covariance matrix
//'
//' @param N \[`integer(1)`\]\cr
//' The sample size.
//'
//' @param S \[`matrix(J - 1, J - 1)`\]\cr
//' The sum over the outer products of the residuals
//' \eqn{(\epsilon_n)_{n = 1, \dots, N}}.
//'
//' @inheritParams check_prior
//'
//' @return
//' An update for \code{Sigma}.
//'
//' @examples
//' (Sigma_true <- matrix(c(1, 0.5, 0.2, 0.5, 1, 0.2, 0.2, 0.2, 2), ncol = 3))
//' beta <- matrix(c(-1, 1), ncol = 1)
//' N <- 100
//' X <- replicate(N, matrix(rnorm(6), ncol = 2), simplify = FALSE)
//' eps <- replicate(
//'   N, oeli::rmvnorm(n = 1, mean = c(0, 0, 0), Sigma = Sigma_true),
//'   simplify = FALSE
//' )
//' U <- mapply(function(X, eps) X %*% beta + eps, X, eps, SIMPLIFY = FALSE)
//' n_Sigma_0 <- 4
//' V_Sigma_0 <- diag(3)
//' outer_prod <- function(X, U) (U - X %*% beta) %*% t(U - X %*% beta)
//' S <- Reduce(`+`, mapply(
//'   function(X, U) (U - X %*% beta) %*% t(U - X %*% beta), X, U,
//'   SIMPLIFY = FALSE
//' ))
//' Sigma_draws <- replicate(100, update_Sigma(n_Sigma_0, V_Sigma_0, N, S))
//' apply(Sigma_draws, 1:2, mean)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
arma::mat update_Sigma (
   int n_Sigma_0, arma::mat V_Sigma_0, int N, arma::mat S
) {
 return oeli::rwishart(n_Sigma_0 + N, V_Sigma_0 + S, true);
}

//' Update utility vector
//'
//' @param U \[`numeric(J - 1)`\]\cr
//' The current utility vector.
//'
//' @param y \[`integer(1)`\]\cr
//' The index of the chosen alternative, from \code{1} to \code{J}.
//'
//' @param sys \[`numeric(J - 1)`\]\cr
//' The systematic utility.
//'
//' @param Sigma_inv \[`matrix(J - 1, J - 1)`\]\cr
//' The inverted error covariance matrix.
//'
//' @return
//' An update for (a single) \code{U}.
//'
//' @examples
//' U <- sys <- c(0, 0, 0)
//' Sigma_inv <- diag(3)
//' lapply(1:4, function(y) update_U(U, y, sys, Sigma_inv))
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
arma::vec update_U (
    arma::vec U, int y, arma::vec sys, arma::mat Sigma_inv
) {
  arma::mat Prec = clamp_pd(Sigma_inv, 1e-10);
  const int Jm1 = static_cast<int>(U.n_elem);
  arma::vec U_update = U;
  for (int i = 0; i < Jm1; ++i) {
    double bound = 0.0;
    for (int j = 0; j < Jm1; ++j) {
      if (j != i) {
        bound = std::max(bound, U_update[j]);
      }
    }
    const double sii = std::max(Prec(i, i), 1e-10);
    double m = 0.0;
    for (int k = 0; k < Jm1; ++k) if (k != i) {
      m += -(Prec(i, k) / sii) * (U_update[k] - sys[k]);
    }
    double sd = std::sqrt(1.0 / sii);
    if (!std::isfinite(sd) || sd <= 0.0) sd = 1.0;
    U_update[i] = oeli::rtnorm(sys[i] + m, sd, bound, y != (i + 1));
  }
  return U_update;
}

//' Update ranked utility vector
//'
//' @inheritParams update_U
//'
//' @return
//' An update for (a single) ranked \code{U}.
//'
//' @examples
//' U <- sys <- c(0, 0)
//' Sigma_inv <- diag(2)
//' update_U_ranked(U, sys, Sigma_inv)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
arma::vec update_U_ranked (
    arma::vec U, arma::vec sys, arma::mat Sigma_inv
) {
  arma::mat Prec = clamp_pd(Sigma_inv, 1e-10);
  const int Jm1 = static_cast<int>(U.n_elem);
  arma::vec U_update = U;
  for (int i = 0; i < Jm1; ++i) {
    const double sii = std::max(Prec(i, i), 1e-10);
    double m = 0.0;
    for (int k = 0; k < Jm1; ++k) if (k != i) {
      m += -(Prec(i, k) / sii) * (U_update[k] - sys[k]);
    }
    double sd = std::sqrt(1.0 / sii);
    if (!std::isfinite(sd) || sd <= 0.0) sd = 1.0;
    U_update[i] = oeli::rtnorm(sys[i] + m, sd, 0.0, true);
  }
  return U_update;
}

//' Transform increments to thresholds
//'
//' @param d \[`numeric(J - 2)`\]\cr
//' Threshold increments.
//'
//' @return
//' The threshold vector of length \code{J + 1}.
//'
//' @examples
//' d_to_gamma(c(0, 0, 0))
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
arma::vec d_to_gamma (arma::vec const& d) {
  const arma::uword K = d.n_elem;
  arma::vec gamma(K + 3);
  gamma[0] = -std::numeric_limits<double>::infinity();
  gamma[1] = 0.0;
  gamma.subvec(2, K + 1) = arma::cumsum(arma::exp(d));
  gamma[K + 2] = std::numeric_limits<double>::infinity();
  return gamma;
}

//' Compute ordered probit log-likelihood
//'
//' @param y \[`matrix(nrow = N, ncol = max(Tvec))`\]\cr
//' Choices \code{1,...,J} for each decider in each choice occasion.
//'
//' @param sys \[`matrix(nrow = N, ncol = max(Tvec))`\]\cr
//' Systematic utilties for each decider in each choice occasion.
//'
//' @param Tvec \[`integer(N)`\]\cr
//' Number of choice occasions per decider.
//'
//' @inheritParams d_to_gamma
//'
//' @return
//' The ordered probit log-likelihood value.
//'
//' @examples
//' d <- c(0, 0, 0)
//' y <- matrix(c(1, 2, 1, NA), ncol = 2)
//' sys <- matrix(c(0, 0, 0, NA), ncol = 2)
//' Tvec <- c(2, 1)
//' ll_ordered(d = d, y = y, sys = sys, Tvec = Tvec)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
double ll_ordered (
   arma::vec const& d, arma::mat const& y, arma::mat const& sys,
   arma::vec const& Tvec
) {
  const arma::vec gamma = d_to_gamma(d);
  const int N = static_cast<int>(Tvec.n_elem);
  double ll = 0.0;
  for (int n = 0; n < N; ++n) {
   const int Tn = static_cast<int>(
     std::lround(Tvec[static_cast<arma::uword>(n)])
   );
   for (int t = 0; t < Tn; ++t) {
     const int c = static_cast<int>(std::lround(y(n, t)));
     const double m  = sys(n, t);
     const double lb = gamma[static_cast<arma::uword>(c - 1)] - m;
     const double ub = gamma[static_cast<arma::uword>(c)] - m;
     double logp;
     if (std::isinf(lb) && lb < 0) {
       logp = R::pnorm(ub, 0.0, 1.0, 1, 1);
     } else if (std::isinf(ub) && ub > 0) {
       logp = R::pnorm(lb, 0.0, 1.0, 0, 1);
     } else {
       const double logFb = R::pnorm(ub, 0.0, 1.0, 1, 1);
       const double logFa = R::pnorm(lb, 0.0, 1.0, 1, 1);
       logp = logFb + std::log1p(-std::exp(logFa - logFb));
     }
     if (!std::isfinite(logp)) {
       logp = std::log(1e-300);
     }
     ll += logp;
   }
  }
  return ll;
}

//' Update utility threshold increments
//'
//' @param ll \[`numeric(1)`\]\cr
//' Current log-likelihood value.
//'
//' @param step_scale \[`numeric(1)`\]\cr
//' Scaling the variance for the Gaussian proposal.
//'
//' @inheritParams ll_ordered
//' @inheritParams check_prior
//'
//' @return
//' An update for \code{d}.
//'
//' @examples
//' set.seed(1)
//' N <- 1000
//' d_true <- rnorm(2)
//' gamma <- c(-Inf, 0, cumsum(exp(d_true)), Inf)
//' X <- matrix(rnorm(N * 2L), ncol = 2L)
//' beta <- c(0.8, -0.5)
//' mu <- matrix(as.vector(X %*% beta), ncol = 1L)
//' U <- rnorm(N, mean = mu[, 1], sd = 1)
//' yvec <- as.integer(cut(U, breaks = gamma, labels = FALSE))
//' y <- matrix(yvec, ncol = 1L)
//' Tvec <- rep(1, N)
//' mu_d_0 <- c(0, 0)
//' Sigma_d_0 <- diag(2) * 5
//' d <- rnorm(2)
//' ll <- -Inf
//' R <- 1000
//' for (iter in seq_len(R)) {
//'   ans <- update_d(
//'     d = d, y = y, sys = mu, ll = ll, mu_d_0 = mu_d_0, Sigma_d_0 = Sigma_d_0,
//'     Tvec = Tvec
//'   )
//'   d  <- ans$d
//'   ll <- ans$ll
//' }
//' cbind("true" = d_true, "sample" = d) |> round(2)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
Rcpp::List update_d (
   arma::vec d, arma::mat const& y, arma::mat const& sys, double ll,
   arma::vec const& mu_d_0, arma::mat const& Sigma_d_0, arma::vec const& Tvec,
   double step_scale = 0.1
) {
  const arma::uword K = d.n_elem;
  arma::vec step = step_scale * arma::randn<arma::vec>(K);
  arma::vec d_cand = d + step;
  double ll_cand = ll_ordered(d_cand, y, sys, Tvec);
  const double log_prior_curr = oeli::dmvnorm(d, mu_d_0, Sigma_d_0, true);
  const double log_prior_cand = oeli::dmvnorm(d_cand, mu_d_0, Sigma_d_0, true);
  const double log_alpha = (ll_cand - ll) + (log_prior_cand - log_prior_curr);
  if (log_alpha >= 0.0 || std::log(R::runif(0.0, 1.0)) <= log_alpha) {
    d  = std::move(d_cand);
    ll = ll_cand;
  }
  return Rcpp::List::create(Rcpp::Named("d") = d, Rcpp::Named("ll") = ll);
}

//' Weight-based class updates
//'
//' @param Cmax \[`integer(1)`\]\cr
//' The maximum number of classes, used to allocate space.
//'
//' @param epsmin \[`numeric(1)`\]\cr
//' The threshold weight for removing a class.
//'
//' @param epsmax \[`numeric(1)`\]\cr
//' The threshold weight for splitting a class.
//'
//' @param deltamin \[`numeric(1)`\]\cr
//' The threshold difference in class means for joining two classes.
//'
//' @param deltashift \[`numeric(1)`\]\cr
//' The scale for shifting the class means after a split.
//'
//' @param identify_classes \[`logical(1)`\]\cr
//' Identify classes by decreasing class weights?
//'
//' @inheritParams RprobitB_parameter
//'
//' @details
//' The following updating rules apply:
//'
//' * Class \eqn{c} is removed if \eqn{s_c < \epsilon_{min}}.
//' * Class \eqn{c} is split into two classes, if \eqn{s_c > \epsilon_{max}}.
//' * Two classes \eqn{c_1} and \eqn{c_2} are merged to one class, if
//'   \eqn{||b_{c_1} - b_{c_2}|| < \delta_{min}}.
//'
//' @examples
//' s <- c(0.7, 0.3)
//' b <- matrix(c(1, 1, 1, -1), ncol = 2)
//' Omega <- matrix(c(0.5, 0.3, 0.3, 0.5, 1, -0.1, -0.1, 0.8), ncol = 2)
//'
//' ### no update
//' update_classes_wb(s = s, b = b, Omega = Omega)
//'
//' ### remove class 2
//' update_classes_wb(s = s, b = b, Omega = Omega, epsmin = 0.31)
//'
//' ### split class 1
//' update_classes_wb(s = s, b = b, Omega = Omega, epsmax = 0.69)
//'
//' ### merge classes 1 and 2
//' update_classes_wb(s = s, b = b, Omega = Omega, deltamin = 3)
//'
//' @return
//' A list of updated values for \code{s}, \code{b}, and \code{Omega} and
//' the indicator \code{update_type} which signals the type of class update:
//'
//' - `0`: no update
//' - `1`: removed class
//' - `2`: split class
//' - `3`: merged classes
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
Rcpp::List update_classes_wb (
   arma::vec s, arma::mat b, arma::mat Omega,
   double epsmin = 0.01, double epsmax = 0.7, double deltamin = 0.1,
   double deltashift = 0.5, bool identify_classes = false, int Cmax = 10
) {

 int update_type = 0;
 int C = b.n_cols;
 int P = b.n_rows;
 arma::mat stack = arma::join_cols(arma::trans(s), arma::join_cols(b, Omega));

 // remove class
 int id_min = arma::index_min(stack(0, arma::span::all));
 if (C > 1 && stack(0, id_min) < epsmin) {
   C -= 1;
   stack.shed_col(id_min);
   stack.row(0) = stack.row(0) / arma::accu(stack.row(0));
   update_type = 1;
 }

 // split class
 if (update_type == 0 && C < Cmax) {
   int id_max = arma::index_max(stack(0, arma::span::all));
   if (stack(0, id_max) > epsmax) {
     arma::mat max_class_Omega = clamp_pd(
       arma::reshape(stack(arma::span(P+1, P+P*P), id_max), P, P), 1e-10
     );
     arma::vec eigval;
     arma::mat eigvec;
     arma::eig_sym(eigval, eigvec, max_class_Omega);
     arma::vec v = eigvec.col(P - 1);
     arma::vec mean_shift = deltashift * std::sqrt(eigval(P - 1)) * v;
     stack.insert_cols(id_max, stack(arma::span::all, id_max));
     stack(0, arma::span(id_max, id_max + 1)) /= 2;
     stack(arma::span(1, 1 + P - 1), id_max) += mean_shift;
     stack(arma::span(1, 1 + P - 1), id_max + 1) -= mean_shift;
     C += 1;
     update_type = 2;
   }
 }

 // merge classes
 if (update_type == 0 && C > 1) {
   arma::vec closest_classes = arma::zeros<arma::vec>(3);
   closest_classes(0) = std::numeric_limits<int>::max();
   for (int c1 = 0; c1 < C; ++c1) {
     for (int c2 = 0; c2 < c1; ++c2) {
       arma::vec bc1 = stack(arma::span(1, 1 + P - 1), c1);
       arma::vec bc2 = stack(arma::span(1, 1 + P - 1), c2);
       double euc_dist = arma::norm(bc1 - bc2, 2);
       if (euc_dist < closest_classes(0)) {
         closest_classes(0) = euc_dist;
         closest_classes(1) = c1;
         closest_classes(2) = c2;
       }
     }
   }
   if (closest_classes(0) < deltamin) {
     int c1 = closest_classes(1);
     int c2 = closest_classes(2);
     stack(0, c1) += stack(0, c2);
     stack(arma::span(1, 1 + P - 1), c1) +=
       stack(arma::span(1, 1 + P - 1), c2);
     stack(arma::span(1, 1 + P - 1), c1) /=2;
     stack(arma::span(1 + P, 1 + P + P * P - 1), c1) +=
       stack(arma::span(1 + P, 1 + P + P * P - 1), c2);
     stack(arma::span(1 + P, 1 + P + P * P - 1), c1) /= 2;
     stack.shed_col(c2);
     C -= 1;
     update_type = 3;
   }
 }

 // identify classes
 if (identify_classes) {
   stack = stack.cols(arma::sort_index(stack(0, arma::span::all), "descend"));
 }

 // return class updates
 return Rcpp::List::create(
   Rcpp::Named("s") = stack.row(0),
   Rcpp::Named("b") = stack.rows(arma::span(1, 1 + P - 1)),
   Rcpp::Named("Omega") = stack.rows(arma::span(P + 1, 1 + P + P * P - 1)),
   Rcpp::Named("update_type") = update_type
 );
}

//' Dirichlet process class updates
//'
//' @inheritParams RprobitB_parameter
//' @inheritParams check_prior
//' @inheritParams update_classes_wb
//'
//' @return
//' A list of updated values for \code{z}, \code{b}, \code{Omega}, and \code{C}.
//'
//' @examples
//' set.seed(1)
//' z <- c(rep(1, 10),rep(2, 10))
//' b <- matrix(c(5, 5, 5, -5), ncol = 2)
//' Omega <- matrix(c(1, 0.3, 0.3, 0.5, 1, -0.3, -0.3, 0.8), ncol = 2)
//' beta <- sapply(
//'   z, function(z) oeli::rmvnorm(n = 1, b[, z], matrix(Omega[, z], 2, 2))
//' )
//' beta[, 1] <- c(-10, 10)
//' update_classes_dp(
//'   beta = beta, z = z, b = b, Omega = Omega,
//'   delta = 1, mu_b_0 = numeric(2), Sigma_b_0 = diag(2),
//'   n_Omega_0 = 4, V_Omega_0 = diag(2)
//' )
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
Rcpp::List update_classes_dp(
   arma::mat beta, arma::vec z, arma::mat b, arma::mat Omega,
   double delta, arma::vec mu_b_0, arma::mat Sigma_b_0, int n_Omega_0,
   arma::mat V_Omega_0, bool identify_classes = false, int Cmax = 10
) {
  const int N = static_cast<int>(z.n_elem);
  int C = static_cast<int>(b.n_cols);
  const int P_r = static_cast<int>(b.n_rows);
  arma::mat Sigma_b_0_inv = safe_inv_sympd(Sigma_b_0);
  arma::mat b_full(P_r, Cmax, arma::fill::zeros);
  b_full.cols(0, std::max(0, C - 1)) = b;
  arma::mat Omega_full(P_r * P_r, Cmax, arma::fill::zeros);
  Omega_full.cols(0, std::max(0, C - 1)) = Omega;
  arma::vec m_full(Cmax, arma::fill::zeros);
  arma::vec m_curr = update_m(C, z, true);
  if (C > 0) m_full.subvec(0, C - 1) = m_curr;

  // start DP
  for (int n = 0; n < N; ++n) {

   // unassign current class membership
   int z_n = static_cast<int>(std::lround(z[n]));
   if (z_n >= 1 && z_n <= C) {
     m_full[static_cast<arma::uword>(z_n - 1)] -= 1.0;
   }

   // remove empty class
   bool rm = z_n >= 1 && z_n <= C &&
     m_full[static_cast<arma::uword>(z_n - 1)] <= 0.0;
   if (rm) {
     if (C > 1) {
       for (int i = 0; i < N; ++i) {
         int zi = static_cast<int>(std::lround(z[i]));
         if (zi == C) z[i] = static_cast<double>(z_n);
       }
       m_full[static_cast<arma::uword>(z_n - 1)] =
         m_full[static_cast<arma::uword>(C - 1)];
       m_full[static_cast<arma::uword>(C - 1)] = 0.0;
       b_full.col(static_cast<arma::uword>(z_n - 1)) =
         b_full.col(static_cast<arma::uword>(C - 1));
       b_full.col(static_cast<arma::uword>(C - 1)).zeros();
       Omega_full.col(static_cast<arma::uword>(z_n - 1)) =
         Omega_full.col(static_cast<arma::uword>(C - 1));
       Omega_full.col(static_cast<arma::uword>(C - 1)).zeros();
       C -= 1;
     } else {
       b_full.col(0).zeros();
       Omega_full.col(0).zeros();
       m_full[0] = 0.0;
       C = 0;
     }
   }

   // compute class allocation posterior
   const arma::vec beta_n = beta.col(n);
   const bool at_cap = (C >= Cmax);
   const int L = at_cap ? C : (C + 1);
   arma::vec logp(L, arma::fill::zeros);

   // existing classes
   for (int c = 0; c < C; ++c) {
     const double mc = m_full[static_cast<arma::uword>(c)];
     if (mc <= 0.0) {
       logp[c] = -std::numeric_limits<double>::infinity();
       continue;
     }
     arma::vec mean_c = b_full.col(static_cast<arma::uword>(c));
     arma::mat Sigma_c = arma::reshape(
       Omega_full.col(static_cast<arma::uword>(c)), P_r, P_r
     );
     const double loglik = oeli::dmvnorm(beta_n, mean_c, Sigma_c, true);
     logp[c] = std::log(mc) + loglik;
   }

   // new class
   if (!at_cap) {
     const int Rmc = 10;
     double ppd = 0.0;
     for (int r = 0; r < Rmc; ++r) {
       arma::vec b_draw = oeli::rmvnorm(mu_b_0, Sigma_b_0);
       arma::mat Omega_dr = oeli::rwishart(n_Omega_0, V_Omega_0, true);
       ppd += oeli::dmvnorm(beta_n, b_draw, Omega_dr, false);
     }
     ppd /= static_cast<double>(Rmc);
     const double tiny = 1e-300;
     logp[C] = std::log(delta) + std::log(ppd + tiny);
   }

   // sample new class allocation
   double lp_max = logp.max();
   arma::vec p = arma::exp(logp - lp_max);
   double ps = arma::accu(p);
   if (!std::isfinite(ps) || ps <= 0.0) {
     p.ones();
     ps = static_cast<double>(p.n_elem);
   }
   p /= ps;

   int z_new = sample_allocation(p);
   z[n] = static_cast<double>(z_new);
   m_full[static_cast<arma::uword>(z_new - 1)] += 1.0;

   // generate new class
   if (!at_cap && z_new == C + 1) {
     C += 1;
     arma::mat S_c = arma::zeros<arma::mat>(P_r, P_r);
     const int m_c = 1;
     arma::mat Omega_new = update_Omega_c(S_c, m_c, n_Omega_0, V_Omega_0);
     arma::mat b_new =
       update_b_c(beta_n, Omega_new, m_c, Sigma_b_0_inv, mu_b_0);
     Omega_full.col(static_cast<arma::uword>(C - 1)) =
       arma::vectorise(Omega_new);
     b_full.col(static_cast<arma::uword>(C - 1)) = b_new;
   }
  }

  // update class parameters
  arma::vec m = update_m(C, z, false);
  arma::mat b_update = update_b(
    beta, Omega_full.cols(0, std::max(0, C - 1)), z, m, Sigma_b_0_inv, mu_b_0
  );
  arma::mat Omega_update = update_Omega(
    beta, b_update, z, m, n_Omega_0, V_Omega_0
  );

  // identify classes
  if (identify_classes) {
   arma::uvec perm = arma::sort_index(m, "descend");
   b_update = b_update.cols(perm);
   Omega_update = Omega_update.cols(perm);
   arma::uvec inv(perm.n_elem);
   for (arma::uword new_id = 0; new_id < perm.n_elem; ++new_id) {
     inv[perm[new_id]] = new_id;
   }
   arma::vec z_old = z;
   for (int i = 0; i < N; ++i) {
     int old_lbl = static_cast<int>(std::lround(z_old[i])) - 1;
     if (old_lbl >= 0 && old_lbl < static_cast<int>(inv.n_elem)) {
       z[i] = static_cast<double>(inv[static_cast<arma::uword>(old_lbl)] + 1);
     }
   }
  }

  // return results
  return Rcpp::List::create(
   Rcpp::Named("z") = z,
   Rcpp::Named("b") = b_update,
   Rcpp::Named("Omega") = Omega_update,
   Rcpp::Named("C") = C
  );
}

//' Gibbs sampler for probit models
//'
//' @details
//' This function is not supposed to be called directly, but rather via
//' \code{\link{fit_model}}.
//'
//' @param sufficient_statistics \[`list`\]\cr
//' The output of \code{\link{sufficient_statistics}}.
//'
//' @inheritParams fit_model
//' @inheritParams RprobitB_data
//'
//' @return
//' A list of Gibbs samples for
//' \itemize{
//'   \item \code{Sigma},
//'   \item \code{alpha} (only if \code{P_f > 0}),
//'   \item \code{s}, \code{z}, \code{b}, \code{Omega} (only if \code{P_r > 0}),
//'   \item \code{d} (only if \code{ordered = TRUE}),
//' }
//' and a vector \code{class_sequence} of length \code{R}, where the \code{r}-th
//' entry is the number of classes after iteration \code{r}.
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]
Rcpp::List gibbs_sampler (
   Rcpp::List sufficient_statistics, Rcpp::List prior,
   Rcpp::List latent_classes, Rcpp::List fixed_parameter,
   int R, int B, bool print_progress, bool ordered, bool ranked
) {

 // extract 'sufficient_statistics' parameters
 int N = Rcpp::as<int>(sufficient_statistics["N"]);
 int J = Rcpp::as<int>(sufficient_statistics["J"]);
 int P_f = Rcpp::as<int>(sufficient_statistics["P_f"]);
 int P_r = Rcpp::as<int>(sufficient_statistics["P_r"]);
 arma::vec Tvec = Rcpp::as<arma::vec>(sufficient_statistics["Tvec"]);
 arma::vec csTvec = Rcpp::as<arma::vec>(sufficient_statistics["csTvec"]);
 Rcpp::List W;
 Rcpp::List X;
 arma::mat y = Rcpp::as<arma::mat>(sufficient_statistics["y"]);
 arma::mat WkW;
 Rcpp::List XkX;
 Rcpp::List rdiff;
 if (P_f > 0) {
   W = Rcpp::as<Rcpp::List>(sufficient_statistics["W"]);
   WkW = Rcpp::as<arma::mat>(sufficient_statistics["WkW"]);
 }
 if (P_r > 0) {
   X = Rcpp::as<Rcpp::List>(sufficient_statistics["X"]);
   XkX = Rcpp::as<Rcpp::List>(sufficient_statistics["XkX"]);
 }
 if (ranked) {
   rdiff = Rcpp::as<Rcpp::List>(sufficient_statistics["rdiff"]);
 }
 std::vector<arma::mat> rdiff_list;
 std::vector<arma::mat> rdiff_inv_list;
 if (ranked) {
   const int L = rdiff.size();
   rdiff_list.resize(L);
   for (int i = 0; i < L; ++i) {
     rdiff_list[i] = Rcpp::as<arma::mat>(rdiff[i]);
   }
   rdiff_inv_list.resize(L);
   for (int i = 0; i < L; ++i) {
     // Use Moore–Penrose pseudo-inverse for robustness
     rdiff_inv_list[i] = arma::pinv(rdiff_list[i]);
   }
 }
 std::vector<arma::mat> Wv, Xv;
 const int NT = static_cast<int>(arma::accu(Tvec));
 if (P_f > 0) {
   Wv.resize(NT);
   for (int i = 0; i < NT; ++i) Wv[i] = Rcpp::as<arma::mat>(W[i]);
 }
 if (P_r > 0) {
   Xv.resize(NT);
   for (int i = 0; i < NT; ++i) Xv[i] = Rcpp::as<arma::mat>(X[i]);
 }
 std::vector<arma::mat> XSigX_ord;
 std::vector<arma::mat> XkX_mats;
 if (ordered && P_r > 0) {
   XSigX_ord.resize(N);
   for (int n = 0; n < N; ++n) {
     XSigX_ord[n] = arma::reshape(Rcpp::as<arma::mat>(XkX[n]), P_r, P_r);
   }
 } else if (P_r > 0) {
   XkX_mats.resize(N);
   for (int n = 0; n < N; ++n) {
     XkX_mats[n] = Rcpp::as<arma::mat>(XkX[n]);
   }
 }

 // extract 'latent_classes' parameters
 int C = Rcpp::as<int>(latent_classes["C"]);
 int Cmax = 10;
 int Cdrawsize = 10;
 int buffer = 50;
 double epsmin = 0.01;
 double epsmax = 0.7;
 double deltamin = 0.1;
 double deltashift = 0.5;
 bool wb_update = Rcpp::as<bool>(latent_classes["wb_update"]);
 bool dp_update = Rcpp::as<bool>(latent_classes["dp_update"]);
 if (!wb_update && !dp_update) {
   Cdrawsize = C;
 } else {
   Cmax = Rcpp::as<int>(latent_classes["Cmax"]);
   Cdrawsize = Cmax;
 }
 if (wb_update == true){
   buffer = Rcpp::as<int>(latent_classes["buffer"]);
   epsmin = Rcpp::as<double>(latent_classes["epsmin"]);
   epsmax = Rcpp::as<double>(latent_classes["epsmax"]);
   deltamin = Rcpp::as<double>(latent_classes["deltamin"]);
   deltashift = Rcpp::as<double>(latent_classes["deltashift"]);
 }

 // extract 'prior' parameters
 int delta = 1;
 arma::vec mu_alpha_0;
 arma::mat Sigma_alpha_0, Sigma_alpha_0_;
 int n_Omega_0 = P_r + 2;
 arma::mat V_Omega_0;
 arma::vec mu_b_0;
 arma::mat Sigma_b_0;
 arma::mat Sigma_b_0_inv;
 int n_Sigma_0 = J + 1;
 arma::mat V_Sigma_0;
 if (!ordered) {
   n_Sigma_0 = Rcpp::as<int>(prior["n_Sigma_0"]);
   V_Sigma_0 = Rcpp::as<arma::mat>(prior["V_Sigma_0"]);
 }
 arma::vec mu_d_0;
 arma::mat Sigma_d_0;
 if (P_f > 0) {
   mu_alpha_0 = Rcpp::as<arma::vec>(prior["mu_alpha_0"]);
   Sigma_alpha_0 = Rcpp::as<arma::mat>(prior["Sigma_alpha_0"]);
   Sigma_alpha_0_ = safe_inv_sympd(Sigma_alpha_0);
 }
 if (P_r > 0) {
   delta = Rcpp::as<int>(prior["delta"]);
   mu_b_0 = Rcpp::as<arma::vec>(prior["mu_b_0"]);
   Sigma_b_0 = Rcpp::as<arma::mat>(prior["Sigma_b_0"]);
   Sigma_b_0_inv = safe_inv_sympd(Sigma_b_0);
   n_Omega_0 = Rcpp::as<int>(prior["n_Omega_0"]);
   V_Omega_0 = Rcpp::as<arma::mat>(prior["V_Omega_0"]);
 }
 if (ordered) {
   mu_d_0 = Rcpp::as<arma::vec>(prior["mu_d_0"]);
   Sigma_d_0 = Rcpp::as<arma::mat>(prior["Sigma_d_0"]);
 }

 // define helper variables and functions
 arma::vec s_cand;
 arma::mat Omega_c_inv;
 arma::vec b_c;
 arma::mat S;
 arma::mat IW;
 arma::vec eps;
 const int Tmax = static_cast<int>(y.n_cols);
 arma::mat mu_mat = arma::zeros<arma::mat>(N, Tmax);
 arma::mat mu_mat_tmp = arma::zeros<arma::mat>(1,1);
 int ind;
 int Jm1 = J - 1;
 double old_ll = 0.0;
 Rcpp::List update_d_out;
 arma::vec Sigmainv_vec;
 std::vector<arma::mat> Sigmainv_ranked;

 // allocate space for output
 arma::mat s_draws = arma::zeros<arma::mat>(R, Cdrawsize);
 arma::mat z_draws = arma::zeros<arma::mat>(R, N);
 arma::mat b_draws = arma::zeros<arma::mat>(R, P_r * Cdrawsize);
 arma::mat Omega_draws = arma::zeros<arma::mat>(R, P_r * P_r * Cdrawsize);
 arma::mat alpha_draws = arma::zeros<arma::mat>(R, P_f);
 arma::mat Sigma_draws;
 if (ordered) {
   Sigma_draws = arma::zeros<arma::mat>(R, 1);
 } else {
   Sigma_draws = arma::zeros<arma::mat>(R, Jm1 * Jm1);
 }
 arma::mat d_draws = arma::zeros<arma::mat>(R, J - 2);
 arma::vec class_sequence(R, arma::fill::zeros);
 bool frozen_C = false;

 // initialize Gibbs sampler
 arma::vec s;
 arma::vec z;
 arma::vec m;
 arma::mat b;
 arma::mat Omega;
 arma::vec alpha;
 arma::mat beta;
 arma::mat U;
 arma::mat Sigma;
 arma::mat Sigmainv;
 arma::vec d;
 arma::vec gamma;
 const int rowsU = ordered ? 1 : (J - 1);

 if (P_f > 0) {
   alpha = mu_alpha_0;
 } else {
   alpha.reset();
 }
 if (ordered) {
   Sigma    = arma::ones<arma::mat>(1, 1);
   Sigmainv = arma::ones<arma::mat>(1, 1);
 } else {
   Sigma    = arma::eye(J - 1, J - 1);
   Sigmainv = arma::eye(J - 1, J - 1);
 }
 if (ordered) {
   d = mu_d_0;
   gamma = d_to_gamma(d);
 } else {
   d.reset();
 }
 if (P_r > 0) {
   s = arma::ones<arma::vec>(C) / static_cast<double>(C);
   b.set_size(P_r, C);
   Omega.set_size(P_r * P_r, C);
   arma::mat Omega0 = clamp_pd(V_Omega_0, 1e-8);
   for (int c = 0; c < C; ++c) {
     Omega.col(c) = arma::vectorise(Omega0);
     b.col(c)     = mu_b_0;
   }
   z.set_size(N);
   for (int n = 0; n < N; ++n) z[n] = static_cast<double>((n % C) + 1);
   m = update_m(C, z, false);
   beta.set_size(P_r, N);
   for (int n = 0; n < N; ++n) {
     int ci = std::max(
       0, std::min(C - 1, static_cast<int>(std::lround(z[n])) - 1)
     );
     beta.col(n) = b.col(ci);
   }
 } else {
   s.set_size(0); z.set_size(0); m.set_size(0);
   b.set_size(P_r, 0); Omega.set_size(P_r * P_r, 0);
   beta.set_size(P_r, N); beta.zeros();
 }
 U = arma::zeros<arma::mat>(rowsU, NT);

 // set fixed parameter values
 bool do_update_s = true;
 if (fixed_parameter.containsElementNamed("s")) {
   do_update_s = false;
   s = Rcpp::as<arma::vec>(fixed_parameter["s"]);
 }
 bool do_update_z = true;
 if (fixed_parameter.containsElementNamed("z")) {
   do_update_z = false;
   z = Rcpp::as<arma::vec>(fixed_parameter["z"]);
 }
 bool do_update_b = true;
 if (fixed_parameter.containsElementNamed("b")) {
   do_update_b = false;
   b = Rcpp::as<arma::mat>(fixed_parameter["b"]);
 }
 bool do_update_Omega = true;
 if (fixed_parameter.containsElementNamed("Omega")) {
   do_update_Omega = false;
   Omega = Rcpp::as<arma::mat>(fixed_parameter["Omega"]);
 }
 bool do_update_alpha = true;
 if (fixed_parameter.containsElementNamed("alpha")) {
   do_update_alpha = false;
   alpha = Rcpp::as<arma::vec>(fixed_parameter["alpha"]);
 }
 bool do_update_beta = true;
 if (fixed_parameter.containsElementNamed("beta")) {
   do_update_beta = false;
   beta = Rcpp::as<arma::mat>(fixed_parameter["beta"]);
 }
 bool do_update_Sigma = true;
 if (fixed_parameter.containsElementNamed("Sigma")) {
   do_update_Sigma = false;
   Sigma = Rcpp::as<arma::mat>(fixed_parameter["Sigma"]);
   Sigmainv = safe_inv_sympd(Sigma);
 }
 if (ordered) {
   do_update_Sigma = false;
   Sigma = arma::ones<arma::mat>(1,1);
   Sigmainv = arma::ones<arma::mat>(1,1);
 }

 // set progress output
 Rcpp::Environment pkg = Rcpp::Environment::namespace_env("RprobitB");
 Rcpp::Function RprobitB_pp = pkg["RprobitB_pp"];

 // start loop
 for (int r = 0; r < R; ++r) {

   // freeze class number after burn-in
   if (P_r > 0 && !frozen_C && r == B && B > 0) {
     std::unordered_map<int,int> counts;
     int Cstar = 0, best = -1;
     for (int i = 0; i < B; ++i) {
       int Ci = static_cast<int>(std::lround(class_sequence[i]));
       int c = ++counts[Ci];
       if (c > best) { best = c; Cstar = Ci; }
     }
     int r_ref = B - 1;
     while (r_ref >= 0 &&
            static_cast<int>(std::lround(class_sequence[r_ref])) != Cstar) {
       --r_ref;
     }
     if (r_ref >= 0) {
       C = Cstar;
       if (C > 0) {
         s = arma::trans(s_draws.row(r_ref).cols(0, C - 1));
         arma::rowvec b_row = b_draws.row(r_ref).cols(0, P_r * C - 1);
         b = arma::reshape(b_row.t(), P_r, C);
         const int PP = P_r * P_r;
         arma::rowvec Om_row = Omega_draws.row(r_ref).cols(0, PP * C - 1);
         Omega = arma::reshape(Om_row.t(), PP, C);
         z = arma::trans(z_draws.row(r_ref));
         m = update_m(C, z, false);
       } else {
         s.set_size(0); b.set_size(P_r, 0); Omega.set_size(P_r * P_r, 0);
         z.zeros(); m.zeros();
       }
       frozen_C = true;
     }
   }

   // print progress
   if (print_progress && ((r+1) % 10 == 0 || r == 0)) {
     if (!wb_update && !dp_update) {
       RprobitB_pp("MCMC iteration", r + 1, R);
     } else {
       RprobitB_pp("MCMC iteration (C = " + std::to_string(C) + ")", r + 1, R);
     }
   }

   // check for code interruption by user
   Rcpp::checkUserInterrupt();

   // prepare Sigma
   if (!ordered) {
     Sigmainv_vec = arma::vectorise(Sigmainv);
   }
   if (ranked) {
     const int L = static_cast<int>(rdiff_list.size());
     Sigmainv_ranked.resize(L);
     for (int yi = 0; yi < L; ++yi) {
       arma::mat S_tmp = rdiff_list[yi] * Sigma * rdiff_list[yi].t();
       S_tmp = 0.5 * (S_tmp + S_tmp.t()) + 1e-12 *
         arma::eye(S_tmp.n_rows, S_tmp.n_cols);
       Sigmainv_ranked[yi] = safe_inv_sympd(S_tmp);
     }
   }

   if (P_r > 0) {

     // update s, z, m, b, Omega by posterior draws if
     // - no DP or
     // - outside updating period
     if (!dp_update || r + 1 > B || r == 0) {

       // update s (but only if draw is descending)
       if (do_update_s) {
         arma::vec s_cand = update_s(delta, m);
         if (r < B / 10) {
           // manually sort s in 10% burn-in
           s_cand = arma::sort(s_cand, "descend");
         }
         bool s_sorted = std::is_sorted(
           s_cand.begin(), s_cand.end(), std::greater_equal<double>()
         );
         if (s_sorted) s = s_cand;
       }

       // update z
       if (do_update_z) {
         z = update_z(s, beta, b, Omega);
       }

       // update m
       m = update_m(C, z, !frozen_C);

       // update b
       if (do_update_b) {
         b = update_b(beta, Omega, z, m, Sigma_b_0_inv, mu_b_0);
       }

       // update Omega
       if(do_update_Omega) {
         Omega = update_Omega(beta, b, z, m, n_Omega_0, V_Omega_0);
         for (int c = 0; c < C; ++c) {
           arma::mat Oc = arma::reshape(Omega.col(c), P_r, P_r);
           Oc = clamp_pd(Oc, 1e-10);
           Omega.col(c) = arma::vectorise(Oc);
         }
       }
     }

     // update beta
     if (do_update_beta) {
       std::vector<arma::mat> Omega_inv_cache;
       Omega_inv_cache.reserve(C);
       for (int c = 0; c < C; ++c) {
         arma::mat Oc = arma::reshape(Omega.col(c), P_r, P_r);
         Oc = clamp_pd(Oc, 1e-12);
         Omega_inv_cache.emplace_back(safe_inv_sympd(Oc));
       }
       for (int n = 0; n < N; ++n) {
         const int z_n_lbl = static_cast<int>(std::lround(z[n]));
         const int ci = std::max(1, std::min(C, z_n_lbl)) - 1;
         const arma::mat& Omega_c_inv = Omega_inv_cache[ci];
         const arma::vec b_c = b.col(ci);
         arma::mat XSigX;
         if (ordered){
           XSigX = XSigX_ord[n];
         } else {
           XSigX = arma::reshape(XkX_mats[n] * Sigmainv_vec, P_r, P_r);
         }
         arma::vec XSigU = arma::zeros<arma::vec>(P_r);
         const int Tn = static_cast<int>(std::lround(Tvec[n]));
         const int base = static_cast<int>(std::lround(csTvec[n]));
         for (int t = 0; t < Tn; ++t) {
           ind = base + t;
           if (P_f == 0) {
             XSigU += arma::trans(Xv[ind]) * Sigmainv * U(arma::span::all,ind);
           }
           if (P_f > 0) {
             XSigU += arma::trans(Xv[ind]) * Sigmainv *
               U(arma::span::all,ind) - arma::trans(Xv[ind]) * Sigmainv *
               Wv[ind] * alpha;
           }
         }
         beta(arma::span::all,n) = update_coefficient(
           b_c, Omega_c_inv, XSigX, XSigU
         );
       }
     }

     // weight-based update of classes
     if (wb_update == true && (r + 1) <= B && (r + 1) % buffer == 0) {
       Rcpp::List class_update = update_classes_wb(
         s, b, Omega, epsmin, epsmax, deltamin, deltashift, true, Cmax
       );
       s = Rcpp::as<arma::vec>(class_update["s"]);
       C = s.size();
       b = Rcpp::as<arma::mat>(class_update["b"]);
       Omega = Rcpp::as<arma::mat>(class_update["Omega"]);
       z = update_z(s, beta, b, Omega);
       m = update_m(C, z, true);
     }

     // Dirichlet process
     if (dp_update == true && (r + 1) <= B) {
       Rcpp::List class_update = update_classes_dp(
         beta, z, b, Omega, delta, mu_b_0, Sigma_b_0, n_Omega_0, V_Omega_0,
         true, Cmax
       );
       z = Rcpp::as<arma::vec>(class_update["z"]);
       b = Rcpp::as<arma::mat>(class_update["b"]);
       Omega = Rcpp::as<arma::mat>(class_update["Omega"]);
       C = Rcpp::as<int>(class_update["C"]);
       m = update_m(C, z, !frozen_C);
       s = m / N;
     }

     // save number of classes in each iteration
     class_sequence[r] = C;
   }

   if (P_f > 0) {
     // update alpha
     if(do_update_alpha) {
       arma::mat WSigW;
       if (ordered) {
         WSigW = arma::reshape(WkW, P_f, P_f);
       } else {
         WSigW = arma::reshape(WkW * Sigmainv_vec, P_f, P_f);
       }
       arma::vec WSigU = arma::zeros<arma::vec>(P_f);
       for (int n = 0; n < N; ++n) {
         const int Tn = static_cast<int>(std::lround(Tvec[n]));
         const int base = static_cast<int>(std::lround(csTvec[n]));
         for (int t = 0; t < Tn; ++t) {
           ind = base + t;
           if (P_r == 0) {
             WSigU += arma::trans(Wv[ind]) * Sigmainv * U(arma::span::all,ind);
           }
           if (P_r > 0) {
             WSigU += arma::trans(Wv[ind]) * Sigmainv *
               (U(arma::span::all, ind) - Xv[ind] * beta(arma::span::all, n));
           }
         }
       }
       alpha = update_coefficient(mu_alpha_0, Sigma_alpha_0_, WSigW, WSigU);
     }
   }

   // update U
   if (ordered) {
     for (int n = 0; n < N; ++n) {
       const int Tn = static_cast<int>(std::lround(Tvec[n]));
       const int base = static_cast<int>(std::lround(csTvec[n]));
       for (int t = 0; t < Tn; ++t) {
         ind = base + t;
         if (P_f > 0 && P_r > 0) {
           mu_mat_tmp = Wv[ind] * alpha +
             Xv[ind] * beta(arma::span::all, n);
         }
         if (P_f > 0 && P_r == 0) {
           mu_mat_tmp = Wv[ind] * alpha;
         }
         if (P_f == 0 && P_r > 0) {
           mu_mat_tmp = Xv[ind] * beta(arma::span::all, n);
         }
         if (P_f == 0 && P_r == 0) {
           mu_mat_tmp = arma::zeros<arma::mat>(1, 1);
         }
         U(arma::span::all,ind) = oeli::rttnorm(
           mu_mat_tmp(0,0), 1.0, gamma[y(n,t) - 1], gamma[y(n,t)]
         );
       }
     }
   } else if (ranked) {
     for (int n = 0; n < N; ++n) {
       const int Tn = static_cast<int>(std::lround(Tvec[n]));
       const int base = static_cast<int>(std::lround(csTvec[n]));
       for (int t = 0; t < Tn; ++t) {
         ind = base + t;
         const arma::uword yi = static_cast<arma::uword>(y(n, t) - 1);
         const arma::mat& rdiff_tmp = rdiff_list[yi];
         arma::vec U_tmp = rdiff_tmp * U(arma::span::all, ind);
         arma::vec mu_vec_tmp = arma::zeros<arma::vec>(Jm1);
         const arma::mat& Sigmainv_tmp = Sigmainv_ranked[yi];
         if (P_f > 0 && P_r > 0) {
           mu_vec_tmp = rdiff_tmp * Wv[ind] * alpha + rdiff_tmp * Xv[ind] *
             beta(arma::span::all,n);
         }
         if (P_f > 0 && P_r == 0) {
           mu_vec_tmp = rdiff_tmp * Wv[ind] * alpha;
         }
         if (P_f == 0 && P_r > 0) {
           mu_vec_tmp = rdiff_tmp * Xv[ind] * beta(arma::span::all, n);
         }
         U(arma::span::all, ind) = rdiff_inv_list[yi] *
           update_U_ranked(U_tmp, mu_vec_tmp, Sigmainv_tmp);
       }
     }
   } else {
     for (int n = 0; n < N; ++n) {
       const int Tn = static_cast<int>(std::lround(Tvec[n]));
       const int base = static_cast<int>(std::lround(csTvec[n]));
       for (int t = 0; t < Tn; ++t) {
         ind = base + t;
         if (P_f > 0 && P_r > 0) {
           U(arma::span::all, ind) = update_U(
             U(arma::span::all, ind), y(n, t),
             Wv[ind] * alpha + Xv[ind] * beta(arma::span::all, n), Sigmainv
           );
         }
         if (P_f > 0 && P_r == 0) {
           U(arma::span::all, ind) = update_U(
             U(arma::span::all, ind), y(n, t), Wv[ind] * alpha, Sigmainv
           );
         }
         if (P_f == 0 && P_r > 0) {
           U(arma::span::all, ind) = update_U(
             U(arma::span::all, ind), y(n, t),
             Xv[ind] * beta(arma::span::all, n), Sigmainv
           );
         }
         if (P_f == 0 && P_r == 0) {
           arma::vec mu_null(Jm1, arma::fill::zeros);
           U(arma::span::all,ind) = update_U(U(arma::span::all, ind), y(n, t),
             mu_null, Sigmainv);
         }
       }
     }
   }

   // update Sigma
   if (do_update_Sigma) {
     S = arma::zeros<arma::mat>(Jm1, Jm1);
     for (int n = 0; n < N; ++n) {
       const int Tn = static_cast<int>(std::lround(Tvec[n]));
       const int base = static_cast<int>(std::lround(csTvec[n]));
       for (int t = 0; t < Tn; ++t) {
         ind = base + t;
         if (P_f > 0 && P_r > 0) {
           eps = U(arma::span::all,ind) - Wv[ind] * alpha - Xv[ind] *
             beta(arma::span::all, n);
         }
         if (P_f > 0 && P_r == 0) {
           eps = U(arma::span::all, ind) - Wv[ind] * alpha;
         }
         if (P_f == 0 && P_r > 0) {
           eps = U(arma::span::all, ind) - Xv[ind] * beta(arma::span::all, n);
         }
         if (P_f == 0 && P_r == 0) {
           eps = U(arma::span::all, ind);
         }
         S += eps * arma::trans(eps);
       }
     }
     Sigma = clamp_pd(
       update_Sigma(
         n_Sigma_0, V_Sigma_0, static_cast<int>(arma::accu(Tvec)), S
       ),
       1e-8
     );
     Sigmainv = safe_inv_sympd(Sigma);
   }

   // update d (for the ordered probit model)
   if (ordered) {
     for (int n = 0; n < N; ++n) {
       const int Tn = static_cast<int>(std::lround(Tvec[n]));
       const int base = static_cast<int>(std::lround(csTvec[n]));
       for (int t = 0; t < Tn; ++t) {
         ind = base + t;
         if (P_f > 0 && P_r > 0) {
           mu_mat_tmp = Wv[ind] * alpha + Xv[ind] * beta(arma::span::all,n);
         } else if (P_f > 0 && P_r == 0) {
           mu_mat_tmp = Wv[ind] * alpha;
         } else if(P_f == 0 && P_r > 0) {
           mu_mat_tmp = Xv[ind] * beta(arma::span::all,n);
         } else {
           mu_mat_tmp = arma::zeros<arma::mat>(1,1);
         }
         mu_mat(n,t) = mu_mat_tmp(0, 0);
       }
     }
     if (r == 0) {
       old_ll = ll_ordered(d, y, mu_mat, Tvec);
     }
     update_d_out = update_d(d, y, mu_mat, old_ll, mu_d_0, Sigma_d_0, Tvec);
     d = Rcpp::as<arma::vec>(update_d_out["d"]);
     old_ll = Rcpp::as<double>(update_d_out["ll"]);
     gamma = d_to_gamma(d);
   }

   // save draws
   if (P_f > 0) {
     alpha_draws(r, arma::span::all) = arma::trans(alpha);
   }
   if (P_r > 0) {
     s_draws(r, arma::span(0, s.size() - 1)) = arma::trans(s);
     z_draws(r, arma::span::all) = arma::trans(z);
     arma::vec vectorise_b = arma::vectorise(b);
     b_draws(r, arma::span(0, vectorise_b.size() - 1)) =
       arma::trans(vectorise_b);
     arma::vec vectorise_Omega = arma::vectorise(Omega);
     Omega_draws(r, arma::span(0, vectorise_Omega.size() - 1)) =
       arma::trans(vectorise_Omega);
   }
   Sigma_draws(r, arma::span::all) = arma::trans(arma::vectorise(Sigma));
   if (ordered == true) {
     d_draws(r, arma::span::all) = arma::trans(d);
   }
 }

  // return Gibbs samples
  return Rcpp::List::create(
   Rcpp::Named("s") = s_draws,
   Rcpp::Named("z") = z_draws,
   Rcpp::Named("alpha") = alpha_draws,
   Rcpp::Named("b") = b_draws,
   Rcpp::Named("Omega") = Omega_draws,
   Rcpp::Named("Sigma") = Sigma_draws,
   Rcpp::Named("d") = d_draws,
   Rcpp::Named("class_sequence") = class_sequence
  );
}
