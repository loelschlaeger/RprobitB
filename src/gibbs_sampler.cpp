// [[Rcpp::depends("RcppArmadillo")]]

#include "gibbs_sampler.h"

//' Update latent classes
//'
//' @description
//' Low-level sampler kernels for class weights, allocations, sizes, the
//' weight-based class update, and Dirichlet-process class updates.
//'
//' @param prob \[`numeric(C)`\]\cr
//' Class probabilities.
//'
//' @param delta \[`numeric(1)`\]\cr
//' Dirichlet concentration parameter.
//'
//' @param m \[`numeric(C)`\]\cr
//' Class sizes.
//'
//' @param s \[`numeric(C)`\]\cr
//' Class weights.
//'
//' @param beta \[`matrix(P, N)`\]\cr
//' Individual coefficient draws in columns.
//'
//' @param b \[`matrix(P, C)`\]\cr
//' Class means in columns.
//'
//' @param Omega \[`matrix(P * P, C)`\]\cr
//' Vectorized class covariance matrices in columns.
//'
//' @param C \[`integer(1)`\]\cr
//' Number of classes.
//'
//' @param z \[`numeric(N)`\]\cr
//' Class allocations numbered from one to `C`.
//'
//' @param non_zero \[`logical(1)`\]\cr
//' Replace empty class sizes by one?
//'
//' @param identify_classes \[`logical(1)`\]\cr
//' Order the current active classes by size?
//'
//' @param Cmax \[`integer(1)`\]\cr
//' Maximum number of classes.
//'
//' @param epsmin \[`numeric(1)`\]\cr
//' Remove the smallest class when its weight is below this threshold.
//'
//' @param epsmax \[`numeric(1)`\]\cr
//' Split the largest class when its weight exceeds this threshold.
//'
//' @param deltamin \[`numeric(1)`\]\cr
//' Merge the two closest classes when the Euclidean distance between their
//' means is below this threshold.
//'
//' @param deltashift \[`numeric(1)`\]\cr
//' Scale of the mean displacement along the leading covariance eigenvector
//' after splitting a class.
//'
//' @param mu_b_0 \[`numeric(P)`\]\cr
//' Prior mean for class means.
//'
//' @param Sigma_b_0 \[`matrix(P, P)`\]\cr
//' Prior covariance for class means.
//'
//' @param n_Omega_0 \[`integer(1)`\]\cr
//' Prior degrees of freedom for class covariances.
//'
//' @param V_Omega_0 \[`matrix(P, P)`\]\cr
//' Prior scale matrix for class covariances.
//'
//' @return
//' The functions return one sampler update:
//'
//' - `sample_allocation()`: an integer class label.
//' - `update_s()`: a `C` by 1 numeric matrix of class weights.
//' - `update_z()`: an `N` by 1 numeric matrix of allocations.
//' - `update_m()`: a `C` by 1 numeric matrix of class sizes.
//' - `update_classes_wb()`: a list with `s`, `b`, `Omega`, and `update_type`,
//'   where the latter is zero for no update, one for removal, two for
//'   splitting, and three for merging.
//' - `update_classes_dp()`: a list with `z`, `b`, `Omega`, and `C`.
//'
//' @references
//' \insertRef{Neal2000}{RprobitB}
//'
//' \insertRef{Oelschlaeger2021}{RprobitB}
//'
//' @keywords models
//'
//' @examples
//' ### a latent class state of six deciders with one random coefficient
//' set.seed(1)
//' beta <- matrix(c(-1, -1.2, -0.8, 1, 1.3, 0.9), nrow = 1)
//' b <- matrix(c(-1, 1), nrow = 1)
//' Omega <- matrix(c(0.2, 0.2), nrow = 1)
//'
//' ### the weights, the allocations, and the class sizes are drawn in turn
//' s <- update_s(delta = 1, m = c(3, 3))
//' z <- update_z(s, beta, b, Omega)
//' m <- update_m(C = 2, z = z)
//' sample_allocation(c(0.5, 0.3, 0.2))
//'
//' ### the weight-based update splits a class that grew too large
//' update_classes_wb(s = c(0.9, 0.1), b = b, Omega = Omega)
//'
//' ### the Dirichlet process update draws the class count from the data
//' update_classes_dp(
//'   beta = beta, z = z, b = b, Omega = Omega, delta = 1,
//'   mu_b_0 = 0, Sigma_b_0 = diag(1), n_Omega_0 = 4, V_Omega_0 = diag(1)
//' )
//'
//' @name class_updates
//' @rdname class_updates
//' @export
// [[Rcpp::export]]
int sample_allocation(arma::vec const& prob) {
  const int C = static_cast<int>(prob.n_elem);
  arma::vec safe_prob = prob;
  safe_prob.elem(arma::find_nonfinite(safe_prob)).zeros();
  safe_prob.transform([](double p) { return (p > 0.0) ? p : 0.0; });
  const double total = arma::sum(safe_prob);
  if (total <= 0.0) return 1 + static_cast<int>(C * unif_rand()) % C;
  const double u = total * unif_rand();
  double cumulative = 0.0;
  for (int c = 0; c < C; ++c) {
    cumulative += safe_prob[c];
    if (u < cumulative) return c + 1;
  }
  return C;
}

arma::mat inv_spd(arma::mat const& x) {
  arma::mat out;
  if (arma::inv_sympd(out, arma::symmatu(x))) return out;
  return arma::inv(x);
}

static arma::uvec optional_flags(
    Rcpp::Nullable<Rcpp::IntegerVector> const& flags, arma::uword size,
    std::string const& name, std::string const& unit
) {
  arma::uvec out(size, arma::fill::ones);
  if (flags.isNull()) return out;
  Rcpp::IntegerVector values(flags.get());
  if (static_cast<arma::uword>(values.size()) != size) {
    Rcpp::stop("`" + name + "` must have one entry per " + unit + ".");
  }
  for (arma::uword i = 0; i < size; ++i) {
    out[i] = values[i] == 1 ? 1u : 0u;
  }
  return out;
}

arma::vec transform_random_effects(
    const arma::vec& latent, const arma::ivec& distribution
) {
  arma::vec utility = latent;
  for (arma::uword p = 0; p < latent.n_elem; ++p) {
    if (distribution[p] > 0) {
      utility[p] = std::exp(latent[p]);
    } else if (distribution[p] < 0) {
      utility[p] = -std::exp(latent[p]);
    }
  }
  return utility;
}

//' @name utility_updates
//' @rdname utility_updates
//' @export
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

double log_density_delta(
    double delta, arma::vec const& s, double shape, double rate
) {
  if (!std::isfinite(delta) || delta <= 0.0) {
    return -std::numeric_limits<double>::infinity();
  }
  const double C = static_cast<double>(s.n_elem);
  const double sum_log_s = arma::accu(arma::log(arma::clamp(s, 1e-300, 1.0)));
  return std::lgamma(C * delta) - C * std::lgamma(delta) +
    (delta - 1.0) * sum_log_s + (shape - 1.0) * std::log(delta) -
    rate * delta;
}

double log_probability_choice(
    int y, const arma::vec& sys, const arma::mat& Sigma,
    const arma::uvec& available, bool ordered, bool ranked,
    const arma::vec& gamma, const arma::mat& rdiff
) {
  const double floor_log = std::log(1e-300);
  if (ordered) {
    const double m = sys(0);
    const double lb = gamma[static_cast<arma::uword>(y - 1)] - m;
    const double ub = gamma[static_cast<arma::uword>(y)] - m;
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
    return std::isfinite(logp) ? logp : floor_log;
  }
  arma::mat A;
  if (ranked) {
    A = rdiff;
  } else {
    const int Jm1 = static_cast<int>(sys.n_elem);
    const int chosen = y - 1;
    const bool complete = available.n_elem == 0;
    std::vector<arma::rowvec> rows;
    for (int k = 0; k < Jm1; ++k) {
      if (k == chosen || !(complete || available[k] == 1u)) continue;
      arma::rowvec row(Jm1, arma::fill::zeros);
      row[k] = 1.0;
      if (chosen < Jm1) row[chosen] -= 1.0;
      rows.push_back(row);
    }
    if (chosen < Jm1 && (complete || available[Jm1] == 1u)) {
      arma::rowvec row(Jm1, arma::fill::zeros);
      row[chosen] = -1.0;
      rows.push_back(row);
    }
    if (rows.empty()) return 0.0;
    A.set_size(rows.size(), Jm1);
    for (size_t i = 0; i < rows.size(); ++i) A.row(i) = rows[i];
  }
  const arma::vec mean = A * sys;
  arma::mat covariance = A * Sigma * A.t();
  covariance = 0.5 * (covariance + covariance.t());
  if (mean.n_elem == 1) {
    const double logp = R::pnorm(
      0.0, mean(0), std::sqrt(covariance(0, 0)), 1, 1
    );
    return std::isfinite(logp) ? logp : floor_log;
  }
  const double probability = oeli::pmvnorm(
    arma::zeros<arma::vec>(mean.n_elem), mean, covariance, 1e-3, R_NilValue,
    "ghk", 500
  );
  return std::log(std::max(probability, 1e-300));
}

//' @rdname utility_updates
//' @export
// [[Rcpp::export]]
double log_likelihood_ordered (
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

arma::mat log_likelihood_mixture(
    const arma::mat& beta, const arma::mat& b, const arma::mat& Omega,
    const arma::mat& lambda, const ClassLogLikelihood& log_likelihood_class,
    int N, int C
) {
  const int P_r = static_cast<int>(b.n_rows);
  const bool latent_class = static_cast<bool>(log_likelihood_class);
  arma::mat loglik(N, C, arma::fill::zeros);
  for (int c = 0; c < C; ++c) {
    arma::vec mean_c;
    arma::mat cov_c;
    if (P_r > 0) {
      mean_c = b.col(c);
      cov_c = arma::reshape(Omega.col(c), P_r, P_r);
    }
    for (int n = 0; n < N; ++n) {
      double value = 0.0;
      if (P_r > 0) {
        value += oeli::dmvnorm(beta.col(n), mean_c, cov_c, true);
      }
      if (latent_class) value += log_likelihood_class(n, lambda.col(c));
      loglik(n, c) = value;
    }
  }
  return loglik;
}

double log_likelihood_random_effect(
    const arma::vec& latent, const arma::ivec& distribution,
    const arma::mat& X, const arma::mat& target, const arma::mat& Sigma_inv
) {
  const arma::vec utility = transform_random_effects(latent, distribution);
  if (!utility.is_finite()) {
    return -std::numeric_limits<double>::infinity();
  }
  const arma::mat residual = target - arma::reshape(
    X * utility, target.n_rows, target.n_cols
  );
  return -0.5 * arma::accu((Sigma_inv * residual) % residual);
}

//' @name coefficient_updates
//' @rdname coefficient_updates
//' @export
// [[Rcpp::export]]
arma::vec update_coefficient (
    arma::vec mu_beta_0, arma::mat Sigma_beta_0_inv,
    arma::mat XSigX, arma::vec XSigU
) {
  arma::mat XSigX_clean = XSigX;
  XSigX_clean.elem(arma::find_nonfinite(XSigX_clean)).zeros();
  arma::vec XSigU_clean = XSigU;
  XSigU_clean.elem(arma::find_nonfinite(XSigU_clean)).zeros();
  arma::mat Sigma_beta = inv_spd(Sigma_beta_0_inv + XSigX_clean);
  arma::vec mu_beta = Sigma_beta * (Sigma_beta_0_inv * mu_beta_0 + XSigU_clean);
  return oeli::rmvnorm(mu_beta, Sigma_beta);
}

static arma::mat update_lambda(
    const arma::vec& z, int C, int P_l, const std::vector<arma::mat>& L,
    const std::vector<arma::mat>& LkL, const arma::mat& resid,
    const arma::mat& Sigma_inv, const arma::vec& Sigmainv_vec, bool ordered,
    const arma::vec& Tvec, const arma::vec& csTvec,
    const arma::vec& mu_lambda_0, const arma::mat& Sigma_lambda_0_inv
) {
  const int N = static_cast<int>(z.n_elem);
  arma::mat lambda(P_l, C);
  for (int c = 0; c < C; ++c) {
    arma::mat LSigL(P_l, P_l, arma::fill::zeros);
    arma::vec LSigU(P_l, arma::fill::zeros);
    for (int n = 0; n < N; ++n) {
      if (static_cast<int>(std::lround(z[n])) != c + 1) continue;
      if (ordered) {
        LSigL += LkL[n];
      } else {
        LSigL += arma::reshape(LkL[n] * Sigmainv_vec, P_l, P_l);
      }
      const int Tn = static_cast<int>(std::lround(Tvec[n]));
      const int base = static_cast<int>(std::lround(csTvec[n]));
      for (int t = 0; t < Tn; ++t) {
        const int index = base + t;
        LSigU += arma::trans(L[index]) * Sigma_inv * resid.col(index);
      }
    }
    lambda.col(c) = update_coefficient(
      mu_lambda_0, Sigma_lambda_0_inv, LSigL, LSigU
    );
  }
  return lambda;
}

static arma::vec update_beta_elliptical(
    const arma::vec& current, const arma::vec& mean,
    const arma::mat& covariance, const arma::ivec& distribution,
    const arma::mat& X, const arma::mat& target, const arma::mat& Sigma_inv
) {
  const double current_log_likelihood = log_likelihood_random_effect(
    current, distribution, X, target, Sigma_inv
  );
  const double log_slice = current_log_likelihood +
    std::log(std::max(unif_rand(), 1e-300));
  const arma::vec direction = oeli::rmvnorm(
    arma::zeros<arma::vec>(current.n_elem), covariance
  );
  const arma::vec centered = current - mean;
  double angle = 2.0 * arma::datum::pi * unif_rand();
  double lower = angle - 2.0 * arma::datum::pi;
  double upper = angle;
  for (int attempt = 0; attempt < 1000; ++attempt) {
    arma::vec proposal = mean + centered * std::cos(angle) +
      direction * std::sin(angle);
    const double proposal_log_likelihood = log_likelihood_random_effect(
      proposal, distribution, X, target, Sigma_inv
    );
    if (std::isfinite(proposal_log_likelihood) &&
        proposal_log_likelihood >= log_slice) {
      return proposal;
    }
    if (angle < 0.0) lower = angle; else upper = angle;
    angle = lower + (upper - lower) * unif_rand();
  }
  return current;
}

//' Update coefficient distributions
//'
//' @description
//' Low-level Gibbs sampler kernels for coefficient means and covariance
//' matrices.
//'
//' @param bar_b_c \[`numeric(P)`\]\cr
//' Average individual coefficient vector in one class.
//'
//' @param Omega_c \[`matrix(P, P)`\]\cr
//' Covariance matrix of one class.
//'
//' @param m_c \[`integer(1)`\]\cr
//' Size of one class.
//'
//' @param Sigma_b_0_inv \[`matrix(P, P)`\]\cr
//' Prior precision matrix for class means.
//'
//' @param mu_b_0 \[`numeric(P)`\]\cr
//' Prior mean for class means.
//'
//' @param beta \[`matrix(P, N)`\]\cr
//' Individual coefficient draws in columns.
//'
//' @param Omega \[`matrix(P * P, C)`\]\cr
//' Vectorized class covariance matrices in columns.
//'
//' @param z \[`numeric(N)`\]\cr
//' Class allocations numbered from one to `C`.
//'
//' @param m \[`numeric(C)`\]\cr
//' Class sizes.
//'
//' @param S_c \[`matrix(P, P)`\]\cr
//' Scatter matrix for one class.
//'
//' @param n_Omega_0 \[`integer(1)`\]\cr
//' Prior degrees of freedom for class covariances.
//'
//' @param V_Omega_0 \[`matrix(P, P)`\]\cr
//' Prior scale matrix for class covariances.
//'
//' @param correlated \[`logical(P)` | `NULL`\]\cr
//' Which random effects are correlated. Covariances between the other
//' effects are zero. By default (`NULL`), all random effects are correlated.
//'
//' @param b \[`matrix(P, C)`\]\cr
//' Class means in columns.
//'
//' @param mu_beta_0 \[`numeric(P)`\]\cr
//' Prior mean for a coefficient vector.
//'
//' @param Sigma_beta_0_inv \[`matrix(P, P)`\]\cr
//' Prior precision matrix for a coefficient vector.
//'
//' @param XSigX \[`matrix(P, P)`\]\cr
//' Sum of design cross-products weighted by inverse error covariance.
//'
//' @param XSigU \[`numeric(P)`\]\cr
//' Sum of design-utility products weighted by inverse error covariance.
//'
//' @return
//' The functions return one sampler update:
//'
//' - `update_b_c()`: a `P` by 1 numeric matrix containing a class mean.
//' - `update_b()`: a `P` by `C` matrix of class means.
//' - `update_Omega_c()`: a `P` by `P` class covariance matrix.
//' - `update_Omega()`: a `P * P` by `C` matrix of vectorized covariances.
//' - `update_coefficient()`: a `P` by 1 numeric coefficient matrix.
//'
//' @keywords models
//'
//' @examples
//' ### four deciders with one random coefficient in two classes
//' set.seed(1)
//' beta <- matrix(c(-1, -1.2, 1, 1.3), nrow = 1)
//' Omega <- matrix(c(0.2, 0.2), nrow = 1)
//' z <- c(1, 1, 2, 2)
//' m <- c(2, 2)
//'
//' ### a coefficient from its conditional posterior
//' update_coefficient(c(0, 0), diag(2), diag(2), c(0, 0))
//'
//' ### the class means, for one class and for all classes at once
//' update_b_c(
//'   bar_b_c = c(0, 0), Omega_c = diag(2), m_c = 4,
//'   Sigma_b_0_inv = diag(2), mu_b_0 = c(0, 0)
//' )
//' update_b(beta, Omega, z, m, Sigma_b_0_inv = diag(1), mu_b_0 = 0)
//'
//' ### the class covariances
//' update_Omega_c(
//'   S_c = diag(2), m_c = 4, n_Omega_0 = 4, V_Omega_0 = diag(2),
//'   correlated = c(TRUE, TRUE)
//' )
//' update_Omega(
//'   beta, b = matrix(c(-1, 1), nrow = 1), z, m,
//'   n_Omega_0 = 4, V_Omega_0 = diag(1)
//' )
//'
//' @rdname coefficient_updates
//' @export
// [[Rcpp::export]]
arma::mat update_b_c (
   arma::vec bar_b_c, arma::mat Omega_c, int m_c,
   arma::mat Sigma_b_0_inv, arma::vec mu_b_0
) {
  arma::mat Omega_c_inv = inv_spd(Omega_c);
  arma::mat Sigma_b_c = inv_spd(Sigma_b_0_inv + m_c * Omega_c_inv);
  arma::vec mu_b_c = Sigma_b_c *
   (Sigma_b_0_inv * mu_b_0 + m_c * Omega_c_inv * bar_b_c);
  return oeli::rmvnorm(mu_b_c, Sigma_b_c);
}

//' @rdname coefficient_updates
//' @export
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
       mu_b_0, inv_spd(Sigma_b_0_inv)
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

//' @rdname coefficient_updates
//' @export
// [[Rcpp::export]]
arma::mat update_Omega_c (
    arma::mat S_c, int m_c, int n_Omega_0, arma::mat V_Omega_0,
    arma::uvec correlated
) {
  const int P = static_cast<int>(S_c.n_rows);
  arma::mat draw(P, P, arma::fill::zeros);
  // the correlated effects form one block, every other effect its own
  std::vector<arma::uvec> blocks;
  const arma::uvec joint = arma::find(correlated == 1u);
  if (joint.n_elem > 0) blocks.push_back(joint);
  const arma::uvec single = arma::find(correlated == 0u);
  for (arma::uword i = 0; i < single.n_elem; ++i) {
    blocks.push_back(arma::uvec{single[i]});
  }
  for (const arma::uvec& block : blocks) {
    const arma::mat scale =
      V_Omega_0.submat(block, block) + S_c.submat(block, block);
    const int dof = std::max(
      n_Omega_0 + std::max(m_c, 0), static_cast<int>(block.n_elem) + 1
    );
    draw.submat(block, block) = oeli::rwishart(dof, scale, true);
  }
  return draw;
}

//' @rdname coefficient_updates
//' @export
// [[Rcpp::export]]
arma::mat update_Omega (
   arma::mat beta, arma::mat b, arma::vec z, arma::vec m,
   int n_Omega_0, arma::mat V_Omega_0,
   Rcpp::Nullable<Rcpp::IntegerVector> correlated = R_NilValue
) {
  const arma::uvec flags = optional_flags(
    correlated, beta.n_rows, "correlated", "random effect"
  );
  const int P_r = static_cast<int>(beta.n_rows);
  const int C = static_cast<int>(m.n_elem);
  const int N = static_cast<int>(beta.n_cols);
  arma::mat draw(P_r * P_r, C, arma::fill::zeros);
  for (int c = 0; c < C; ++c) {
    arma::mat S_c(P_r, P_r, arma::fill::zeros);
    for (int n = 0; n < N; ++n) {
      if (static_cast<int>(std::lround(z[n])) == c + 1) {
        arma::vec centered = beta.col(n) - b.col(c);
        S_c += centered * centered.t();
      }
    }
    arma::mat covariance = update_Omega_c(
      S_c, static_cast<int>(std::lround(m[c])), n_Omega_0, V_Omega_0, flags
    );
    draw.col(c) = arma::vectorise(covariance);
  }
  return draw;
}

// class updates
//' @rdname class_updates
//' @export
// [[Rcpp::export]]
arma::vec update_s (double delta, arma::vec m) {
  return oeli::rdirichlet(delta * arma::ones(m.size()) + m);
}

double update_delta_finite(
    double delta, arma::vec const& s, double shape, double rate,
    double step_scale
) {
  const double log_current = std::log(delta);
  const double log_proposal = log_current + step_scale * norm_rand();
  const double proposal = std::exp(log_proposal);
  const double log_ratio =
    log_density_delta(proposal, s, shape, rate) + log_proposal -
    log_density_delta(delta, s, shape, rate) - log_current;
  if (log_ratio >= 0.0 ||
      std::log(std::max(unif_rand(), 1e-300)) < log_ratio) {
    return proposal;
  }
  return delta;
}

double update_delta_dp(
    double delta, int N, int C, double shape, double rate
) {
  const double eta = R::rbeta(delta + 1.0, static_cast<double>(N));
  const double updated_rate = rate - std::log(std::max(eta, 1e-300));
  const double numerator = shape + static_cast<double>(C) - 1.0;
  const double probability = numerator /
    (numerator + static_cast<double>(N) * updated_rate);
  const bool upper_component = unif_rand() < probability;
  const double updated_shape = shape + static_cast<double>(C) -
    (upper_component ? 0.0 : 1.0);
  return R::rgamma(updated_shape, 1.0 / updated_rate);
}

arma::vec allocate_classes(
    const arma::vec& s, const arma::mat& loglik, arma::vec z, bool nonempty
) {
  const int N = static_cast<int>(loglik.n_rows);
  const int C = static_cast<int>(loglik.n_cols);
  arma::vec counts(C, arma::fill::zeros);
  if (nonempty) {
    for (int n = 0; n < N; ++n) {
      const int label = static_cast<int>(std::lround(z[n])) - 1;
      if (label >= 0 && label < C) counts[label] += 1.0;
    }
  }
  arma::vec logp(C);
  for (int n = 0; n < N; ++n) {
    if (nonempty) {
      const int old_class = static_cast<int>(std::lround(z[n])) - 1;
      counts[old_class] -= 1.0;
      if (counts[old_class] <= 0.0) {
        counts[old_class] += 1.0;
        continue;
      }
    }
    for (int c = 0; c < C; ++c) {
      logp[c] = std::log(std::max(1e-300, s[c])) + loglik(n, c);
    }
    const double maximum = logp.max();
    arma::vec probability = arma::exp(logp - maximum);
    const double total = arma::accu(probability);
    if (!std::isfinite(total) || total <= 0.0) probability.ones();
    const int new_class = sample_allocation(probability) - 1;
    z[n] = static_cast<double>(new_class + 1);
    if (nonempty) counts[new_class] += 1.0;
  }
  return z;
}

//' @rdname class_updates
//' @export
// [[Rcpp::export]]
arma::vec update_z (
   arma::vec s, arma::mat beta, arma::mat b, arma::mat Omega
) {
  const int N = static_cast<int>(beta.n_cols);
  const int C = static_cast<int>(s.n_elem);
  const arma::mat loglik = log_likelihood_mixture(
    beta, b, Omega, arma::mat(0, C), ClassLogLikelihood(), N, C
  );
  return allocate_classes(s, loglik, arma::vec(N, arma::fill::ones), false);
}

//' @rdname class_updates
//' @export
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

static Rcpp::List update_classes_wb_lc(
   arma::vec s, arma::mat b, arma::mat Omega, arma::mat lambda,
   double epsmin, double epsmax, double deltamin, double deltashift,
   bool identify_classes, int Cmax
) {
  int update_type = 0;
  int C = static_cast<int>(s.n_elem);
  const int P = static_cast<int>(b.n_rows);
  const int P_l = static_cast<int>(lambda.n_rows);
  arma::mat stack = arma::join_cols(
    arma::trans(s), arma::join_cols(b, arma::join_cols(Omega, lambda))
  );
  arma::uvec location(P + P_l);
  for (int p = 0; p < P; ++p) location[p] = 1 + p;
  for (int p = 0; p < P_l; ++p) location[P + p] = 1 + P + P * P + p;

  // remove the component with the smallest weight
  const arma::uword id_min = arma::index_min(stack.row(0));
  if (C > 1 && stack(0, id_min) < epsmin) {
    --C;
    stack.shed_col(id_min);
    stack.row(0) /= arma::accu(stack.row(0));
    update_type = 1;
  }

  // split the component with the largest weight along its leading
  // covariance eigenvector
  if (update_type == 0 && C < Cmax) {
    const arma::uword id_max = arma::index_max(stack.row(0));
    if (stack(0, id_max) > epsmax) {
      stack.insert_cols(id_max, stack.col(id_max));
      stack(0, arma::span(id_max, id_max + 1)) /= 2.0;
      if (P > 0) {
        const arma::mat covariance = arma::reshape(
          stack(arma::span(P + 1, P + P * P), id_max), P, P
        );
        arma::vec eigenvalues;
        arma::mat eigenvectors;
        arma::eig_sym(eigenvalues, eigenvectors, covariance);
        const arma::vec direction = eigenvectors.col(P - 1);
        const double leading = std::max(0.0, eigenvalues(P - 1));
        const arma::vec shift = deltashift * std::sqrt(leading) * direction;
        stack(arma::span(1, P), id_max) += shift;
        stack(arma::span(1, P), id_max + 1) -= shift;
      }
      ++C;
      update_type = 2;
    }
  }

  // merge the two components with the closest means and class-specific
  // coefficients
  if (update_type == 0 && C > 1) {
    double minimum_distance = std::numeric_limits<double>::infinity();
    arma::uword first = 0;
    arma::uword second = 0;
    arma::uvec column_1(1);
    arma::uvec column_2(1);
    for (int c1 = 0; c1 < C; ++c1) {
      for (int c2 = 0; c2 < c1; ++c2) {
        column_1[0] = static_cast<arma::uword>(c1);
        column_2[0] = static_cast<arma::uword>(c2);
        const arma::mat difference =
          stack(location, column_1) - stack(location, column_2);
        const double distance = arma::norm(difference, 2);
        if (distance < minimum_distance) {
          minimum_distance = distance;
          first = static_cast<arma::uword>(c1);
          second = static_cast<arma::uword>(c2);
        }
      }
    }
    if (minimum_distance < deltamin) {
      column_1[0] = first;
      column_2[0] = second;
      stack(0, first) += stack(0, second);
      const arma::mat merged =
        (stack(location, column_1) + stack(location, column_2)) / 2.0;
      stack(location, column_1) = merged;
      if (P > 0) {
        stack(arma::span(P + 1, P + P * P), first) +=
          stack(arma::span(P + 1, P + P * P), second);
        stack(arma::span(P + 1, P + P * P), first) /= 2.0;
      }
      stack.shed_col(second);
      --C;
      update_type = 3;
    }
  }

  if (identify_classes) {
    stack = stack.cols(arma::sort_index(stack.row(0), "descend"));
  }

  arma::mat b_out(P, C);
  arma::mat Omega_out(P * P, C);
  arma::mat lambda_out(P_l, C);
  if (P > 0) {
    b_out = stack.rows(1, P);
    Omega_out = stack.rows(P + 1, P + P * P);
  }
  if (P_l > 0) lambda_out = stack.rows(P + P * P + 1, P + P * P + P_l);
  return Rcpp::List::create(
    Rcpp::Named("s") = stack.row(0),
    Rcpp::Named("b") = b_out,
    Rcpp::Named("Omega") = Omega_out,
    Rcpp::Named("lambda") = lambda_out,
    Rcpp::Named("update_type") = update_type
  );
}

//' @rdname class_updates
//' @export
// [[Rcpp::export]]
Rcpp::List update_classes_wb(
   arma::vec s, arma::mat b, arma::mat Omega, double epsmin = 0.01,
   double epsmax = 0.7, double deltamin = 0.1, double deltashift = 0.5,
   bool identify_classes = false, int Cmax = 10
) {
  Rcpp::List update = update_classes_wb_lc(
    s, b, Omega, arma::mat(0, b.n_cols), epsmin, epsmax, deltamin,
    deltashift, identify_classes, Cmax
  );
  return Rcpp::List::create(
    Rcpp::Named("s") = update["s"],
    Rcpp::Named("b") = update["b"],
    Rcpp::Named("Omega") = update["Omega"],
    Rcpp::Named("update_type") = update["update_type"]
  );
}

static Rcpp::List update_classes_dp_lc(
   arma::mat beta, arma::vec z, arma::mat b, arma::mat Omega,
   arma::mat lambda, double delta, arma::vec mu_b_0, arma::mat Sigma_b_0,
   int n_Omega_0, arma::mat V_Omega_0, const arma::uvec& correlated,
   arma::vec mu_lambda_0, arma::mat Sigma_lambda_0,
   const ClassLogLikelihood& log_likelihood_class, bool identify_classes,
   int Cmax
) {
  const int N = static_cast<int>(z.n_elem);
  const int P_r = static_cast<int>(b.n_rows);
  const int P_l = static_cast<int>(lambda.n_rows);
  int C = static_cast<int>(std::max(b.n_cols, lambda.n_cols));
  arma::mat b_full(P_r, Cmax, arma::fill::zeros);
  arma::mat Omega_full(P_r * P_r, Cmax, arma::fill::zeros);
  arma::mat lambda_full(P_l, Cmax, arma::fill::zeros);
  arma::vec m_full(Cmax, arma::fill::zeros);
  if (C > 0) {
    b_full.cols(0, C - 1) = b;
    Omega_full.cols(0, C - 1) = Omega;
    lambda_full.cols(0, C - 1) = lambda;
    m_full.subvec(0, C - 1) = update_m(C, z, false);
  }

  for (int n = 0; n < N; ++n) {

   // unassign current class membership
   int z_n = static_cast<int>(std::lround(z[n]));
   if (z_n >= 1 && z_n <= C) {
     m_full[static_cast<arma::uword>(z_n - 1)] -= 1.0;
   }

   // remove an emptied class by moving the last class into its slot
   bool rm = z_n >= 1 && z_n <= C &&
     m_full[static_cast<arma::uword>(z_n - 1)] <= 0.0;
   if (rm) {
     const arma::uword slot = static_cast<arma::uword>(z_n - 1);
     const arma::uword last = static_cast<arma::uword>(C - 1);
     if (C > 1) {
       for (int i = 0; i < N; ++i) {
         int zi = static_cast<int>(std::lround(z[i]));
         if (zi == C) z[i] = static_cast<double>(z_n);
       }
       m_full[slot] = m_full[last];
       b_full.col(slot) = b_full.col(last);
       Omega_full.col(slot) = Omega_full.col(last);
       lambda_full.col(slot) = lambda_full.col(last);
     }
     m_full[last] = 0.0;
     b_full.col(last).zeros();
     Omega_full.col(last).zeros();
     lambda_full.col(last).zeros();
     C -= 1;
   }

   // compute class allocation posterior
   const bool at_cap = (C >= Cmax);
   const int auxiliaries = at_cap ? 0 : 10;
   const int total = C + auxiliaries;
   arma::vec logp(total, arma::fill::zeros);
   arma::mat b_aux(P_r, auxiliaries, arma::fill::zeros);
   arma::mat Omega_aux(P_r * P_r, auxiliaries, arma::fill::zeros);
   arma::mat lambda_aux(P_l, auxiliaries, arma::fill::zeros);

   // existing classes
   for (int c = 0; c < C; ++c) {
     const double mc = m_full[static_cast<arma::uword>(c)];
     if (mc <= 0.0) {
       logp[c] = -std::numeric_limits<double>::infinity();
       continue;
     }
     double loglik = 0.0;
     if (P_r > 0) {
       arma::mat Sigma_c = arma::reshape(
         Omega_full.col(static_cast<arma::uword>(c)), P_r, P_r
       );
       loglik += oeli::dmvnorm(
         beta.col(n), b_full.col(static_cast<arma::uword>(c)), Sigma_c, true
       );
     }
     if (P_l > 0) {
       loglik += log_likelihood_class(
         n, lambda_full.col(static_cast<arma::uword>(c))
       );
     }
     logp[c] = std::log(mc) + loglik;
   }

   // auxiliary proposals for one possible new class
   for (int auxiliary = 0; auxiliary < auxiliaries; ++auxiliary) {
     double loglik = 0.0;
     if (P_r > 0) {
       b_aux.col(auxiliary) = oeli::rmvnorm(mu_b_0, Sigma_b_0);
       arma::mat covariance = update_Omega_c(
         arma::zeros<arma::mat>(P_r, P_r), 0, n_Omega_0,
         V_Omega_0, correlated
       );
       Omega_aux.col(auxiliary) = arma::vectorise(covariance);
       loglik += oeli::dmvnorm(
         beta.col(n), b_aux.col(auxiliary), covariance, true
       );
     }
     if (P_l > 0) {
       lambda_aux.col(auxiliary) =
         oeli::rmvnorm(mu_lambda_0, Sigma_lambda_0);
       loglik += log_likelihood_class(n, lambda_aux.col(auxiliary));
     }
     logp[C + auxiliary] = std::log(delta) - std::log(auxiliaries) + loglik;
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
   const int selected = sample_allocation(p) - 1;
   const bool creates_class = selected >= C;
   const int z_new = creates_class ? C : selected;
   z[n] = static_cast<double>(z_new + 1);
   m_full[static_cast<arma::uword>(z_new)] += 1.0;

   // generate new class
   if (creates_class) {
     const int auxiliary = selected - C;
     C += 1;
     const arma::uword last = static_cast<arma::uword>(C - 1);
     b_full.col(last) = b_aux.col(auxiliary);
     Omega_full.col(last) = Omega_aux.col(auxiliary);
     lambda_full.col(last) = lambda_aux.col(auxiliary);
   }
  }

  // identify classes
  arma::mat b_update = b_full.cols(0, std::max(0, C - 1));
  arma::mat Omega_update = Omega_full.cols(0, std::max(0, C - 1));
  arma::mat lambda_update = lambda_full.cols(0, std::max(0, C - 1));
  if (identify_classes) {
   arma::vec m = update_m(C, z, false);
   arma::uvec perm = arma::sort_index(m, "descend");
   b_update = b_update.cols(perm);
   Omega_update = Omega_update.cols(perm);
   lambda_update = lambda_update.cols(perm);
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
   Rcpp::Named("lambda") = lambda_update,
   Rcpp::Named("C") = C
  );
}

//' @rdname class_updates
//' @export
// [[Rcpp::export]]
Rcpp::List update_classes_dp(
   arma::mat beta, arma::vec z, arma::mat b, arma::mat Omega,
   double delta, arma::vec mu_b_0, arma::mat Sigma_b_0, int n_Omega_0,
   arma::mat V_Omega_0, bool identify_classes = false, int Cmax = 10
) {
  arma::uvec correlated(b.n_rows, arma::fill::ones);
  Rcpp::List update = update_classes_dp_lc(
    beta, z, b, Omega, arma::mat(0, b.n_cols), delta, mu_b_0, Sigma_b_0,
    n_Omega_0, V_Omega_0, correlated, arma::vec(), arma::mat(),
    ClassLogLikelihood(), identify_classes, Cmax
  );
  // the class parameters are updated given the new allocation
  arma::vec z_update = Rcpp::as<arma::vec>(update["z"]);
  const int C = Rcpp::as<int>(update["C"]);
  arma::vec m = update_m(C, z_update, false);
  arma::mat b_update = update_b(
    beta, Rcpp::as<arma::mat>(update["Omega"]), z_update, m,
    inv_spd(Sigma_b_0), mu_b_0
  );
  arma::mat Omega_update = update_Omega(
    beta, b_update, z_update, m, n_Omega_0, V_Omega_0
  );
  return Rcpp::List::create(
    Rcpp::Named("z") = z_update,
    Rcpp::Named("b") = b_update,
    Rcpp::Named("Omega") = Omega_update,
    Rcpp::Named("C") = C
  );
}

//' Update utilities and thresholds
//'
//' @description
//' Low-level Gibbs sampler kernels for error covariance matrices, latent
//' utilities, and ordered-response thresholds.
//'
//' @param n_Sigma_0 \[`integer(1)`\]\cr
//' Prior degrees of freedom for the error covariance.
//'
//' @param V_Sigma_0 \[`matrix(J - 1, J - 1)`\]\cr
//' Prior scale matrix for the error covariance.
//'
//' @param N \[`integer(1)`\]\cr
//' Number of independent sampling units.
//'
//' @param S \[`matrix(J - 1, J - 1)`\]\cr
//' Error scatter matrix.
//'
//' @param U \[`numeric(J - 1)`\]\cr
//' Current latent utility differences.
//'
//' @param y \[`integer(1)` | `matrix(N, max(Tvec))`\]\cr
//' Chosen alternative for `update_U()`, where `J` denotes the base
//' alternative, or ordered responses by decider and occasion for the
//' threshold functions.
//'
//' @param sys \[`numeric(J - 1)` | `matrix(N, max(Tvec))`\]\cr
//' Systematic utilities matching `U` or `y`.
//'
//' @param Sigma_inv \[`matrix(J - 1, J - 1)`\]\cr
//' Inverse error covariance matrix.
//'
//' @param available \[`logical(J)` | `NULL`\]\cr
//' Availability of the `J - 1` non-base alternatives followed by the base
//' alternative. Utilities of unavailable alternatives are drawn without
//' truncation. By default (`NULL`), all alternatives are available.
//'
//' @param d \[`numeric(J - 2)`\]\cr
//' Log-increments between finite ordered thresholds.
//'
//' @param Tvec \[`integer(N)`\]\cr
//' Number of observed occasions for each decider.
//'
//' @param log_likelihood \[`numeric(1)`\]\cr
//' Current ordered-response log-likelihood.
//'
//' @param mu_d_0 \[`numeric(J - 2)`\]\cr
//' Prior mean for threshold log-increments.
//'
//' @param Sigma_d_0 \[`matrix(J - 2, J - 2)`\]\cr
//' Prior covariance for threshold log-increments.
//'
//' @param step_scale \[`numeric(1)`\]\cr
//' Random-walk proposal standard deviation.
//'
//' @return
//' The functions return one sampler update or transformation:
//'
//' - `update_Sigma()`: a `J - 1` by `J - 1` covariance matrix.
//' - `update_U()` and `update_U_ranked()`: `J - 1` by 1 numeric latent
//'   utility matrices.
//' - `d_to_gamma()`: a numeric column matrix containing ordered thresholds
//'   and their infinite bounds.
//' - `log_likelihood_ordered()`: one numeric log-likelihood value.
//' - `update_d()`: a list with updated `d` and `log_likelihood` values.
//'
//' @references
//' \insertRef{Robert1995}{RprobitB}
//'
//' @keywords models
//'
//' @examples
//' ### two deciders who choose twice on an ordered scale of four levels
//' set.seed(1)
//' d <- c(0, log(2))
//' y <- matrix(c(1, 2, 3, 2), nrow = 2)
//' sys <- matrix(0, nrow = 2, ncol = 2)
//' Tvec <- c(2, 2)
//'
//' ### the thresholds, their likelihood, and their random-walk update
//' d_to_gamma(d)
//' log_likelihood <- log_likelihood_ordered(d, y, sys, Tvec)
//' update_d(
//'   d, y, sys, log_likelihood, mu_d_0 = c(0, 0), Sigma_d_0 = diag(2),
//'   Tvec = Tvec
//' )
//'
//' ### the latent utilities of an unordered and of a ranked choice
//' update_U(
//'   c(0, 0), y = 1, sys = c(0, 0), Sigma_inv = diag(2),
//'   available = c(TRUE, TRUE, TRUE)
//' )
//' update_U_ranked(c(0, 0), sys = c(0, 0), Sigma_inv = diag(2))
//'
//' ### the error covariance
//' update_Sigma(n_Sigma_0 = 4, V_Sigma_0 = diag(2), N = 10, S = diag(2))
//'
//' @rdname utility_updates
//' @export
// [[Rcpp::export]]
arma::mat update_Sigma (
   int n_Sigma_0, arma::mat V_Sigma_0, int N, arma::mat S
) {
  return oeli::rwishart(n_Sigma_0 + N, V_Sigma_0 + S, true);
}

//' @rdname utility_updates
//' @export
// [[Rcpp::export]]
arma::vec update_U (
  arma::vec U, int y, arma::vec sys, arma::mat Sigma_inv,
  Rcpp::Nullable<Rcpp::IntegerVector> available = R_NilValue
) {
  const arma::uvec flags = optional_flags(
    available, U.n_elem + 1, "available", "alternative"
  );
  const int Jm1 = U.size();
  const bool base_available = flags[Jm1] == 1u;
  arma::vec U_update = U;
  for (int i = 0; i < Jm1; ++i) {
    double m = 0.0;
    const double sii = Sigma_inv(i, i);
    for (int k = 0; k < Jm1; ++k) if (k != i) {
      m += -1.0 / sii * Sigma_inv(i, k) * (U_update[k] - sys[k]);
    }
    const double mean = sys[i] + m;
    const double sd = std::sqrt(1.0 / sii);
    if (flags[i] != 1u) {
      U_update[i] = mean + sd * norm_rand();
      continue;
    }
    bool bounded = base_available;
    double bound = base_available ? 0.0 :
      -std::numeric_limits<double>::infinity();
    for (int j = 0; j < Jm1; ++j) if (j != i && flags[j] == 1u) {
      bound = std::max(bound, U_update[j]);
      bounded = true;
    }
    if (!bounded) {
      U_update[i] = mean + sd * norm_rand();
      continue;
    }
    U_update[i] = oeli::rtnorm(mean, sd, bound, y != (i + 1));
  }
  return U_update;
}

//' @rdname utility_updates
//' @export
// [[Rcpp::export]]
arma::vec update_U_ranked (
  arma::vec U, arma::vec sys, arma::mat Sigma_inv
) {
  int Jm1 = U.size();
  arma::vec U_update = U;
  for (int i = 0; i < Jm1; ++i) {
    double m = 0.0;
    const double sii = Sigma_inv(i, i);
    for (int k = 0; k < Jm1; ++k) if (k != i) {
      m += -1.0 / sii * Sigma_inv(i, k) * (U_update[k] - sys[k]);
    }
    U_update[i] = oeli::rtnorm(
      sys[i] + m, std::sqrt(1.0 / sii), 0.0, true
    );
  }
  return U_update;
}

//' @rdname utility_updates
//' @export
// [[Rcpp::export]]
Rcpp::List update_d (
   arma::vec d, arma::mat const& y, arma::mat const& sys,
   double log_likelihood, arma::vec const& mu_d_0,
   arma::mat const& Sigma_d_0, arma::vec const& Tvec, double step_scale = 0.1
) {
  const arma::uword K = d.n_elem;
  arma::vec step(K);
  for (arma::uword k = 0; k < K; ++k) step[k] = step_scale * norm_rand();
  arma::vec d_cand = d + step;
  double log_likelihood_cand = log_likelihood_ordered(d_cand, y, sys, Tvec);
  const double log_prior_curr =
    oeli::dmvnorm(d, mu_d_0, Sigma_d_0, true);
  const double log_prior_cand =
    oeli::dmvnorm(d_cand, mu_d_0, Sigma_d_0, true);
  const double log_alpha = (log_likelihood_cand - log_likelihood) +
    (log_prior_cand - log_prior_curr);
  if (log_alpha >= 0.0 || std::log(unif_rand()) <= log_alpha) {
    d  = std::move(d_cand);
    log_likelihood = log_likelihood_cand;
  }
  return Rcpp::List::create(
    Rcpp::Named("d") = d, Rcpp::Named("log_likelihood") = log_likelihood
  );
}

// [[Rcpp::export]]
Rcpp::List gibbs_sampler (
   Rcpp::List sufficient_statistics, Rcpp::List prior,
   Rcpp::List latent_classes,
   int R, int B, bool ordered, bool ranked, bool save_beta_draws,
   Rcpp::Nullable<Rcpp::Function> progress = R_NilValue
) {

  // extract 'sufficient_statistics' parameters
  int N = Rcpp::as<int>(sufficient_statistics["N"]);
  int J = Rcpp::as<int>(sufficient_statistics["J"]);
  int P_f = Rcpp::as<int>(sufficient_statistics["P_f"]);
  int P_r = Rcpp::as<int>(sufficient_statistics["P_r"]);
  int P_l = 0;
  if (sufficient_statistics.containsElementNamed("P_l")) {
    P_l = Rcpp::as<int>(sufficient_statistics["P_l"]);
  }
  // a mixture model has random effects, class-specific coefficients, or both
  const bool mixture = P_r > 0 || P_l > 0;
  arma::ivec random_distribution(P_r, arma::fill::zeros);
  arma::uvec random_correlated(P_r, arma::fill::ones);
  if (P_r > 0 &&
      sufficient_statistics.containsElementNamed("random_distribution")) {
    Rcpp::IntegerVector distribution =
      sufficient_statistics["random_distribution"];
    for (int p = 0; p < P_r; ++p) random_distribution[p] = distribution[p];
  }
  if (P_r > 0 &&
      sufficient_statistics.containsElementNamed("random_correlated")) {
    Rcpp::LogicalVector correlated = sufficient_statistics["random_correlated"];
    for (int p = 0; p < P_r; ++p) {
      random_correlated[p] = correlated[p] == TRUE ? 1u : 0u;
    }
  }
  // random effects with a latent class effect have class-specific means and
  // covariances, the other random effects share one distribution
  arma::uvec class_specific(P_r, arma::fill::ones);
  if (P_r > 0 &&
      sufficient_statistics.containsElementNamed("random_class_specific")) {
    Rcpp::LogicalVector flags = sufficient_statistics["random_class_specific"];
    for (int p = 0; p < P_r; ++p) {
      class_specific[p] = flags[p] == TRUE ? 1u : 0u;
    }
  }
  arma::uvec cs, co;
  if (P_r > 0 && mixture) {
    cs = arma::find(class_specific == 1u);
    co = arma::find(class_specific == 0u);
  } else if (P_r > 0) {
    cs = arma::regspace<arma::uvec>(0, P_r - 1);
  }
  const int P_cs = static_cast<int>(cs.n_elem);
  const int P_co = static_cast<int>(co.n_elem);
  const Rcpp::IntegerVector correlated_cs = Rcpp::wrap(
    arma::conv_to<arma::ivec>::from(random_correlated.elem(cs))
  );
  const Rcpp::IntegerVector correlated_co = Rcpp::wrap(
    arma::conv_to<arma::ivec>::from(random_correlated.elem(co))
  );
  const bool all_random_normal = arma::all(random_distribution == 0);
  arma::vec Tvec = Rcpp::as<arma::vec>(sufficient_statistics["Tvec"]);
  arma::vec csTvec = Rcpp::as<arma::vec>(sufficient_statistics["csTvec"]);
  Rcpp::List W;
  Rcpp::List L;
  Rcpp::List X;
  arma::mat y = Rcpp::as<arma::mat>(sufficient_statistics["y"]);
  arma::umat available;
  if (sufficient_statistics.containsElementNamed("available") &&
      !Rf_isNull(sufficient_statistics["available"])) {
    available = Rcpp::as<arma::umat>(sufficient_statistics["available"]);
  }
  arma::mat WkW;
  Rcpp::List LkL;
  Rcpp::List XkX;
  Rcpp::List rdiff;
  if (P_f > 0) {
    W = Rcpp::as<Rcpp::List>(sufficient_statistics["W"]);
    WkW = Rcpp::as<arma::mat>(sufficient_statistics["WkW"]);
  }
  if (P_l > 0) {
    L = Rcpp::as<Rcpp::List>(sufficient_statistics["L"]);
    LkL = Rcpp::as<Rcpp::List>(sufficient_statistics["LkL"]);
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
    const int L_size = rdiff.size();
    rdiff_list.resize(L_size);
    for (int i = 0; i < L_size; ++i) {
      rdiff_list[i] = Rcpp::as<arma::mat>(rdiff[i]);
    }
    rdiff_inv_list.resize(L_size);
    for (int i = 0; i < L_size; ++i) {
      rdiff_inv_list[i] = arma::pinv(rdiff_list[i]);
    }
  }
  std::vector<arma::mat> Wv, Lv, Xv;
  const int NT = static_cast<int>(arma::accu(Tvec));
  if (P_f > 0) {
    Wv.resize(NT);
    for (int i = 0; i < NT; ++i) Wv[i] = Rcpp::as<arma::mat>(W[i]);
  }
  if (P_l > 0) {
    Lv.resize(NT);
    for (int i = 0; i < NT; ++i) Lv[i] = Rcpp::as<arma::mat>(L[i]);
  }
  if (P_r > 0) {
    Xv.resize(NT);
    for (int i = 0; i < NT; ++i) Xv[i] = Rcpp::as<arma::mat>(X[i]);
  }
  const int rows = ordered ? 1 : (J - 1);
  arma::mat Wstack;
  if (P_f > 0) {
    Wstack.set_size(rows * NT, P_f);
    for (int i = 0; i < NT; ++i) {
      Wstack.rows(i * rows, (i + 1) * rows - 1) = Wv[i];
    }
  }
  std::vector<arma::mat> Lstack(N), Xstack(N);
  for (int n = 0; n < N; ++n) {
    const int Tn = static_cast<int>(std::lround(Tvec[n]));
    const int base = static_cast<int>(std::lround(csTvec[n]));
    if (P_l > 0) Lstack[n].set_size(rows * Tn, P_l);
    if (P_r > 0) Xstack[n].set_size(rows * Tn, P_r);
    for (int t = 0; t < Tn; ++t) {
      if (P_l > 0) {
        Lstack[n].rows(t * rows, (t + 1) * rows - 1) = Lv[base + t];
      }
      if (P_r > 0) {
        Xstack[n].rows(t * rows, (t + 1) * rows - 1) = Xv[base + t];
      }
    }
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
  std::vector<arma::mat> LkL_mats;
  if (P_l > 0) {
    LkL_mats.resize(N);
    for (int n = 0; n < N; ++n) {
      LkL_mats[n] = Rcpp::as<arma::mat>(LkL[n]);
      if (ordered) LkL_mats[n] = arma::reshape(LkL_mats[n], P_l, P_l);
    }
  }

  // the observed response of every choice occasion
  arma::ivec response_of(NT);
  for (int n = 0; n < N; ++n) {
    const int Tn = static_cast<int>(std::lround(Tvec[n]));
    const int base = static_cast<int>(std::lround(csTvec[n]));
    for (int t = 0; t < Tn; ++t) {
      response_of[base + t] = static_cast<int>(std::lround(y(n, t)));
    }
  }

  // extract 'latent_classes' parameters
  int C = Rcpp::as<int>(latent_classes["C"]);
  int Cmax = 10;
  bool weight_based_update =
    latent_classes.containsElementNamed("weight_based_update") &&
    Rcpp::as<bool>(latent_classes["weight_based_update"]);
  bool dp_update = Rcpp::as<bool>(latent_classes["dp_update"]);
  bool nonempty = Rcpp::as<bool>(latent_classes["nonempty"]);
  int buffer = 50;
  double epsmin = 0.01;
  double epsmax = 0.7;
  double deltamin = 0.1;
  double deltashift = 0.5;
  int Cdrawsize = C;
  if (dp_update || weight_based_update) {
    Cmax = Rcpp::as<int>(latent_classes["Cmax"]);
    Cdrawsize = Cmax;
  }
  if (weight_based_update) {
    buffer = Rcpp::as<int>(latent_classes["buffer"]);
    epsmin = Rcpp::as<double>(latent_classes["epsmin"]);
    epsmax = Rcpp::as<double>(latent_classes["epsmax"]);
    deltamin = Rcpp::as<double>(latent_classes["deltamin"]);
    deltashift = Rcpp::as<double>(latent_classes["deltashift"]);
  }

  // extract 'prior' parameters
  double delta = 1.0;
  bool sample_delta = false;
  double delta_shape = 1.0;
  double delta_rate = 1.0;
  arma::vec mu_alpha_0;
  arma::mat Sigma_alpha_0, Sigma_alpha_0_;
  arma::vec mu_lambda_0;
  arma::mat Sigma_lambda_0, Sigma_lambda_0_inv;
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
    Sigma_alpha_0_ = inv_spd(Sigma_alpha_0);
  }
  if (P_l > 0) {
    mu_lambda_0 = Rcpp::as<arma::vec>(prior["mu_lambda_0"]);
    Sigma_lambda_0 = Rcpp::as<arma::mat>(prior["Sigma_lambda_0"]);
    Sigma_lambda_0_inv = inv_spd(Sigma_lambda_0);
  }
  if (mixture) {
    delta = Rcpp::as<double>(prior["delta"]);
    sample_delta = Rcpp::as<bool>(prior["sample_delta"]);
    if (sample_delta) {
      delta_shape = Rcpp::as<double>(prior["delta_shape"]);
      delta_rate = Rcpp::as<double>(prior["delta_rate"]);
    }
  }
  if (P_r > 0) {
    mu_b_0 = Rcpp::as<arma::vec>(prior["mu_b_0"]);
    Sigma_b_0 = Rcpp::as<arma::mat>(prior["Sigma_b_0"]);
    Sigma_b_0_inv = inv_spd(Sigma_b_0);
    n_Omega_0 = Rcpp::as<int>(prior["n_Omega_0"]);
    V_Omega_0 = Rcpp::as<arma::mat>(prior["V_Omega_0"]);
  }
  if (ordered) {
    mu_d_0 = Rcpp::as<arma::vec>(prior["mu_d_0"]);
    Sigma_d_0 = Rcpp::as<arma::mat>(prior["Sigma_d_0"]);
  }

  // define helper variables
  const int Tmax = static_cast<int>(y.n_cols);
  arma::mat mu_mat = arma::zeros<arma::mat>(N, Tmax);
  int ind;
  int Jm1 = J - 1;
  double old_ll = 0.0;
  Rcpp::List threshold_update;
  arma::vec Sigmainv_vec;
  std::vector<arma::mat> Sigmainv_ranked;

  // allocate space for output
  arma::mat s_draws = arma::zeros<arma::mat>(R, Cdrawsize);
  arma::mat z_draws = arma::zeros<arma::mat>(R, N);
  arma::mat b_draws = arma::zeros<arma::mat>(R, P_r * Cdrawsize);
  arma::mat Omega_draws = arma::zeros<arma::mat>(R, P_r * P_r * Cdrawsize);
  arma::mat lambda_draws = arma::zeros<arma::mat>(R, P_l * Cdrawsize);
  Rcpp::List beta_draws(R);
  arma::mat alpha_draws = arma::zeros<arma::mat>(R, P_f);
  arma::mat Sigma_draws;
  if (ordered) {
    Sigma_draws = arma::zeros<arma::mat>(R, 1);
  } else {
    Sigma_draws = arma::zeros<arma::mat>(R, Jm1 * Jm1);
  }
  arma::mat d_draws = arma::zeros<arma::mat>(R, J - 2);
  arma::vec class_sequence(R, arma::fill::zeros);
  arma::vec delta_draws(R, arma::fill::zeros);

  // initialize Gibbs sampler
  arma::vec s;
  arma::vec z;
  arma::vec m;
  arma::mat b;
  arma::mat Omega;
  arma::mat lambda;
  arma::vec alpha;
  arma::mat beta;
  arma::mat U;
  arma::mat Sigma;
  arma::mat Sigma_inv;
  arma::vec d;
  arma::vec gamma;
  const int rowsU = ordered ? 1 : (J - 1);
  if (P_f > 0) {
    alpha = mu_alpha_0;
  } else {
    alpha.reset();
  }
  if (ordered) {
    Sigma = arma::ones<arma::mat>(1, 1);
    Sigma_inv = arma::ones<arma::mat>(1, 1);
  } else {
    Sigma = arma::eye(J - 1, J - 1);
    Sigma_inv = arma::eye(J - 1, J - 1);
  }
  if (ordered) {
    d = mu_d_0;
    gamma = d_to_gamma(d);
  } else {
    d.reset();
  }
  if (mixture) {
    s = arma::ones<arma::vec>(C) / static_cast<double>(C);
    z.set_size(N);
    for (int n = 0; n < N; ++n) z[n] = static_cast<double>((n % C) + 1);
    m = update_m(C, z, false);
  } else {
    s.set_size(0);
    z.set_size(0);
    m.set_size(0);
  }
  b.set_size(P_r, C);
  Omega.set_size(P_r * P_r, C);
  lambda.set_size(P_l, C);
  beta.set_size(P_r, N);
  beta.zeros();
  arma::vec b_co;
  arma::mat Omega_co;
  auto block = [&](const arma::mat& M, const arma::uvec& index) -> arma::mat {
    arma::mat out(index.n_elem * index.n_elem, M.n_cols);
    for (arma::uword c = 0; c < M.n_cols; ++c) {
      const arma::mat full = arma::reshape(M.col(c), P_r, P_r);
      out.col(c) = arma::vectorise(full.submat(index, index));
    }
    return out;
  };
  auto assemble = [&](const arma::mat& b_cs, const arma::mat& Omega_cs) {
    const int classes = static_cast<int>(Omega_cs.n_cols);
    b.zeros(P_r, classes);
    Omega.zeros(P_r * P_r, classes);
    for (int c = 0; c < classes; ++c) {
      arma::vec mean(P_r, arma::fill::zeros);
      arma::mat covariance(P_r, P_r, arma::fill::zeros);
      if (P_cs > 0) {
        mean.elem(cs) = b_cs.col(c);
        covariance.submat(cs, cs) = arma::reshape(Omega_cs.col(c), P_cs, P_cs);
      }
      if (P_co > 0) {
        mean.elem(co) = b_co;
        covariance.submat(co, co) = Omega_co;
      }
      b.col(c) = mean;
      Omega.col(c) = arma::vectorise(covariance);
    }
  };
  if (P_r > 0) {
    arma::mat b_cs(P_cs, C);
    arma::mat Omega_cs(P_cs * P_cs, C);
    const arma::vec mean_cs = mu_b_0.elem(cs);
    const arma::mat V_cs = V_Omega_0.submat(cs, cs);
    for (int c = 0; c < C; ++c) {
      Omega_cs.col(c) = arma::vectorise(V_cs);
      if (P_cs > 0) {
        b_cs.col(c) = C > 1 ? oeli::rmvnorm(mean_cs, V_cs) : mean_cs;
      }
    }
    b_co = mu_b_0.elem(co);
    Omega_co = V_Omega_0.submat(co, co);
    assemble(b_cs, Omega_cs);
    for (int n = 0; n < N; ++n) {
      int ci = std::max(
        0, std::min(C - 1, static_cast<int>(std::lround(z[n])) - 1)
      );
      beta.col(n) = b.col(ci);
    }
  }
  if (P_l > 0) {
    const arma::mat identity = arma::eye(P_l, P_l);
    for (int c = 0; c < C; ++c) {
      lambda.col(c) =
        C > 1 ? oeli::rmvnorm(mu_lambda_0, identity) : mu_lambda_0;
    }
  }
  U = arma::zeros<arma::mat>(rowsU, NT);

  if (P_co > 0) {
    const arma::vec first_mean = b.col(0);
    b_co = first_mean.elem(co);
    const arma::mat first_covariance = arma::reshape(Omega.col(0), P_r, P_r);
    Omega_co = first_covariance.submat(co, co);
  }
  // the ordered model has a single utility, whose variance is fixed to one
  if (ordered) {
    Sigma = arma::ones<arma::mat>(1, 1);
    Sigma_inv = arma::ones<arma::mat>(1, 1);
  }

  // the class of a decider
  auto class_of = [&](int n) -> int {
    const int label = static_cast<int>(std::lround(z[n])) - 1;
    return std::max(0, std::min(C - 1, label));
  };
  // the random coefficients on the utility scale
  arma::mat beta_utility;
  auto transformed_beta = [&]() -> arma::mat {
    arma::mat utility = beta;
    if (P_r > 0 && !all_random_normal) {
      for (int n = 0; n < N; ++n) {
        utility.col(n) = transform_random_effects(
          beta.col(n), random_distribution
        );
      }
    }
    return utility;
  };
  // the systematic utilities
  auto systematic = [&](bool with_W, bool with_L, bool with_X) -> arma::mat {
    arma::mat mu(rowsU, NT, arma::fill::zeros);
    if (with_W && P_f > 0) mu += arma::reshape(Wstack * alpha, rowsU, NT);
    if ((with_L && P_l > 0) || (with_X && P_r > 0)) {
      for (int n = 0; n < N; ++n) {
        const int Tn = static_cast<int>(std::lround(Tvec[n]));
        const int base = static_cast<int>(std::lround(csTvec[n]));
        if (with_L && P_l > 0) {
          mu.cols(base, base + Tn - 1) += arma::reshape(
            Lstack[n] * lambda.col(class_of(n)), rowsU, Tn
          );
        }
        if (with_X && P_r > 0) {
          mu.cols(base, base + Tn - 1) += arma::reshape(
            Xstack[n] * beta_utility.col(n), rowsU, Tn
          );
        }
      }
    }
    return mu;
  };
  // the log-likelihood of the observed choices of one decider under given
  // class-specific coefficients, with the latent utilities integrated out
  const arma::uvec no_availability;
  const arma::mat no_ranking;
  ClassLogLikelihood log_likelihood_class;
  if (P_l > 0) {
    log_likelihood_class = [&](int n, const arma::vec& lambda_c) -> double {
      const int Tn = static_cast<int>(std::lround(Tvec[n]));
      const int base = static_cast<int>(std::lround(csTvec[n]));
      double value = 0.0;
      for (int t = 0; t < Tn; ++t) {
        const int index = base + t;
        arma::vec mu = Lv[index] * lambda_c;
        if (P_f > 0) mu += Wv[index] * alpha;
        if (P_r > 0) mu += Xv[index] * beta_utility.col(n);
        const int response = static_cast<int>(std::lround(y(n, t)));
        value += log_probability_choice(
          response, mu, Sigma,
          available.n_rows > 0 ? available.row(index).t() : no_availability,
          ordered, ranked, gamma,
          ranked ? rdiff_list[static_cast<arma::uword>(response - 1)] :
            no_ranking
        );
      }
      return value;
    };
  }

  // the progress callback is called about one hundred times per run
  const int progress_interval = std::max(1, R / 100);

  // start loop
  for (int r = 0; r < R; ++r) {

    // print progress
    if (progress.isNotNull() &&
        ((r + 1) % progress_interval == 0 || r == 0 || r + 1 == R)) {
      Rcpp::Function(progress.get())(r + 1, R, C);
    }

    // check for code interruption by user
    Rcpp::checkUserInterrupt();

    // prepare Sigma
    if (!ordered) {
      Sigmainv_vec = arma::vectorise(Sigma_inv);
    }
    if (ranked) {
      const int L_size = static_cast<int>(rdiff_list.size());
      Sigmainv_ranked.resize(L_size);
      for (int yi = 0; yi < L_size; ++yi) {
        arma::mat S_tmp = rdiff_list[yi] * Sigma * rdiff_list[yi].t();
        S_tmp = 0.5 * (S_tmp + S_tmp.t()) + 1e-12 *
          arma::eye(S_tmp.n_rows, S_tmp.n_cols);
        Sigmainv_ranked[yi] = inv_spd(S_tmp);
      }
    }
    beta_utility = transformed_beta();

    // the class parameters and the individual coefficients are updated
    // given the allocation of the previous iteration
    if (mixture) {

      // update finite-mixture weights without imposing a label constraint
      if (!dp_update && C > 1) {
        s = update_s(delta, m);
      }

      // update b, the common block from all deciders
      if (P_r > 0) {
        arma::mat b_cs(P_cs, C);
        if (P_cs > 0) {
          b_cs = update_b(
            beta.rows(cs), block(Omega, cs), z, m,
            Sigma_b_0_inv.submat(cs, cs), mu_b_0.elem(cs)
          );
        }
        if (P_co > 0) {
          b_co = update_b(
            beta.rows(co), arma::vectorise(Omega_co), arma::ones(N),
            arma::vec{static_cast<double>(N)}, Sigma_b_0_inv.submat(co, co),
            mu_b_0.elem(co)
          );
        }
        assemble(b_cs, block(Omega, cs));
      }

      // update Omega, the common block from all deciders
      if (P_r > 0) {
        arma::mat Omega_cs(P_cs * P_cs, C);
        if (P_cs > 0) {
          Omega_cs = update_Omega(
            beta.rows(cs), b.rows(cs), z, m, n_Omega_0,
            V_Omega_0.submat(cs, cs), correlated_cs
          );
        }
        if (P_co > 0) {
          Omega_co = arma::reshape(update_Omega(
            beta.rows(co), arma::mat(b_co), arma::ones(N),
            arma::vec{static_cast<double>(N)}, n_Omega_0,
            V_Omega_0.submat(co, co), correlated_co
          ), P_co, P_co);
        }
        assemble(b.rows(cs), Omega_cs);
      }

      // update lambda
      if (P_l > 0) {
        lambda = update_lambda(
          z, C, P_l, Lv, LkL_mats, U - systematic(true, false, true), Sigma_inv,
          Sigmainv_vec, ordered, Tvec, csTvec, mu_lambda_0, Sigma_lambda_0_inv
        );
      }

      // update beta
      if (P_r > 0) {
        const arma::mat fixed_part = systematic(true, true, false);
        const arma::mat residual = Sigma_inv * (U - fixed_part);
        std::vector<arma::mat> Omega_cache;
        std::vector<arma::mat> Omega_inv_cache;
        Omega_cache.reserve(C);
        Omega_inv_cache.reserve(C);
        for (int c = 0; c < C; ++c) {
          arma::mat Oc = arma::reshape(Omega.col(c), P_r, P_r);
          Omega_cache.emplace_back(Oc);
          if (all_random_normal) Omega_inv_cache.emplace_back(inv_spd(Oc));
        }
        for (int n = 0; n < N; ++n) {
          const int ci = class_of(n);
          const arma::vec b_c = b.col(ci);
          const int Tn = static_cast<int>(std::lround(Tvec[n]));
          const int base = static_cast<int>(std::lround(csTvec[n]));
          if (all_random_normal) {
            arma::mat XSigX;
            if (ordered){
              XSigX = XSigX_ord[n];
            } else {
              XSigX = arma::reshape(XkX_mats[n] * Sigmainv_vec, P_r, P_r);
            }
            const arma::vec XSigU = Xstack[n].t() *
              arma::vectorise(residual.cols(base, base + Tn - 1));
            beta.col(n) = update_coefficient(
              b_c, Omega_inv_cache[ci], XSigX, XSigU
            );
          } else {
            beta.col(n) = update_beta_elliptical(
              beta.col(n), b_c, Omega_cache[ci], random_distribution,
              Xstack[n], U.cols(base, base + Tn - 1) -
                fixed_part.cols(base, base + Tn - 1),
              Sigma_inv
            );
          }
        }
        beta_utility = transformed_beta();
      }

      // interweave the class means of the normal random effects (Yu and Meng
      // 2011)
      const arma::uvec cs_normal = cs.elem(arma::find(
        random_distribution.elem(cs) == 0
      ));
      const arma::uvec co_normal = co.elem(arma::find(
        random_distribution.elem(co) == 0
      ));
      const arma::uvec normal = arma::join_cols(cs_normal, co_normal);
      const int P_csn = static_cast<int>(cs_normal.n_elem);
      const int P_con = static_cast<int>(co_normal.n_elem);
      if (normal.n_elem > 0) {
        const int dim = C * P_csn + P_con;
        arma::mat prior_precision(dim, dim, arma::fill::zeros);
        arma::vec prior_mean(dim, arma::fill::zeros);
        arma::mat XSigX(dim, dim, arma::fill::zeros);
        arma::vec XSigU(dim, arma::fill::zeros);
        arma::uvec index(normal.n_elem);
        for (int c = 0; c < C && P_csn > 0; ++c) {
          const arma::uvec rows =
            c * P_csn + arma::regspace<arma::uvec>(0, P_csn - 1);
          prior_precision.submat(rows, rows) =
            Sigma_b_0_inv.submat(cs_normal, cs_normal);
          prior_mean.elem(rows) = mu_b_0.elem(cs_normal);
        }
        if (P_con > 0) {
          const arma::uvec rows =
            C * P_csn + arma::regspace<arma::uvec>(0, P_con - 1);
          prior_precision.submat(rows, rows) =
            Sigma_b_0_inv.submat(co_normal, co_normal);
          prior_mean.elem(rows) = mu_b_0.elem(co_normal);
        }
        const arma::mat Mu_current = systematic(true, true, true);
        for (int n = 0; n < N; ++n) {
          const int ci = class_of(n);
          const int Tn = static_cast<int>(std::lround(Tvec[n]));
          const int base = static_cast<int>(std::lround(csTvec[n]));
          const arma::mat X_normal = Xstack[n].cols(normal);
          const arma::vec mean_normal = b.col(ci);
          const arma::mat target = Sigma_inv * (
            U.cols(base, base + Tn - 1) - Mu_current.cols(base, base + Tn - 1) +
            arma::reshape(X_normal * mean_normal.elem(normal), rowsU, Tn)
          );
          const arma::mat cross = ordered ?
            XSigX_ord[n] : arma::reshape(XkX_mats[n] * Sigmainv_vec, P_r, P_r);
          for (int p = 0; p < P_csn; ++p) index[p] = ci * P_csn + p;
          for (int p = 0; p < P_con; ++p) index[P_csn + p] = C * P_csn + p;
          XSigX.submat(index, index) += cross.submat(normal, normal);
          XSigU.elem(index) += X_normal.t() * arma::vectorise(target);
        }
        const arma::vec means = update_coefficient(
          prior_mean, prior_precision, XSigX, XSigU
        );
        arma::mat b_cs = b.rows(cs);
        for (int c = 0; c < C && P_csn > 0; ++c) {
          arma::vec column = b.col(c);
          column.elem(cs_normal) = means.subvec(c * P_csn, (c + 1) * P_csn - 1);
          b_cs.col(c) = column.elem(cs);
        }
        if (P_con > 0) {
          arma::vec column = b.col(0);
          column.elem(co_normal) = means.subvec(C * P_csn, dim - 1);
          b_co = column.elem(co);
        }
        const arma::mat old_b = b;
        assemble(b_cs, block(Omega, cs));
        for (int n = 0; n < N; ++n) {
          const int ci = class_of(n);
          const arma::vec shift = b.col(ci) - old_b.col(ci);
          beta.col(n) += shift;
        }
        beta_utility = transformed_beta();
      }
    }

    // update alpha
    if (P_f > 0) {
      arma::mat WSigW;
      if (ordered) {
        WSigW = arma::reshape(WkW, P_f, P_f);
      } else {
        WSigW = arma::reshape(WkW * Sigmainv_vec, P_f, P_f);
      }
      arma::mat target;
      if (P_l > 0 || P_r > 0) {
        target = Sigma_inv * (U - systematic(false, true, true));
      } else {
        target = Sigma_inv * U;
      }
      const arma::vec WSigU = Wstack.t() * arma::vectorise(target);
      alpha = update_coefficient(mu_alpha_0, Sigma_alpha_0_, WSigW, WSigU);
    }

    // latent class updates
    if (mixture) {

      if (weight_based_update && r + 1 <= B && (r + 1) % buffer == 0) {
        Rcpp::List update = update_classes_wb_lc(
          s, b.rows(cs), block(Omega, cs), lambda, epsmin, epsmax, deltamin,
          deltashift, true, Cmax
        );
        s = Rcpp::as<arma::vec>(update["s"]);
        lambda = Rcpp::as<arma::mat>(update["lambda"]);
        assemble(
          Rcpp::as<arma::mat>(update["b"]), Rcpp::as<arma::mat>(update["Omega"])
        );
        C = static_cast<int>(s.n_elem);
        z = allocate_classes(
          s, log_likelihood_mixture(
            beta.rows(cs), b.rows(cs), block(Omega, cs), lambda,
            log_likelihood_class, N, C
          ),
          z, false
        );
      } else if (dp_update) {
        Rcpp::List class_update = update_classes_dp_lc(
          beta.rows(cs), z, b.rows(cs), block(Omega, cs), lambda, delta,
          mu_b_0.elem(cs), Sigma_b_0.submat(cs, cs), n_Omega_0,
          V_Omega_0.submat(cs, cs), random_correlated.elem(cs), mu_lambda_0,
          Sigma_lambda_0, log_likelihood_class, false, Cmax
        );
        z = Rcpp::as<arma::vec>(class_update["z"]);
        lambda = Rcpp::as<arma::mat>(class_update["lambda"]);
        C = Rcpp::as<int>(class_update["C"]);
        assemble(
          Rcpp::as<arma::mat>(class_update["b"]),
          Rcpp::as<arma::mat>(class_update["Omega"])
        );
      } else if (C > 1) {
        z = allocate_classes(
          s, log_likelihood_mixture(
            beta.rows(cs), b.rows(cs), block(Omega, cs), lambda,
            log_likelihood_class, N, C
          ),
          z, nonempty
        );
      }
      m = update_m(C, z, false);
      if (dp_update) s = m / N;
      if (sample_delta) {
        if (dp_update) {
          delta = update_delta_dp(
            delta, N, C, delta_shape, delta_rate
          );
        } else {
          delta = update_delta_finite(
            delta, s, delta_shape, delta_rate, 0.2
          );
        }
      }

      class_sequence[r] = weight_based_update ? C : arma::accu(m > 0.0);
      delta_draws[r] = delta;
    }

    // update U, component by component over all occasions at once
    const arma::mat Mu = systematic(true, true, true);
    arma::mat residual;
    if (ordered) {
      for (int n = 0; n < N; ++n) {
        const int Tn = static_cast<int>(std::lround(Tvec[n]));
        const int base = static_cast<int>(std::lround(csTvec[n]));
        for (int t = 0; t < Tn; ++t) {
          ind = base + t;
          U.at(0, ind) = oeli::rttnorm(
            Mu.at(0, ind), 1.0, gamma[y(n,t) - 1], gamma[y(n,t)]
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
          arma::vec U_tmp = rdiff_tmp * U.col(ind);
          arma::vec mu_vec_tmp = rdiff_tmp * Mu.col(ind);
          U.col(ind) = rdiff_inv_list[yi] *
            update_U_ranked(U_tmp, mu_vec_tmp, Sigmainv_ranked[yi]);
        }
      }
    } else {
      arma::mat A = -Sigma_inv;
      arma::vec sd(Jm1);
      for (int i = 0; i < Jm1; ++i) {
        A.row(i) /= Sigma_inv.at(i, i);
        A.at(i, i) = 0.0;
        sd[i] = std::sqrt(1.0 / Sigma_inv.at(i, i));
      }
      const bool complete = available.n_rows == 0;
      residual = U - Mu;
      double* u = U.memptr();
      double* res = residual.memptr();
      const double* mu = Mu.memptr();
      for (int i = 0; i < Jm1; ++i) {
        const arma::rowvec a = A.row(i);
        for (int index = 0; index < NT; ++index) {
          const int offset = index * Jm1;
          double mean = mu[offset + i];
          for (int k = 0; k < Jm1; ++k) mean += a[k] * res[offset + k];
          double draw;
          if (!complete && available.at(index, i) != 1u) {
            draw = oeli::rtnorm(mean, sd[i], -arma::datum::inf, false);
          } else {
            bool bounded = complete || available.at(index, Jm1) == 1u;
            double bound = bounded ?
              0.0 : -std::numeric_limits<double>::infinity();
            for (int k = 0; k < Jm1; ++k) {
              if (k == i) continue;
              if (!complete && available.at(index, k) != 1u) continue;
              if (u[offset + k] > bound) bound = u[offset + k];
              bounded = true;
            }
            draw = oeli::rtnorm(
              mean, sd[i], bounded ? bound : -arma::datum::inf,
              bounded && response_of[index] != i + 1
            );
          }
          u[offset + i] = draw;
          res[offset + i] = draw - mu[offset + i];
        }
      }
    }

    if (!U.is_finite()) {
      Rcpp::stop(
        "The latent utilities became non-finite in iteration %d. This happens "
        "when the systematic utility of a chosen alternative lies extremely "
        "far from its truncation point, typically because a covariate with a "
        "log-normal random effect has a large scale. Rescale the covariate, "
        "for example divide a price by 10, or start the effect at a smaller "
        "value through the prior.", r + 1
      );
    }

    // update Sigma by marginal data augmentation (Imai and van Dyk 2005)
    if (!ordered) {
      if (residual.n_elem == 0) residual = U - Mu;
      const double alpha2 = arma::trace(V_Sigma_0 * Sigma_inv) /
        R::rchisq(static_cast<double>(n_Sigma_0 * Jm1));
      const arma::mat expanded = update_Sigma(
        n_Sigma_0, V_Sigma_0, NT, alpha2 * residual * residual.t()
      );
      Sigma = expanded / expanded.at(0, 0);
      Sigma_inv = inv_spd(Sigma);
      U = Mu + residual * std::sqrt(alpha2 / expanded.at(0, 0));
    }

    // update d (for the ordered probit model)
    if (ordered) {
      for (int n = 0; n < N; ++n) {
        const int Tn = static_cast<int>(std::lround(Tvec[n]));
        const int base = static_cast<int>(std::lround(csTvec[n]));
        for (int t = 0; t < Tn; ++t) {
          ind = base + t;
          mu_mat(n, t) = Mu(0, ind);
        }
      }
      if (r == 0) {
        old_ll = log_likelihood_ordered(d, y, mu_mat, Tvec);
      }
      threshold_update = update_d(
        d, y, mu_mat, old_ll, mu_d_0, Sigma_d_0, Tvec
      );
      d = Rcpp::as<arma::vec>(threshold_update["d"]);
      old_ll = Rcpp::as<double>(threshold_update["log_likelihood"]);
      gamma = d_to_gamma(d);
    }

    // save draws
    if (P_f > 0) {
      alpha_draws(r, arma::span::all) = arma::trans(alpha);
    }
    if (mixture) {
      s_draws(r, arma::span(0, s.size() - 1)) = arma::trans(s);
      z_draws(r, arma::span::all) = arma::trans(z);
    }
    if (P_r > 0) {
      arma::vec vectorise_b = arma::vectorise(b);
      b_draws(r, arma::span(0, vectorise_b.size() - 1)) =
        arma::trans(vectorise_b);
      arma::vec vectorise_Omega = arma::vectorise(Omega);
      Omega_draws(r, arma::span(0, vectorise_Omega.size() - 1)) =
        arma::trans(vectorise_Omega);
      if (save_beta_draws) beta_draws[r] = beta;
    }
    if (P_l > 0) {
      arma::vec vectorise_lambda = arma::vectorise(lambda);
      lambda_draws(r, arma::span(0, vectorise_lambda.size() - 1)) =
        arma::trans(vectorise_lambda);
    }
    Sigma_draws(r, arma::span::all) = arma::trans(arma::vectorise(Sigma));
    if (ordered == true) {
      d_draws(r, arma::span::all) = arma::trans(d);
    }
  }

  // return Gibbs samples
  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("s") = s_draws,
    Rcpp::Named("z") = z_draws,
    Rcpp::Named("alpha") = alpha_draws,
    Rcpp::Named("b") = b_draws,
    Rcpp::Named("Omega") = Omega_draws,
    Rcpp::Named("lambda") = lambda_draws,
    Rcpp::Named("Sigma") = Sigma_draws,
    Rcpp::Named("d") = d_draws,
    Rcpp::Named("class_sequence") = class_sequence,
    Rcpp::Named("delta") = delta_draws
  );
  if (save_beta_draws) out["beta"] = beta_draws;
  return out;
}
