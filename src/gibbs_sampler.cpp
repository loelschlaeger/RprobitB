// [[Rcpp::depends("RcppArmadillo")]]
#include <oeli.h>
#include <RcppArmadillo.h>
#include <Rmath.h>

//' Update class weight vector \code{s}
//'
//' @inheritParams check_prior
//'
//' @param m \[`numeric()`\]\cr
//' The vector of current class frequencies.
//'
//' @return
//' An update for \code{s}.
//'
//' @examples
//' update_s(delta = 1, m = sample.int(4))
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

arma::vec update_s (int delta, arma::vec m) {
  return oeli::rdirichlet(delta * arma::ones(m.size()) + m);
}

//' Update class allocation vector \code{z}
//'
//' @inheritParams RprobitB_parameter
//'
//' @return
//' An update for \code{z}.
//'
//' @examples
//' update_z(
//'   s = c(0.5, 0.5), beta = matrix(c(-2, 0, 2), ncol = 3),
//'   b = cbind(0, 1), Omega = cbind(1, 1)
//' )
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

arma::vec update_z (arma::vec s, arma::mat beta, arma::mat b, arma::mat Omega) {
  Rcpp::Function sample("sample");
  Rcpp::Function seq("seq");
  int N = beta.n_cols;
  int C = s.size();
  int P_r = b.n_rows;
  arma::vec z = arma::zeros<arma::vec>(N);
  arma::vec prob_z = arma::zeros<arma::vec>(C);
  for(int n = 0; n < N; n++){
    for(int c = 0; c < C; c++){
      prob_z[c] = s[c] * oeli::dmvnorm(
        beta(arma::span::all,n),
        b(arma::span::all,c),
        reshape(Omega(arma::span::all,c), P_r, P_r)
      );
    }
    z[n] = Rcpp::as<int>(sample(seq(1, C), 1, false, prob_z));
  }
  return z;
}

//' Update class sizes \code{m}
//'
//' @inheritParams RprobitB_parameter
//'
//' @param non_zero \[`logical(1)`\]\cr
//' Enforce strictly positive values in \code{m} (for numerical stability)?
//'
//' @return
//' An update for \code{m}.
//'
//' @examples
//' update_m(C = 3, z = c(1, 1, 1, 2, 2, 3))
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

arma::vec update_m (int C, arma::vec z, bool non_zero = false) {
  int N = z.size();
  arma::vec m(C);
  for (int c = 0; c < C; c++){
    for (int n = 0; n < N; n++){
      if (z[n] == c+1) m[c] += 1;
    }
  }
  if (non_zero == true){
    for(int c = 0; c<C; c++) if(m[c]==0) m[c] = 1;
  }
  return m;
}

//' Update class means
//'
//' @inheritParams RprobitB_parameter
//'
//' @param m
//' The vector of class sizes of length \code{C}.
//'
//' @inheritParams check_prior
//'
//' @param Dinv
//' The precision matrix (i.e. the inverse of the covariance matrix) of dimension \code{P_r} x \code{P_r}
//' of the normal prior for each \code{b_c}.
//'
//' @details
//' The following holds independently for each class \eqn{c}.
//' Let \eqn{b_c} be the mean of class number \eqn{c}. A priori, we assume that \eqn{b_c} is normally distributed
//' with mean vector \eqn{\xi} and covariance matrix \eqn{D}.
//' Let \eqn{(\beta_n)_{z_n=c}} be the collection of \eqn{\beta_n} that are currently allocated to class \eqn{c},
//' \eqn{m_c} the class size, and \eqn{\bar{b}_c} their arithmetic mean.
//' Assuming independence across draws, \eqn{(\beta_n)_{z_n=c}} has
//' a normal likelihood of \deqn{\prod_n \phi(\beta_n \mid b_c,\Omega_c),} where the product is over the values \eqn{n}
//' for which \eqn{z_n=c} holds.
//' Due to the conjugacy of the prior, the posterior \eqn{\Pr(b_c \mid (\beta_n)_{z_n=c})} follows a normal distribution
//' with mean \deqn{(D^{-1} + m_c\Omega_c^{-1})^{-1}(D^{-1}\xi + m_c\Omega_c^{-1}\bar{b}_c)} and covariance matrix
//' \deqn{(D^{-1} + m_c \Omega_c^{-1})^{-1}.}
//'
//' @return
//' A matrix of updated means for each class in columns.
//'
//' @examples
//' ### N = 100 decider, P_r = 2 random coefficients, and C = 2 latent classes
//' N <- 100
//' (b_true <- cbind(c(0,0),c(1,1)))
//' Omega <- matrix(c(1,0.3,0.3,0.5,1,-0.3,-0.3,0.8), ncol=2)
//' z <- c(rep(1,N/2),rep(2,N/2))
//' m <- as.numeric(table(z))
//' beta <- sapply(z, function(z) oeli::rmvnorm(n = 1, b_true[,z], matrix(Omega[,z],2,2)))
//' ### prior mean vector and precision matrix (inverse of covariance matrix)
//' xi <- c(0,0)
//' Dinv <- diag(2)
//' ### updated class means (in columns)
//' update_b(beta = beta, Omega = Omega, z = z, m = m, xi = xi, Dinv = Dinv)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

arma::mat update_b (arma::mat beta, arma::mat Omega, arma::vec z, arma::vec m, arma::vec xi, arma::mat Dinv) {
  int P_r = beta.n_rows;
  int C = m.size();
  int N = beta.n_cols;
  arma::mat b_draw = arma::zeros<arma::mat>(P_r,C);
  arma::mat b_bar = arma::zeros<arma::mat>(P_r,C);
  for(int c = 0; c<C; c++){
    for(int n = 0; n<N; n++){
      if(z[n]==c+1) b_bar(arma::span::all,c) += beta(arma::span::all,n);
    }
    b_bar(arma::span::all,c) /= m[c];
  }
  for(int c = 0; c<C; c++){
    arma::mat Omega_c_inv = arma::inv(reshape(Omega(arma::span::all,c),P_r,P_r));
    b_draw(arma::span::all,c) = oeli::rmvnorm(
      arma::inv(Dinv+m[c]*Omega_c_inv) * (Dinv*xi+m[c]*Omega_c_inv*b_bar(arma::span::all,c)),
      arma::inv(Dinv+m[c]*Omega_c_inv)
    );
  }
  return(b_draw);
}

//' Update class covariances
//'
//' @inheritParams RprobitB_parameter
//'
//' @param m
//' The vector of class sizes of length \code{C}.
//'
//' @inheritParams check_prior
//'
//' @details
//' The following holds independently for each class \eqn{c}.
//' Let \eqn{\Omega_c} be the covariance matrix of class number \code{c}.
//' A priori, we assume that \eqn{\Omega_c} is inverse Wishart distributed
//' with \eqn{\nu} degrees of freedom and scale matrix \eqn{\Theta}.
//' Let \eqn{(\beta_n)_{z_n=c}} be the collection of \eqn{\beta_n} that are currently allocated to class \eqn{c},
//' \eqn{m_c} the size of class \eqn{c}, and \eqn{b_c} the class mean vector.
//' Due to the conjugacy of the prior, the posterior \eqn{\Pr(\Omega_c \mid (\beta_n)_{z_n=c})} follows an inverted Wishart distribution
//' with \eqn{\nu + m_c} degrees of freedom and scale matrix \eqn{\Theta^{-1} + \sum_n (\beta_n - b_c)(\beta_n - b_c)'}, where
//' the product is over the values \eqn{n} for which \eqn{z_n=c} holds.
//'
//' @return
//' A matrix of updated covariance matrices for each class in columns.
//'
//' @examples
//' ### N = 100 decider, P_r = 2 random coefficients, and C = 2 latent classes
//' N <- 100
//' b <- cbind(c(0,0),c(1,1))
//' (Omega_true <- matrix(c(1,0.3,0.3,0.5,1,-0.3,-0.3,0.8), ncol=2))
//' z <- c(rep(1,N/2),rep(2,N/2))
//' m <- as.numeric(table(z))
//' beta <- sapply(z, function(z) oeli::rmvnorm(n = 1, b[,z], matrix(Omega_true[,z],2,2)))
//' ### degrees of freedom and scale matrix for the Wishart prior
//' nu <- 1
//' Theta <- diag(2)
//' ### updated class covariance matrices (in columns)
//' update_Omega(beta = beta, b = b, z = z, m = m, nu = nu, Theta = Theta)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

arma::mat update_Omega (arma::mat beta, arma::mat b, arma::vec z, arma::vec m, int nu, arma::mat Theta) {
  int P_r = beta.n_rows;
  int C = m.size();
  int N = beta.n_cols;
  arma::mat Omega = arma::zeros<arma::mat>(P_r*P_r,C);
  for(int c = 0; c<C; c++){
    arma::mat sum_sp = arma::zeros<arma::mat>(P_r,P_r);
    for(int n = 0; n<N; n++){
      if(z[n]==c+1){
        sum_sp += (beta(arma::span::all,n)-b(arma::span::all,c)) * trans(beta(arma::span::all,n)-b(arma::span::all,c));
      }
    }
    arma::mat Omega_draw = oeli::rwishart(nu+m[c],arma::inv(Theta+sum_sp),true);
    Omega(arma::span::all,c) = reshape(Omega_draw,P_r*P_r,1);
  }
  return(Omega);
}

//' Update coefficient vector of multiple linear regression
//'
//' @param mu0
//' The mean vector of the normal prior distribution for the coefficient vector.
//'
//' @param Tau0
//' The precision matrix (i.e. inverted covariance matrix) of the normal prior
//' distribution for the coefficient vector.
//'
//' @param XSigX
//' The matrix \eqn{\sum_{n=1}^N X_n'\Sigma^{-1}X_n}. See below for details.
//'
//' @param XSigU
//' The vector \eqn{\sum_{n=1}^N X_n'\Sigma^{-1}U_n}. See below for details.
//'
//' @details
//' This function draws from the posterior distribution of \eqn{\beta} in the linear utility
//' equation \deqn{U_n = X_n\beta + \epsilon_n,} where \eqn{U_n} is the
//' (latent, but here assumed to be known) utility vector of decider \eqn{n = 1,\dots,N}, \eqn{X_n}
//' is the design matrix build from the choice characteristics faced by \eqn{n},
//' \eqn{\beta} is the unknown coefficient vector (this can be either the fixed
//' coefficient vector \eqn{\alpha} or the decider-specific coefficient vector \eqn{\beta_n}),
//' and \eqn{\epsilon_n} is the error term assumed to be normally distributed with mean \eqn{0}
//' and (known) covariance matrix \eqn{\Sigma}.
//' A priori we assume the (conjugate) normal prior distribution \deqn{\beta \sim N(\mu_0,T_0)}
//' with mean vector \eqn{\mu_0} and precision matrix (i.e. inverted covariance matrix) \eqn{T_0}.
//' The posterior distribution for \eqn{\beta} is normal with
//' covariance matrix \deqn{\Sigma_1 = (T_0 + \sum_{n=1}^N X_n'\Sigma^{-1}X_n)^{-1}} and mean vector
//' \deqn{\mu_1 = \Sigma_1(T_0\mu_0 + \sum_{n=1}^N X_n'\Sigma^{-1}U_n)}.
//' Note the analogy of \eqn{\mu_1} to the generalized least squares estimator
//' \deqn{\hat{\beta}_{GLS} = (\sum_{n=1}^N X_n'\Sigma^{-1}X_n)^{-1} \sum_{n=1}^N X_n'\Sigma^{-1}U_n} which
//' becomes weighted by the prior parameters \eqn{\mu_0} and \eqn{T_0}.
//'
//' @return
//' A vector, a draw from the normal posterior distribution of the coefficient
//' vector in a multiple linear regression.
//'
//' @examples
//' ### true coefficient vector
//' beta_true <- matrix(c(-1,1), ncol=1)
//' ### error term covariance matrix
//' Sigma <- matrix(c(1,0.5,0.2,0.5,1,0.2,0.2,0.2,2), ncol=3)
//' ### draw data
//' N <- 100
//' X <- replicate(N, matrix(rnorm(6), ncol=2), simplify = FALSE)
//' eps <- replicate(N, oeli::rmvnorm(n = 1, mean = c(0,0,0), Sigma = Sigma), simplify = FALSE)
//' U <- mapply(function(X, eps) X %*% beta_true + eps, X, eps, SIMPLIFY = FALSE)
//' ### prior parameters for coefficient vector
//' mu0 <- c(0,0)
//' Tau0 <- diag(2)
//' ### draw from posterior of coefficient vector
//' XSigX <- Reduce(`+`, lapply(X, function(X) t(X) %*% solve(Sigma) %*% X))
//' XSigU <- Reduce(`+`, mapply(function(X, U) t(X) %*% solve(Sigma) %*% U, X, U, SIMPLIFY = FALSE))
//' beta_draws <- replicate(100, update_reg(mu0, Tau0, XSigX, XSigU), simplify = TRUE)
//' rowMeans(beta_draws)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

arma::vec update_reg (arma::vec mu0, arma::mat Tau0, arma::mat XSigX, arma::vec XSigU) {
  arma::mat Sigma1 = arma::inv(Tau0 + XSigX);
  arma::mat mu1 = Sigma1 * (Tau0 * mu0 + XSigU);
  return(oeli::rmvnorm(mu1, Sigma1));
}

//' Update error term covariance matrix of multiple linear regression
//'
//' @param N
//' The draw size.
//'
//' @param S
//' A matrix, the sum over the outer products of the residuals \eqn{(\epsilon_n)_{n=1,\dots,N}}.
//'
//' @inheritParams check_prior
//'
//' @details
//' This function draws from the posterior distribution of the covariance matrix \eqn{\Sigma} in the linear utility
//' equation \deqn{U_n = X_n\beta + \epsilon_n,} where \eqn{U_n} is the
//' (latent, but here assumed to be known) utility vector of decider \eqn{n = 1,\dots,N}, \eqn{X_n}
//' is the design matrix build from the choice characteristics faced by \eqn{n},
//' \eqn{\beta} is the coefficient vector, and \eqn{\epsilon_n} is the error term assumed to be
//' normally distributed with mean \eqn{0} and unknown covariance matrix \eqn{\Sigma}.
//' A priori we assume the (conjugate) Inverse Wishart distribution \deqn{\Sigma \sim W(\kappa,E)}
//' with \eqn{\kappa} degrees of freedom and scale matrix \eqn{E}.
//' The posterior for \eqn{\Sigma} is the Inverted Wishart distribution with \eqn{\kappa + N} degrees of freedom
//' and scale matrix \eqn{E^{-1}+S}, where \eqn{S = \sum_{n=1}^{N} \epsilon_n \epsilon_n'} is the sum over
//' the outer products of the residuals \eqn{(\epsilon_n = U_n - X_n\beta)_n}.
//'
//' @return
//' A matrix, a draw from the Inverse Wishart posterior distribution of the error term
//' covariance matrix in a multiple linear regression.
//'
//' @examples
//' ### true error term covariance matrix
//' (Sigma_true <- matrix(c(1,0.5,0.2,0.5,1,0.2,0.2,0.2,2), ncol=3))
//' ### coefficient vector
//' beta <- matrix(c(-1,1), ncol=1)
//' ### draw data
//' N <- 100
//' X <- replicate(N, matrix(rnorm(6), ncol=2), simplify = FALSE)
//' eps <- replicate(N, oeli::rmvnorm(n = 1, mean = c(0,0,0), Sigma = Sigma_true), simplify = FALSE)
//' U <- mapply(function(X, eps) X %*% beta + eps, X, eps, SIMPLIFY = FALSE)
//' ### prior parameters for covariance matrix
//' kappa <- 4
//' E <- diag(3)
//' ### draw from posterior of coefficient vector
//' outer_prod <- function(X, U) (U - X %*% beta) %*% t(U - X %*% beta)
//' S <- Reduce(`+`, mapply(outer_prod, X, U, SIMPLIFY = FALSE))
//' Sigma_draws <- replicate(100, update_Sigma(kappa, E, N, S))
//' apply(Sigma_draws, 1:2, mean)
//' apply(Sigma_draws, 1:2, stats::sd)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

arma::mat update_Sigma (int kappa, arma::mat E, int N, arma::mat S) {
  return oeli::rwishart(kappa + N, arma::inv(E + S), true);
}

//' Update latent utility vector
//'
//' @param U
//' The current utility vector of length \code{J-1}.
//'
//' @param y
//' An integer from \code{1} to \code{J}, the index of the chosen alternative.
//'
//' @param sys
//' A vector of length \code{J-1}, the systematic utility part.
//'
//' @param Sigmainv
//' The inverted error term covariance matrix of dimension \code{J-1} x \code{J-1}.
//'
//' @details
//' The key ingredient to Gibbs sampling for probit models is considering the latent utilities
//' as parameters themselves which can be updated (data augmentation).
//' Independently for all deciders \eqn{n=1,\dots,N} and choice occasions \eqn{t=1,...,T_n},
//' the utility vectors \eqn{(U_{nt})_{n,t}} in the linear utility equation \eqn{U_{nt} = X_{nt} \beta + \epsilon_{nt}}
//' follow a \eqn{J-1}-dimensional truncated normal distribution, where \eqn{J} is the number of alternatives,
//' \eqn{X_{nt} \beta} the systematic (i.e. non-random) part of the utility and \eqn{\epsilon_{nt} \sim N(0,\Sigma)} the error term.
//' The truncation points are determined by the choices \eqn{y_{nt}}. To draw from a truncated multivariate
//' normal distribution, this function implemented the approach of Geweke (1998) to conditionally draw each component
//' separately from a univariate truncated normal distribution. See Oelschläger (2020) for the concrete formulas.
//'
//' @references
//' See Geweke (1998) \emph{Efficient Simulation from the Multivariate Normal and Student-t Distributions Subject
//' to Linear Constraints and the Evaluation of Constraint Probabilities} for Gibbs sampling
//' from a truncated multivariate normal distribution. See Oelschläger and Bauer (2020) \emph{Bayes Estimation
//' of Latent Class Mixed Multinomial Probit Models} for its application to probit utilities.
//'
//' @return
//' An updated utility vector of length \code{J-1}.
//'
//' @examples
//' U <- c(0,0,0)
//' y <- 3
//' sys <- c(0,0,0)
//' Sigmainv <- solve(diag(3))
//' update_U(U, y, sys, Sigmainv)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

arma::vec update_U (arma::vec U, int y, arma::vec sys, arma::mat Sigmainv) {
  int Jm1 = U.size();
  bool above;
  double bound;
  double m;
  arma::vec U_update = U;
  arma::vec maxInd(2);
  for(int i = 0; i<Jm1; i++){
    bound = 0.0;
    for(int j = 0; j<Jm1; j++) if(j!=i) {
      maxInd[0] = bound;
      maxInd[1] = U_update[j];
      bound = max(maxInd);
    }
    if (y==(i+1))
      above = false;
    else
      above = true;
    m = 0.0;
    for(int k = 0; k<Jm1; k++){
      if (k!=i){
        m += - 1/Sigmainv(Jm1*i+i) * Sigmainv(Jm1*i+k)*(U_update[k]-sys[k]);
      }
    }
    U_update[i] = oeli::rtnorm(sys[i]+m, sqrt(1/Sigmainv(Jm1*i+i)), bound, above);
  }
  return (U_update);
}

//' Update latent utility vector in the ranked probit case
//'
//' @param U
//' The current utility vector of length \code{J-1}, differenced such that
//' the vector is negative.
//'
//' @param sys
//' A vector of length \code{J-1}, the systematic utility part.
//'
//' @param Sigmainv
//' The inverted error term covariance matrix of dimension
//' \code{J-1} x \code{J-1}.
//'
//' @details
//' This update is basically the same as in the non-ranked case, despite that
//' the truncation point is zero.
//'
//' @return
//' An updated utility vector of length \code{J-1}.
//'
//' @examples
//' U <- c(0,0)
//' sys <- c(0,0)
//' Sigmainv <- diag(2)
//' update_U_ranked(U, sys, Sigmainv)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

arma::vec update_U_ranked (arma::vec U, arma::vec sys, arma::mat Sigmainv) {
  int Jm1 = U.size();
  arma::vec U_update = U;
  double m;
  for(int i = 0; i<Jm1; i++){
    m = 0.0;
    for(int k = 0; k<Jm1; k++){
      if (k!=i){
        m += - 1/Sigmainv(Jm1*i+i) * Sigmainv(Jm1*i+k)*(U_update[k]-sys[k]);
      }
    }
    U_update[i] = oeli::rtnorm(sys[i]+m, sqrt(1/Sigmainv(Jm1*i+i)), 0.0, true);
  }
  return (U_update);
}

//' Transform threshold increments to thresholds
//'
//' @description
//' This helper function transforms the threshold increments \code{d} to the
//' thresholds \code{gamma}.
//'
//' @param d
//' A numeric vector of threshold increments.
//'
//' @details
//' The threshold vector \code{gamma} is computed from the threshold increments
//' \code{d} as \code{c(-100,0,cumsum(exp(d)),100)}, where the bounds
//' \code{-100} and \code{100} exist for numerical reasons and the first
//' threshold is fixed to \code{0} for identification.
//'
//' @return
//' A numeric vector of the thresholds.
//'
//' @examples
//' d_to_gamma(c(0,0,0))
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

arma::vec d_to_gamma (arma::vec d) {
  int length_d = d.size();
  double acc = 0;
  for(int i = 0; i < length_d; i++) {
    d[i] = exp(d[i]) + acc;
    acc = d[i];
  }
  arma::vec gamma(length_d+3);
  gamma[0] = -100;
  gamma[1] = 0;
  for(int i = 0; i < length_d; i++) {
    gamma[2+i] = d[i];
  }
  gamma[length_d+2] = 100;
  return(gamma);
}

//' Log-likelihood in the ordered probit model
//'
//' @param d
//' A numeric vector of threshold increments.
//'
//' @param y
//' A matrix of the choices.
//'
//' @param mu
//' A matrix of the systematic utilities.
//'
//' @param Tvec
//' The element \code{Tvec} in \code{\link{sufficient_statistics}}.
//'
//' @return
//' The log-likelihood value.
//'
//' @examples
//' ll_ordered(c(0,0,0), matrix(1), matrix(1), 1)
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

double ll_ordered (arma::vec d, arma::mat y, arma::mat mu, arma::vec Tvec) {
  int N = Tvec.size();
  double ll = 0.0;
  double ub, lb, prob;
  arma::vec gamma = d_to_gamma(d);
  for(int n = 0; n<N; n++){
    for(int t = 0; t<Tvec[n]; t++){
      ub = gamma[y(n,t)];
      lb = gamma[y(n,t)-1];
      prob = R::pnorm(ub-mu(n,t),0.0,1.0,1,0) - R::pnorm(lb-mu(n,t),0.0,1.0,1,0);
      if(prob < 1e-10) prob = 1e-10;
      ll += std::log(prob);
    }
  }
  return ll;
}

//' Update utility threshold increments
//'
//' @param d
//' The current vector of utility threshold increments.
//'
//' @param ll
//' Current log-likelihood value.
//'
//' @param zeta
//' The mean vector of the normal prior for \code{d}.
//'
//' @param Z
//' The covariance matrix of the normal prior for \code{d}.
//'
//' @inheritParams ll_ordered
//'
//' @return
//' The updated value for \code{d}.
//'
//' @export
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

Rcpp::List update_d (arma::vec d, arma::mat y, arma::mat mu, double ll,
               arma::vec zeta, arma::mat Z, arma::vec Tvec) {
  int length_d = d.size();
  arma::vec d_cand = d + 0.1 * Rcpp::as<arma::vec>(Rcpp::rnorm(length_d,0.0,1.0));
  double ll_cand = ll_ordered(d_cand, y, mu, Tvec);
  double post_cand = ll_cand + oeli::dmvnorm(d_cand, zeta, Z, true);
  double ll_diff = post_cand - ll - oeli::dmvnorm(d, zeta, Z, true);
  double alpha = std::exp(ll_diff);
  double unif = 0.0;
  if (alpha < 1) unif = R::runif(0.0,1.0);
  if (unif <= alpha) {
    d = d_cand;
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
//' The threshold weight (between 0 and 1) for removing a class.
//'
//' @param epsmax \[`numeric(1)`\]\cr
//' The threshold weight (between 0 and 1) for splitting a class.
//'
//' @param deltamin \[`numeric(1)`\]\cr
//' The threshold difference in class means for joining two classes.
//'
//' @param identify_classes \[`logical(1)`\]\cr
//' Identify classes by decreasing class weights?
//'
//' @inheritParams RprobitB_parameter
//'
//' @details
//' The following updating rules apply:
//'
//' * Class \eqn{c} is removed if \eqn{s_c<\epsilon_{min}}.
//' * Class \eqn{c} is split into two classes, if \eqn{s_c>\epsilon_{max}}.
//' * Two classes \eqn{c_1} and \eqn{c_2} are merged to one class, if
//'   \eqn{||b_{c_1} - b_{c_2}||<\delta_{min}}.
//'
//' @examples
//' ### parameter settings
//' s <- c(0.8, 0.2)
//' b <- matrix(c(1, 1, 1, -1), ncol = 2)
//' Omega <- matrix(c(0.5, 0.3, 0.3, 0.5, 1, -0.1, -0.1, 0.8), ncol = 2)
//'
//' ### remove class 2
//' RprobitB:::update_classes_wb(
//'   epsmin = 0.3, epsmax = 0.9, deltamin = 1, s = s, b = b, Omega = Omega
//' )
//'
//' ### split class 1
//' RprobitB:::update_classes_wb(
//'   epsmin = 0.1, epsmax = 0.7, deltamin = 1, s = s, b = b, Omega = Omega
//' )
//'
//' ### merge classes 1 and 2
//' RprobitB:::update_classes_wb(
//'   epsmin = 0.1, epsmax = 0.9, deltamin = 3, s = s, b = b, Omega = Omega
//' )
//'
//' @return
//' A list of updated values for \code{s}, \code{b}, and \code{Omega}.
//'
// [[Rcpp::export]]

Rcpp::List update_classes_wb (
   double epsmin, double epsmax, double deltamin,
   arma::vec s, arma::mat b, arma::mat Omega, int Cmax = 10,
   bool identify_classes = false
) {

   bool flag = false;
   int C = b.n_cols;
   int P = b.n_rows;
   double deltashift = deltamin;
   arma::mat stack = join_cols(trans(s), join_cols(b, Omega));

   // remove class
   int id_min = index_min(stack(0, arma::span::all));
   if (C > 1 && stack(0, id_min) < epsmin) {
     C -= 1;
     stack.shed_col(id_min);
     stack.row(0) = stack.row(0) / arma::accu(stack.row(0));
     flag = true;
   }

   // split class
   if (flag == false && C < Cmax) {
     int id_max = index_max(stack(0, arma::span::all));
     if (stack(0, id_max) > epsmax) {
       arma::mat max_class_Omega = reshape(
         stack(arma::span(P + 1, P + 1 + P * P - 1), id_max), P, P
       );
       arma::vec eigval;
       arma::mat eigvec;
       eig_sym(eigval, eigvec, max_class_Omega);
       arma::vec v = eigvec.col(P - 1);
       arma::vec mean_shift = deltashift * std::sqrt(eigval(P - 1)) * v;
       stack.insert_cols(id_max, stack(arma::span::all, id_max));
       stack(0, arma::span(id_max, id_max + 1)) /= 2;
       stack(arma::span(1, 1 + P - 1), id_max) += mean_shift;
       stack(arma::span(1, 1 + P - 1), id_max + 1) -= mean_shift;
       C += 1;
       flag = true;
     }
   }

   // merge classes
   if (flag == false && C > 1) {
     arma::vec closest_classes = arma::zeros<arma::vec>(3);
     closest_classes(0) = std::numeric_limits<int>::max();
     for (int c1 = 0; c1 < C; c1++) {
       for (int c2 = 0; c2 < c1; c2++) {
         arma::vec bc1 = stack(arma::span(1, 1 + P - 1), c1);
         arma::vec bc2 = stack(arma::span(1, 1 + P - 1), c2);
         double dev_sq = 0;
         for (int p = 0; p < P; p++) {
           dev_sq += (bc1(p) - bc2(p)) * (bc1(p) - bc2(p));
         }
         double euc_dist = sqrt(dev_sq);
         if(euc_dist < closest_classes(0)) {
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
       flag = true;
     }
   }

   // identify classes
   if (identify_classes == true) {
     stack = stack.cols(sort_index(stack(0, arma::span::all), "descend"));
   }

   // return class updates
   return Rcpp::List::create(
     Rcpp::Named("s") = stack.row(0),
     Rcpp::Named("b") = stack.rows(arma::span(1, 1 + P - 1)),
     Rcpp::Named("Omega") = stack.rows(arma::span(P + 1, 1 + P + P * P - 1))
   );
}

//' Dirichlet process-based class updates
//'
//' @inheritParams RprobitB_parameter
//' @inheritParams check_prior
//' @inheritParams update_classes_wb
//'
//' @examples
//' set.seed(1)
//' z <- c(rep(1,20),rep(2,30))
//' b <- matrix(c(1,1,1,-1), ncol=2)
//' Omega <- matrix(c(1,0.3,0.3,0.5,1,-0.3,-0.3,0.8), ncol=2)
//' beta <- sapply(z, function(z) oeli::rmvnorm(n = 1, b[,z], matrix(Omega[,z],2,2)))
//' delta <- 1
//' xi <- numeric(2)
//' D <- diag(2)
//' nu <- 4
//' Theta <- diag(2)
//' RprobitB:::update_classes_dp(
//'   Cmax = 10, beta = beta, z = z, b = b, Omega = Omega,
//'   delta = delta, xi = xi, D = D, nu = nu, Theta = Theta
//' )
//'
//' @return
//' A list of updated values for \code{z}, \code{b}, \code{Omega}, and \code{C}.
//'
// [[Rcpp::export]]

Rcpp::List update_classes_dp (
   arma::mat beta, arma::vec z, arma::mat b, arma::mat Omega,
   double delta, arma::vec xi, arma::mat D, int nu, arma::mat Theta,
   int Cmax = 10, bool identify_classes = false
) {

   Rcpp::Function sample("sample");
   Rcpp::Function seq("seq");
   int N = z.size();
   int C = b.n_cols;
   int P_r = b.n_rows;
   arma::mat b_full(P_r,Cmax);
   b_full(arma::span::all, arma::span(0, C - 1)) = b;
   arma::mat Omega_full(P_r * P_r, Cmax);
   Omega_full(arma::span::all, arma::span(0, C - 1)) = Omega;
   arma::vec m_full(Cmax);
   m_full(arma::span(0, C - 1)) = update_m(C, z, true);
   arma::vec logp(C + 1);

   // start Dirichlet process
   for(int n = 0; n < N; n++) {

     // unassign current class membership
     m_full[z[n] - 1] -= 1;

     // remove empty class
     if (m_full[z[n] - 1] == 0) {
       m_full[z[n] - 1] = m_full[C - 1];
       z.elem(find(z == C)).fill(z[n]);
       m_full[C - 1] = 0;
       C -= 1;
       b_full(arma::span::all, z[n] - 1) = arma::zeros<arma::vec>(P_r);
       Omega_full(arma::span::all, z[n] - 1) = arma::zeros<arma::vec>(P_r *P_r);
     }

     Rcpp::Rcout << "2" << std::endl;

     // calculate class allocation posteriors
     for (int c = 0; c < C; c++) {
       arma::mat beta_c = beta.cols(find(z == c + 1));
       arma::mat beta_c_diff_b = beta_c.each_col() - b_full.col(c);
       arma::mat Omega_c = (Theta + beta_c_diff_b * trans(beta_c_diff_b)) /
         (m_full[c] + nu - P_r - 1);
       arma::mat sig_b = arma::inv(
         arma::inv(D) + m_full[c] * arma::inv(Omega_c)
        );
       arma::vec mu_b = sig_b *
         (arma::inv(Omega_c) * arma::sum(beta_c, 1) + arma::inv(D) * xi);
       logp[c] = log(m_full[c]) +
         oeli::dmvnorm(beta(arma::span::all,n), mu_b, sig_b, true);
     }
     // TODO
     arma::vec b_new = xi;
     arma::mat Omega_new = reshape(
       Omega_full(arma::span::all, arma::span(0, C - 1)) *
         (m_full(arma::span(0,C-1))/sum(m_full(arma::span(0, C - 1)))), P_r, P_r);
     logp[C] = log(delta) +
       oeli::dmvnorm(beta(arma::span::all,n), b_new, D + Omega_new, true);

     // normalize probability vector
     arma::vec loc_probs = exp(logp - max(logp));
     loc_probs = loc_probs / sum(loc_probs);

     // draw new class membership
     int newz;
     if(C == Cmax){
       newz = Rcpp::as<int>(
         sample(seq(1, C), 1, false, loc_probs(arma::span(0, C - 1)))
       );
     } else {
       newz = Rcpp::as<int>(
         sample(seq(1, C + 1), 1, false, loc_probs(arma::span(0, C)))
       );
     }
     if (newz == C + 1) {
       b_full(arma::span::all, C) = b_new;
       Omega_full(arma::span::all, C) = reshape(Omega_new, P_r * P_r, 1);
       C += 1;
     }
     z[n] = newz;
     m_full[newz - 1] += 1;
   }

   // identify classes
   if (identify_classes == true) {
     arma::uvec sortind = arma::sort_index(m_full, "descend");
     b_full = b_full.cols(sortind);
     Omega_full = Omega_full.cols(sortind);
     z += Cmax;
     arma::vec sortind_vec = arma::conv_to<arma::vec>::from(sortind);
     for (int c = 0; c < C; c++) {
       for (int n = 0; n < N; n++) {
         if (z[n] == c + 1 + Cmax) z[n] = sortind_vec[c] + 1;
       }
     }
   }

   // return class updates
   return(
     Rcpp::List::create(
       Rcpp::Named("z") = z,
       Rcpp::Named("b") = b_full(arma::span::all, arma::span(0, C - 1)),
       Rcpp::Named("Omega") = Omega_full(arma::span::all, arma::span(0, C- 1 )),
       Rcpp::Named("C") = C
     )
   );
}

// [[Rcpp::export]]
Rcpp::List update_classes_dp2 (int Cmax, arma::mat beta, arma::vec z, arma::mat b, arma::mat Omega,
                              double delta, arma::vec xi, arma::mat D, int nu, arma::mat Theta,
                              bool s_desc = true) {

  // helper variables and functions
  Rcpp::Function sample("sample");
  Rcpp::Function seq("seq");

  // sizes
  int N = z.size();
  int C = b.n_cols;
  int P_r = b.n_rows;

  // space allocation for class characteristics
  arma::mat b_full(P_r,Cmax);
  b_full(arma::span::all,arma::span(0,C-1)) = b;
  arma::mat Omega_full(P_r*P_r,Cmax);
  Omega_full(arma::span::all,arma::span(0,C-1)) = Omega;
  arma::vec m_full(Cmax);
  m_full(arma::span(0,C-1)) = update_m(C, z, true);

  // Dirichlet process
  for(int n = 0; n<N; n++) {

    // un-assign initial class membership
    m_full[z[n]-1] -= 1;

    if(m_full[z[n]-1] == 0){
      // remove empty classes
      m_full[z[n]-1] = m_full[C-1];
      z.elem(find(z == C)).fill(z[n]);
      m_full[C-1] = 0;
      C -= 1;
      b_full(arma::span::all,z[n]-1) = arma::zeros<arma::vec>(P_r);
      Omega_full(arma::span::all,z[n]-1) = arma::zeros<arma::vec>(P_r*P_r);
    }

    // ensure that z[n] does not get counted
    z[n] = -1;

    // storage for class log-probabilities
    arma::vec logp(C+1);

    // update class characteristics
    for(int c = 0; c<C; c++) {

      // extract beta points currently allocated to class c
      arma::mat beta_c = beta.cols(find(z == c+1));

      // update Omega_c via mean of its posterior distribution
      arma::mat beta_c_diff_b = beta_c.each_col() - b_full.col(c);
      arma::mat Omega_c = (Theta + beta_c_diff_b * trans(beta_c_diff_b)) / (m_full[c] + nu - P_r - 1);

      // compute covariance (sig_b) and mean (mu_b) of posterior distribution of b_c
      arma::mat sig_b = arma::inv(arma::inv(D) + m_full[c] * arma::inv(Omega_c));
      arma::vec mu_b = sig_b * (arma::inv(Omega_c) * arma::sum(beta_c, 1) + arma::inv(D) * xi);

      // compute class assignment log-probabilities for existing classes from PPD
      logp[c] = log(m_full[c]) + oeli::dmvnorm(beta(arma::span::all,n), mu_b, sig_b + Omega_c, true);

      // save updates
      b_full(arma::span::all,c) = mu_b;
      Omega_full(arma::span::all,c) = reshape(Omega_c, P_r*P_r, 1);
    }

    // compute log-probability for new class
    arma::vec b_new = xi;
    arma::mat Omega_new = reshape(Omega_full(arma::span::all,arma::span(0,C-1)) * (m_full(arma::span(0,C-1))/sum(m_full(arma::span(0,C-1)))), P_r, P_r);
    logp[C] = log(delta) + oeli::dmvnorm(beta(arma::span::all,n), b_new, D + Omega_new, true);

    // transform log-probabilities to probabilities
    arma::vec loc_probs = exp(logp - max(logp));
    loc_probs = loc_probs / sum(loc_probs);

    // draw new class membership (prevent 'Cmax' from exceeding)
    int newz;
    if(C == Cmax){
      newz = Rcpp::as<int>(sample(seq(1,C),1,false,loc_probs(arma::span(0,C-1))));
    } else {
      newz = Rcpp::as<int>(sample(seq(1,C+1),1,false,loc_probs(arma::span(0,C))));
    }
    // spawn new class (but only if 'Cmax' is not exceeded)
    if(newz == C+1) {
      b_full(arma::span::all,C) = b_new;
      Omega_full(arma::span::all,C) = reshape(Omega_new, P_r*P_r, 1);
      C += 1;
    }
    z[n] = newz;
    m_full[newz-1] += 1;
  }

  // compute class weights
  arma::vec s_full = m_full / sum(m_full);

  // sort updates with respect to descending class weights s
  if(s_desc){
    arma::uvec sortind = arma::sort_index(s_full, "descend");
    s_full = s_full(sortind);
    b_full = b_full.cols(sortind);
    Omega_full = Omega_full.cols(sortind);
    z += Cmax;
    arma::vec sortind_vec = arma::conv_to<arma::vec>::from(sortind);
    for(int c = 0; c<C; c++) {
      for(int n = 0; n<N; n++) {
        if(z[n] == c+1+Cmax) z[n] = sortind_vec[c] + 1;
      }
    }
  }

  // return updates
  return(
    Rcpp::List::create(
      Rcpp::Named("z") = z,
      Rcpp::Named("b") = b_full(arma::span::all,arma::span(0,C-1)),
      Rcpp::Named("Omega") = Omega_full(arma::span::all,arma::span(0,C-1)),
      Rcpp::Named("s") = s_full(arma::span(0,C-1)),
      Rcpp::Named("C") = C)
    );
}

//' Gibbs sampler for probit models
//'
//' @details
//' This function is not supposed to be called directly, but rather via
//' \code{\link{fit_model}}.
//'
//' @param sufficient_statistics
//' The output of \code{\link{sufficient_statistics}}.
//'
//' @inheritParams fit_model
//'
//' @inheritParams RprobitB_data
//'
//' @param init
//' The output of \code{\link{set_initial_gibbs_values}}.
//'
//' @return
//' A list of Gibbs samples for
//' \itemize{
//'   \item \code{Sigma},
//'   \item \code{alpha} (if \code{P_f>0}),
//'   \item \code{s}, \code{z}, \code{b}, \code{Omega} (if \code{P_r>0}),
//'   \item \code{d} (if \code{ordered = TRUE}),
//' }
//' and a vector \code{class_sequence} of length \code{R}, where the \code{r}th
//' entry is the number of latent classes after iteration \code{r}.
//'
//' @keywords gibbs_sampler
//'
// [[Rcpp::export]]

Rcpp::List gibbs_sampler (
    Rcpp::List sufficient_statistics, Rcpp::List prior, Rcpp::List latent_classes,
    Rcpp::List fixed_parameter, Rcpp::List init, int R, int B, bool print_progress,
    bool ordered, bool ranked
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
  if(P_f>0){
    W = Rcpp::as<Rcpp::List>(sufficient_statistics["W"]);
    WkW = Rcpp::as<arma::mat>(sufficient_statistics["WkW"]);
  }
  if(P_r>0){
    X = Rcpp::as<Rcpp::List>(sufficient_statistics["X"]);
    XkX = Rcpp::as<Rcpp::List>(sufficient_statistics["XkX"]);
  }
  if(ranked){
    rdiff = Rcpp::as<Rcpp::List>(sufficient_statistics["rdiff"]);
  }

  // extract 'latent_classes' parameters
  int C = Rcpp::as<int>(latent_classes["C"]);
  int Cmax = 10;
  int Cdrawsize;
  int buffer = 50;
  double epsmin = 0.01;
  double epsmax = 0.99;
  double deltamin = 0.1;
  bool weight_update = Rcpp::as<bool>(latent_classes["weight_update"]);
  bool dp_update = Rcpp::as<bool>(latent_classes["dp_update"]);
  if(weight_update == false && dp_update == false){
    Cdrawsize = C;
  }
  else{
    Cmax = Rcpp::as<int>(latent_classes["Cmax"]);
    Cdrawsize = Cmax;
  }
  if(weight_update==true){
    buffer = Rcpp::as<int>(latent_classes["buffer"]);
    epsmin = Rcpp::as<double>(latent_classes["epsmin"]);
    epsmax = Rcpp::as<double>(latent_classes["epsmax"]);
    deltamin = Rcpp::as<double>(latent_classes["deltamin"]);
  }

  // extract 'prior' parameters
  int delta = 1;
  arma::vec eta;
  arma::mat Psiinv;
  int nu = P_r+2;
  arma::mat Theta;
  arma::vec xi;
  arma::mat D;
  arma::mat Dinv;
  int kappa = Rcpp::as<int>(prior["kappa"]);
  arma::mat E = Rcpp::as<arma::mat>(prior["E"]);
  arma::vec zeta;
  arma::mat Z;
  if(P_f>0){
    eta = Rcpp::as<arma::vec>(prior["eta"]);
    Psiinv = arma::inv(Rcpp::as<arma::mat>(prior["Psi"]));
  }
  if(P_r>0){
    delta = Rcpp::as<int>(prior["delta"]);
    xi = Rcpp::as<arma::vec>(prior["xi"]);
    D = Rcpp::as<arma::mat>(prior["D"]);
    Dinv = arma::inv(D);
    nu = Rcpp::as<int>(prior["nu"]);
    Theta = Rcpp::as<arma::mat>(prior["Theta"]);
  }
  if(ordered){
    zeta = Rcpp::as<arma::vec>(prior["zeta"]);
    Z = Rcpp::as<arma::mat>(prior["Z"]);
  }

  // extract 'init' parameters
  arma::vec m0;
  arma::vec z0;
  arma::vec alpha0;
  arma:: mat b0;
  arma::mat Omega0;
  arma::mat beta0;
  arma::mat U0 = Rcpp::as<arma::mat>(init["U0"]);
  arma::mat Sigma0 = Rcpp::as<arma::mat>(init["Sigma0"]);
  arma::vec d0;
  if(P_f>0){
    alpha0 = Rcpp::as<arma::vec>(init["alpha0"]);
  }
  if(P_r>0){
    m0 = Rcpp::as<arma::vec>(init["m0"]);
    z0 = Rcpp::as<arma::vec>(init["z0"]);
    b0 = Rcpp::as<arma::mat>(init["b0"]);
    Omega0 = Rcpp::as<arma::mat>(init["Omega0"]);
    beta0 = Rcpp::as<arma::mat>(init["beta0"]);
  }
  if(ordered){
    d0 = Rcpp::as<arma::vec>(init["d0"]);
  }

  // define helper variables and functions
  arma::vec s_cand;
  arma::mat Omega_c_inv;
  arma::vec b_c;
  arma::mat S;
  arma::mat IW;
  arma::vec eps;
  arma::mat mu_mat = arma::zeros<arma::mat>(N,Tvec.size());
  arma::mat mu_mat_tmp = arma::zeros<arma::mat>(1,1);
  int ind;
  int Jm1 = J-1;
  double old_ll = 0.0;
  Rcpp::List update_d_out;

  // allocate space for output
  arma::mat s_draws = arma::zeros<arma::mat>(R,Cdrawsize);
  arma::mat z_draws = arma::zeros<arma::mat>(R,N);
  arma::mat b_draws = arma::zeros<arma::mat>(R,P_r*Cdrawsize);
  arma::mat Omega_draws = arma::zeros<arma::mat>(R,P_r*P_r*Cdrawsize);
  arma::mat alpha_draws = arma::zeros<arma::mat>(R,P_f);
  arma::mat Sigma_draws;
  if(ordered) {
    Sigma_draws = arma::zeros<arma::mat>(R,1);
  } else {
    Sigma_draws = arma::zeros<arma::mat>(R,Jm1*Jm1);
  }
  arma::mat d_draws = arma::zeros<arma::mat>(R,J-2);
  arma::vec class_sequence(R);

  // set initial values
  arma::vec s = arma::ones(C) / C;
  arma::vec z = z0;
  arma::vec m = m0;
  arma::mat b = b0;
  arma::mat Omega = Omega0;
  arma::mat U = U0;
  arma::vec alpha = alpha0;
  arma::mat beta = beta0;
  arma::mat Sigma = Sigma0;
  arma::mat Sigmainv = arma::inv(Sigma);
  arma::vec d = d0;
  arma::vec gamma;
  if (ordered) gamma = d_to_gamma(d0);

  // set fixed parameter values
  bool do_update_s = true;
  if(fixed_parameter.containsElementNamed("s")){
    do_update_s = false;
    s = Rcpp::as<arma::vec>(fixed_parameter["s"]);
  }
  bool do_update_z = true;
  if(fixed_parameter.containsElementNamed("z")){
    do_update_s = false;
    z = Rcpp::as<arma::vec>(fixed_parameter["z"]);
  }
  bool do_update_b = true;
  if(fixed_parameter.containsElementNamed("b")){
    do_update_b = false;
    b = Rcpp::as<arma::mat>(fixed_parameter["b"]);
  }
  bool do_update_Omega = true;
  if(fixed_parameter.containsElementNamed("Omega")){
    do_update_Omega = false;
    Omega = Rcpp::as<arma::mat>(fixed_parameter["Omega"]);
  }
  bool do_update_alpha = true;
  if(fixed_parameter.containsElementNamed("alpha")){
    do_update_alpha = false;
    alpha = Rcpp::as<arma::vec>(fixed_parameter["alpha"]);
  }
  bool do_update_beta = true;
  if(fixed_parameter.containsElementNamed("beta")){
    do_update_beta = false;
    beta = Rcpp::as<arma::mat>(fixed_parameter["beta"]);
  }
  bool do_update_Sigma = true;
  if(fixed_parameter.containsElementNamed("Sigma")){
    do_update_Sigma = false;
    Sigma = Rcpp::as<arma::mat>(fixed_parameter["Sigma"]);
    Sigmainv = arma::inv(Sigma);
  }
  if(ordered){
    do_update_Sigma = false;
    Sigma = arma::ones<arma::mat>(1,1);
    Sigmainv = arma::inv(Sigma);
  }

  // set progress output
  Rcpp::Environment pkg = Rcpp::Environment::namespace_env("RprobitB");
  Rcpp::Function RprobitB_pp = pkg["RprobitB_pp"];

  // start loop
  for(int r = 0; r<R; r++) {

    // print progress
    if(print_progress && ((r+1)%10 == 0 || r == 0)){
      if(weight_update==false && dp_update==false){
        RprobitB_pp("MCMC iteration", r+1, R);
      } else {
        RprobitB_pp("MCMC iteration (C = " + std::to_string(C) + ")", r+1, R);
      }
    }

    // check for code interruption by user
    Rcpp::checkUserInterrupt();

    if(P_r>0){

      // save number of classes in each iteration
      class_sequence[r] = C;

      // update s, z, m, b, Omega by posterior draws if
      // - no DP or
      // - outside updating period
      if(dp_update == false || r+1 > B || r == 0){

        // update s (but only if draw is descending)
        if(do_update_s) {
          arma::vec s_cand = update_s(delta,m);
          if(std::is_sorted(std::begin(s_cand),std::end(s_cand),
                            std::greater_equal<double>())){
            s = s_cand;
          }
        }

        // update z
        if(do_update_z) {
          z = update_z(s, beta, b, Omega);
        }

        // update m
        m = update_m(C, z, true);

        // update b
        if(do_update_b) {
          b = update_b(beta, Omega, z, m, xi, Dinv);
        }

        // update Omega
        if(do_update_Omega) {
          Omega = update_Omega(beta, b, z, m, nu, Theta);
        }
      }

      // update beta
      if(do_update_beta) {
        for(int n = 0; n<N; n++){
          for(int c = 0; c<C; c++){
            if(z[n]==c+1){
              Omega_c_inv = arma::inv(reshape(Omega(arma::span::all,c),P_r,P_r));
              b_c = b(arma::span::all,c);
            }
          }
          arma::mat XSigX;
          if (ordered){
            XSigX = reshape(Rcpp::as<arma::mat>(XkX[n]),P_r,P_r);
          } else {
            XSigX = reshape(Rcpp::as<arma::mat>(XkX[n])*reshape(Sigmainv,Jm1*Jm1,1),P_r,P_r);
          }
          arma::vec XSigU = arma::zeros<arma::vec>(P_r);
          for(int t = 0; t<Tvec[n]; t++){
            ind = csTvec[n]+t;
            if(P_f==0)
              XSigU += trans(Rcpp::as<arma::mat>(X[ind]))*Sigmainv*U(arma::span::all,ind);
            if(P_f>0)
              XSigU += trans(Rcpp::as<arma::mat>(X[ind]))*Sigmainv*U(arma::span::all,ind)-trans(Rcpp::as<arma::mat>(X[ind]))*Sigmainv*Rcpp::as<arma::mat>(W[ind])*alpha;
          }
          beta(arma::span::all,n) = update_reg(b_c,Omega_c_inv,XSigX,XSigU);
        }
      }

      // weight-based update of classes
      if (weight_update == true && (r + 1) <= B && (r + 1) % buffer == 0) {
        Rcpp::List class_update = update_classes_wb(
          epsmin, epsmax, deltamin, s, b, Omega, Cmax, true
        );
        s = Rcpp::as<arma::vec>(class_update["s"]);
        C = s.size();
        b = Rcpp::as<arma::mat>(class_update["b"]);
        Omega = Rcpp::as<arma::mat>(class_update["Omega"]);
        z = update_z(s, beta, b, Omega);
        m = update_m(C, z, true);
      }

      // dp-based update of classes
      if (dp_update == true && (r + 1) <= B) {
        Rcpp::List class_update = update_classes_dp(
          beta, z, b, Omega, delta, xi, D, nu, Theta, Cmax, true
        );
        z = Rcpp::as<arma::vec>(class_update["z"]);
        b = Rcpp::as<arma::mat>(class_update["b"]);
        Omega = Rcpp::as<arma::mat>(class_update["Omega"]);
        C = Rcpp::as<int>(class_update["C"]);
        m = update_m(C, z, true);
      }
    }

    if(P_f>0){
      // update alpha
      if(do_update_alpha) {
        arma::mat WSigW;
        if (ordered) {
          WSigW = reshape(WkW,P_f,P_f);
        } else {
          WSigW = reshape(WkW*reshape(Sigmainv,Jm1*Jm1,1),P_f,P_f);
        }
        arma::vec WSigU = arma::zeros<arma::vec>(P_f);
        for(int n = 0; n<N; n++){
          for(int t = 0; t<Tvec[n]; t++){
            ind = csTvec[n]+t;
            if(P_r==0)
              WSigU += trans(Rcpp::as<arma::mat>(W[ind]))*Sigmainv*U(arma::span::all,ind);
            if(P_r>0)
              WSigU += trans(Rcpp::as<arma::mat>(W[ind]))*Sigmainv*(U(arma::span::all,ind)-Rcpp::as<arma::mat>(X[ind])*beta(arma::span::all,n));
          }
        }
        alpha = update_reg(eta,Psiinv,WSigW,WSigU);
      }
    }

    // update U
    if(ordered) {
      for(int n = 0; n<N; n++){
        for(int t = 0; t<Tvec[n]; t++){
          ind = csTvec[n]+t;
          if(P_f>0 && P_r>0) {
            mu_mat_tmp = Rcpp::as<arma::mat>(W[ind])*alpha+Rcpp::as<arma::mat>(X[ind])*beta(arma::span::all,n);
          }
          if(P_f>0 && P_r==0) {
            mu_mat_tmp = Rcpp::as<arma::mat>(W[ind])*alpha;
          }
          if(P_f==0 && P_r>0) {
            mu_mat_tmp = Rcpp::as<arma::mat>(X[ind])*beta(arma::span::all,n);
          }
          if(P_f==0 && P_r==0) {
            mu_mat_tmp = arma::zeros<arma::mat>(1,1);
          }
          U(arma::span::all,ind) = oeli::rttnorm(mu_mat_tmp(0,0), 1.0, gamma[y(n,t)], gamma[y(n,t)-1]);
        }
      }
    } else if (ranked) {
      for(int n = 0; n<N; n++){
        for(int t = 0; t<Tvec[n]; t++){
          ind = csTvec[n]+t;
          arma::mat rdiff_tmp = Rcpp::as<arma::mat>(rdiff[y(n,t)-1]);
          arma::vec U_tmp = rdiff_tmp * U(arma::span::all,ind);
          arma::vec mu_vec_tmp = arma::zeros<arma::vec>(Jm1);
          arma::mat Sigmainv_tmp = arma::inv(rdiff_tmp * Sigma * trans(rdiff_tmp));
          if(P_f>0 && P_r>0) {
            mu_vec_tmp = rdiff_tmp * Rcpp::as<arma::mat>(W[ind]) * alpha +
              rdiff_tmp * Rcpp::as<arma::mat>(X[ind]) * beta(arma::span::all,n);
          }
          if(P_f>0 && P_r==0) {
            mu_vec_tmp = rdiff_tmp * Rcpp::as<arma::mat>(W[ind]) * alpha;
          }
          if(P_f==0 && P_r>0) {
            mu_vec_tmp = rdiff_tmp * Rcpp::as<arma::mat>(X[ind]) * beta(arma::span::all,n);
          }
          U(arma::span::all,ind) = arma::inv(rdiff_tmp) *
            update_U_ranked(U_tmp, mu_vec_tmp, Sigmainv_tmp);
        }
      }
    } else {
      for(int n = 0; n<N; n++){
        for(int t = 0; t<Tvec[n]; t++){
          ind = csTvec[n]+t;
          if(P_f>0 && P_r>0) {
            U(arma::span::all,ind) = update_U(U(arma::span::all,ind), y(n,t),
              Rcpp::as<arma::mat>(W[ind])*alpha+Rcpp::as<arma::mat>(X[ind])*beta(arma::span::all,n), Sigmainv);
          }
          if(P_f>0 && P_r==0) {
            U(arma::span::all,ind) = update_U(U(arma::span::all,ind), y(n,t),
              Rcpp::as<arma::mat>(W[ind])*alpha, Sigmainv);
          }
          if(P_f==0 && P_r>0) {
            U(arma::span::all,ind) = update_U(U(arma::span::all,ind), y(n,t),
              Rcpp::as<arma::mat>(X[ind])*beta(arma::span::all,n), Sigmainv);
          }
          if(P_f==0 && P_r==0) {
            arma::vec mu_null(Jm1);
            U(arma::span::all,ind) = update_U(U(arma::span::all,ind), y(n,t),
              mu_null, Sigmainv);
          }
        }
      }
    }

    // update Sigma
    if(do_update_Sigma){
      S = arma::zeros<arma::mat>(Jm1,Jm1);
      for(int n = 0; n<N; n++){
        for(int t = 0; t<Tvec[n]; t++){
          ind = csTvec[n]+t;
          if(P_f>0 && P_r>0)
            eps = U(arma::span::all,ind) - Rcpp::as<arma::mat>(W[ind])*alpha - Rcpp::as<arma::mat>(X[ind])*beta(arma::span::all,n);
          if(P_f>0 && P_r==0)
            eps = U(arma::span::all,ind) - Rcpp::as<arma::mat>(W[ind])*alpha;
          if(P_f==0 && P_r>0)
            eps = U(arma::span::all,ind) - Rcpp::as<arma::mat>(X[ind])*beta(arma::span::all,n);
          if(P_f==0 && P_r==0)
            eps = U(arma::span::all,ind);
          S += eps * trans(eps);
        }
      }
      Sigma = update_Sigma(kappa, E, sum(Tvec), S);
      Sigmainv = arma::inv(Sigma);
    }

    // update d (for the ordered probit model)
    if (ordered) {
      for(int n = 0; n<N; n++){
        for(int t = 0; t<Tvec[n]; t++){
          ind = csTvec[n]+t;
          if(P_f>0 && P_r>0)
            mu_mat_tmp = Rcpp::as<arma::mat>(W[ind])*alpha+Rcpp::as<arma::mat>(X[ind])*beta(arma::span::all,n);
          if(P_f>0 && P_r==0)
            mu_mat_tmp = Rcpp::as<arma::mat>(W[ind])*alpha;
          if(P_f==0 && P_r>0)
            mu_mat_tmp = Rcpp::as<arma::mat>(X[ind])*beta(arma::span::all,n);
          if(P_f==0 && P_r==0){
            mu_mat_tmp = arma::zeros<arma::mat>(1,1);
          }
          mu_mat(n,t) = mu_mat_tmp(0,0);
        }
      }
      if(r == 0) old_ll = ll_ordered(d, y, mu_mat, Tvec);
      update_d_out = update_d(d, y, mu_mat, old_ll, zeta, Z, Tvec);
      d = Rcpp::as<arma::vec>(update_d_out["d"]);
      old_ll = Rcpp::as<double>(update_d_out["ll"]);
    }

    // save draws
    if (P_f > 0) {
      alpha_draws(r, arma::span::all) = trans(alpha);
    }
    if (P_r > 0) {
      s_draws(r, arma::span(0, s.size() - 1)) = trans(s);
      z_draws(r, arma::span::all) = trans(z);
      arma::vec vectorise_b = vectorise(b);
      b_draws(r, arma::span(0, vectorise_b.size() - 1)) = trans(vectorise_b);
      arma::vec vectorise_Omega = vectorise(Omega);
      Omega_draws(r, arma::span(0, vectorise_Omega.size() - 1)) =
        trans(vectorise_Omega);
    }
    Sigma_draws(r, arma::span::all) = trans(vectorise(Sigma));
    if (ordered == true) {
      d_draws(r, arma::span::all) = trans(d);
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
