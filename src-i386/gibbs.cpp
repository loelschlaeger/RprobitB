// [[Rcpp::depends("RcppArmadillo")]]
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

//' Update class weight vector
//' @description
//' This function updates the class weight vector by drawing from its posterior distribution.
//' @inheritParams check_prior
//' @param m
//' The vector of current class frequencies.
//' @return
//' A vector, a draw from the Dirichlet posterior distribution for \code{s}.
//' @details
//' Let \eqn{m=(m_1,\dots,m_C)} be the frequencies of \eqn{C} classes.
//' Given the class weight (probability) vector \eqn{s=(s_1,\dots,s_C)}, the distribution
//' of \eqn{m} is multinomial and its likelihood is \deqn{L(m\mid s) \propto \prod_{i=1}^C s_i^{m_i}.}
//' The conjugate prior \eqn{p(s)} for \eqn{s} is a Dirichlet distribution, which has a density function
//' proportional to \deqn{\prod_{i=1}^C s_i^{\delta_i-1},} where \eqn{\delta = (\delta_1,\dots,\delta_C)}
//' is the concentration parameter vector.
//' Note that in RprobitB, \eqn{\delta_1=\dots=\delta_C}. This restriction is necessary because the class number \eqn{C} can change.
//' The posterior distribution of \eqn{s} is proportional to \deqn{p(s) L(m\mid s) \propto \prod_{i=1}^C s_i^{\delta_i + m_i - 1},}
//' which in turn is proportional to a Dirichlet distribution with parameters \eqn{\delta+m}.
//' @examples
//' ### number of classes
//' C <- 4
//' ### current class sizes
//' m <- sample.int(C)
//' ### concentration parameter for Dirichlet prior (single-valued)
//' delta <- 1
//' ### updated class weight vector
//' update_s(delta = 1, m = m)
//' @export
//' @keywords
//' posterior
//'
// [[Rcpp::export]]
arma::vec update_s (int delta, arma::vec m) {
  int C = m.size();
  return(rdirichlet(delta*ones(C)+m));
}

//' Update class allocation vector
//' @description
//' This function updates the class allocation vector (independently for all observations)
//' by drawing from its conditional distribution.
//' @inheritParams RprobitB_parameter
//' @details
//' Let \eqn{z = (z_1,\dots,z_N)} denote the class allocation vector of the observations (mixed coefficients) \eqn{\beta = (\beta_1,\dots,\beta_N)}.
//' Independently for each \eqn{n}, the conditional probability \eqn{\Pr(z_n = c \mid s,\beta_n,b,\Omega)} of having \eqn{\beta_n}
//' allocated to class \eqn{c} for \eqn{c=1,\dots,C} depends on the class allocation vector \eqn{s}, the class means \eqn{b=(b_c)_c} and the class covariance
//' matrices \eqn{Omega=(Omega_c)_c} and is proportional to \deqn{s_c \phi(\beta_n \mid b_c,Omega_c).}
//' @return
//' An updated class allocation vector.
//' @examples
//' ### class weights for C = 2 classes
//' s <- rdirichlet(c(1,1))
//' ### coefficient vector for N = 1 decider and P_r = 2 random coefficients
//' beta <- matrix(c(1,1), ncol = 1)
//' ### class means and covariances
//' b <- cbind(c(0,0),c(1,1))
//' Omega <- cbind(c(1,0,0,1),c(1,0,0,1))
//' ### updated class allocation vector
//' update_z(s = s, beta = beta, b = b, Omega = Omega)
//' @export
//' @keywords
//' posterior
//'
// [[Rcpp::export]]
arma::vec update_z (arma::vec s, arma::mat beta, arma::mat b, arma::mat Omega) {
  Rcpp::Function sample("sample");
  int N = beta.n_cols;
  int C = s.size();
  int P_r = b.n_rows;
  arma::vec z = zeros<vec>(N);
  arma::vec prob_z = zeros<vec>(C);
  for(int n = 0; n<N; n++){
    for(int c = 0; c<C; c++){
      prob_z[c] = s[c]*dmvnorm(beta(span::all,n),b(span::all,c),reshape(Omega(span::all,c),P_r,P_r));
    }
    z[n] = as<int>(sample(seq(1,C),1,false,prob_z));
  }
  return(z);
}

//' Update class sizes
//' @description
//' This function updates the class size vector.
//' @inheritParams RprobitB_parameter
//' @param nozero
//' If \code{TRUE}, each element in the output vector \code{m} is at least one
//' (for numerical stability).
//' @return
//' An updated class size vector.
//' @examples
//' update_m(C = 3, z = c(1,1,1,2,2,3))
//' @export
//' @keywords
//' posterior
//'
// [[Rcpp::export]]
arma::vec update_m (int C, arma::vec z, bool nozero = false) {
  int N = z.size();
  arma::vec m(C);
  for(int c = 0; c<C; c++){
    for(int n = 0; n<N; n++){
      if(z[n]==c+1) m[c] += 1;
    }
  }
  if(nozero==true){
    for(int c = 0; c<C; c++){
      if(m[c]==0) m[c] = 1;
    }
  }
  return(m);
}

//' Update class means
//' @description
//' This function updates the class means (independent from the other classes).
//' @inheritParams RprobitB_parameter
//' @param m
//' The vector of class sizes of length \code{C}.
//' @inheritParams check_prior
//' @param Dinv
//' The precision matrix (i.e. the inverse of the covariance matrix) of dimension \code{P_r} x \code{P_r}
//' of the normal prior for each \code{b_c}.
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
//' @return
//' A matrix of updated means for each class in columns.
//' @examples
//' ### N = 100 decider, P_r = 2 random coefficients, and C = 2 latent classes
//' N <- 100
//' (b_true <- cbind(c(0,0),c(1,1)))
//' Omega <- matrix(c(1,0.3,0.3,0.5,1,-0.3,-0.3,0.8), ncol=2)
//' z <- c(rep(1,N/2),rep(2,N/2))
//' m <- as.numeric(table(z))
//' beta <- sapply(z, function(z) rmvnorm(b_true[,z], matrix(Omega[,z],2,2)))
//' ### prior mean vector and precision matrix (inverse of covariance matrix)
//' xi <- c(0,0)
//' Dinv <- diag(2)
//' ### updated class means (in columns)
//' update_b(beta = beta, Omega = Omega, z = z, m = m, xi = xi, Dinv = Dinv)
//' @export
//' @keywords
//' posterior
//'
// [[Rcpp::export]]
arma::mat update_b (arma::mat beta, arma::mat Omega, arma::vec z, arma::vec m, arma::vec xi, arma::mat Dinv) {
  int P_r = beta.n_rows;
  int C = m.size();
  int N = beta.n_cols;
  arma::mat b_draw = zeros<mat>(P_r,C);
  arma::mat b_bar = zeros<mat>(P_r,C);
  for(int c = 0; c<C; c++){
    for(int n = 0; n<N; n++){
      if(z[n]==c+1) b_bar(span::all,c) += beta(span::all,n);
    }
    b_bar(span::all,c) /= m[c];
  }
  for(int c = 0; c<C; c++){
    arma::mat Omega_c_inv = arma::inv(reshape(Omega(span::all,c),P_r,P_r));
    b_draw(span::all,c) = rmvnorm(arma::inv(Dinv+m[c]*Omega_c_inv) * (Dinv*xi+m[c]*Omega_c_inv*b_bar(span::all,c)), arma::inv(Dinv+m[c]*Omega_c_inv));
  }
  return(b_draw);
}

//' Update class covariances
//' @description
//' This function updates the class covariances (independent from the other classes).
//' @inheritParams RprobitB_parameter
//' @param m
//' The vector of class sizes of length \code{C}.
//' @inheritParams check_prior
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
//' @return
//' A matrix of updated covariance matrices for each class in columns.
//' @examples
//' ### N = 100 decider, P_r = 2 random coefficients, and C = 2 latent classes
//' N <- 100
//' b <- cbind(c(0,0),c(1,1))
//' (Omega_true <- matrix(c(1,0.3,0.3,0.5,1,-0.3,-0.3,0.8), ncol=2))
//' z <- c(rep(1,N/2),rep(2,N/2))
//' m <- as.numeric(table(z))
//' beta <- sapply(z, function(z) rmvnorm(b[,z], matrix(Omega_true[,z],2,2)))
//' ### degrees of freedom and scale matrix for the Wishart prior
//' nu <- 1
//' Theta <- diag(2)
//' ### updated class covariance matrices (in columns)
//' update_Omega(beta = beta, b = b, z = z, m = m, nu = nu, Theta = Theta)
//' @export
//' @keywords
//' posterior
//'
// [[Rcpp::export]]
arma::mat update_Omega (arma::mat beta, arma::mat b, arma::vec z, arma::vec m, int nu, arma::mat Theta) {
  int P_r = beta.n_rows;
  int C = m.size();
  int N = beta.n_cols;
  arma::mat Omega = zeros<mat>(P_r*P_r,C);
  for(int c = 0; c<C; c++){
    arma::mat sum_sp = zeros<mat>(P_r,P_r);
    for(int n = 0; n<N; n++){
      if(z[n]==c+1){
        sum_sp += (beta(span::all,n)-b(span::all,c)) * trans(beta(span::all,n)-b(span::all,c));
      }
    }
    arma::mat Omega_draw = as<mat>(rwishart(nu+m[c],arma::inv(Theta+sum_sp))["IW"]);
    Omega(span::all,c) = reshape(Omega_draw,P_r*P_r,1);
  }
  return(Omega);
}

//' Update coefficient vector of multiple linear regression
//' @description
//' This function updates the coefficient vector of a multiple linear regression.
//' @param mu0
//' The mean vector of the normal prior distribution for the coefficient vector.
//' @param Tau0
//' The precision matrix (i.e. inverted covariance matrix) of the normal prior distribution for the coefficient vector.
//' @param XSigX
//' The matrix \eqn{\sum_{n=1}^N X_n'\Sigma^{-1}X_n}. See below for details.
//' @param XSigU
//' The vector \eqn{\sum_{n=1}^N X_n'\Sigma^{-1}U_n}. See below for details.
//' @details
//' This function draws from the posterior distribution of \eqn{\beta} in the linear utility
//' equation \deqn{U_n = X_n\beta + \epsilon_n,} where \eqn{U_n} is the
//' (latent, but here assumed to be known) utility vector of decider \eqn{n = 1,\dots,N}, \eqn{X_n}
//' is the design matrix build from the choice characteristics faced by \eqn{n},
//' \eqn{\beta} is the unknown coefficient vector (this can be either the fixed
//' coefficient vector \eqn{\alpha} or the decider-specific coefficient vector \eqn{\beta_n}),
//' and \eqn{\epsilon_n} is the error term assumed to be normally distributed with mean \eqn{0}
//' and (known) covariance matrix \eqn{\Sigma}.
//' A priori we assume the (conjugate) normal prior distribution \deqn{\beta \sim N(\mu_0,\Tau_0)}
//' with mean vector \eqn{\mu_0} and precision matrix (i.e. inverted covariance matrix) \eqn{\Tau_0}.
//' The posterior distribution for \eqn{\beta} is normal with
//' covariance matrix \deqn{\Sigma_1 = (\Tau_0 + \sum_{n=1}^N X_n'\Sigma^{-1}X_n)^{-1}} and mean vector
//' \deqn{\mu_1 = \Sigma_1(\Tau_0\mu_0 + \sum_{n=1}^N X_n'\Sigma^{-1}U_n)}.
//' Note the analogy of \eqn{\mu_1} to the generalized least squares estimator
//' \deqn{\hat{\beta}_\text{GLS} = (\sum_{n=1}^N X_n'\Sigma^{-1}X_n)^{-1} \sum_{n=1}^N X_n'\Sigma^{-1}U_n} which
//' becomes weighted by the prior parameters \eqn{\mu_0} and \eqn{\Tau_0}.
//' @return
//' A vector, a draw from the normal posterior distribution of the coefficient
//' vector in a multiple linear regression.
//' @examples
//' ### true coefficient vector
//' beta_true <- matrix(c(-1,1), ncol=1)
//' ### error term covariance matrix
//' Sigma <- matrix(c(1,0.5,0.2,0.5,1,0.2,0.2,0.2,2), ncol=3)
//' ### draw data
//' N <- 100
//' X <- replicate(N, matrix(rnorm(6), ncol=2), simplify = FALSE)
//' eps <- replicate(N, rmvnorm(mu = c(0,0,0), Sigma = Sigma), simplify = FALSE)
//' U <- mapply(function(X, eps) X %*% beta_true + eps, X, eps, SIMPLIFY = FALSE)
//' ### prior parameters for coefficient vector
//' mu0 <- c(0,0)
//' Tau0 <- diag(2)
//' ### draw from posterior of coefficient vector
//' XSigX <- Reduce(`+`, lapply(X, function(X) t(X) %*% solve(Sigma) %*% X))
//' XSigU <- Reduce(`+`, mapply(function(X, U) t(X) %*% solve(Sigma) %*% U, X, U, SIMPLIFY = FALSE))
//' beta_draws <- replicate(100, update_reg(mu0, Tau0, XSigX, XSigU), simplify = TRUE)
//' rowMeans(beta_draws)
//' @export
//' @importFrom stats sd
//' @keywords
//' posterior
//'
// [[Rcpp::export]]
arma::vec update_reg (arma::vec mu0, arma::mat Tau0, arma::mat XSigX, arma::vec XSigU) {
  arma::mat Sigma1 = arma::inv(Tau0 + XSigX);
  arma::mat mu1 = Sigma1 * (Tau0 * mu0 + XSigU);
  return(rmvnorm(mu1, Sigma1));
}

//' Update error term covariance matrix of multiple linear regression
//' @description
//' This function updates the error term covariance matrix of a multiple linear regression.
//' @param N
//' The draw size.
//' @param S
//' A matrix, the sum over the outer products of the residuals \eqn{(\epsilon_n)_{n=1,\dots,N}}.
//' @inheritParams check_prior
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
//' @return
//' A matrix, a draw from the Inverse Wishart posterior distribution of the error term
//' covariance matrix in a multiple linear regression.
//' @examples
//' ### true error term covariance matrix
//' (Sigma_true <- matrix(c(1,0.5,0.2,0.5,1,0.2,0.2,0.2,2), ncol=3))
//' ### coefficient vector
//' beta <- matrix(c(-1,1), ncol=1)
//' ### draw data
//' N <- 100
//' X <- replicate(N, matrix(rnorm(6), ncol=2), simplify = FALSE)
//' eps <- replicate(N, rmvnorm(mu = c(0,0,0), Sigma = Sigma_true), simplify = FALSE)
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
//' @export
//' @keywords
//' posterior
//'
// [[Rcpp::export]]
arma::mat update_Sigma (int kappa, arma::mat E, int N, arma::mat S) {
  return(as<mat>(rwishart(kappa+N,arma::inv(E+S))["IW"]));
}

//' Update latent utility vector
//' @description
//' This function updates the latent utility vector, where (independent across deciders and choice occasions)
//' the utility for each alternative is updated conditional on the other utilities.
//' @param U
//' The current utility vector of length \code{J-1}.
//' @param y
//' An integer from \code{1} to \code{J}, the index of the chosen alternative.
//' @param sys
//' A vector of length \code{J-1}, the systematic utility part.
//' @param Sigmainv
//' The inverted error term covariance matrix of dimension \code{J-1} x \code{J-1}.
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
//' @references
//' See Geweke (1998) \emph{Efficient Simulation from the Multivariate Normal and Student-t Distributions Subject
//' to Linear Constraints and the Evaluation of Constraint Probabilities} for Gibbs sampling
//' from a truncated multivariate normal distribution. See Oelschläger and Bauer (2020) \emph{Bayes Estimation
//' of Latent Class Mixed Multinomial Probit Models} for its application to probit utilities.
//' @return
//' An updated utility vector of length \code{J-1}.
//' @examples
//' U <- c(0,0,0)
//' y <- 3
//' sys <- c(0,0,0)
//' Sigmainv <- solve(diag(3))
//' update_U(U, y, sys, Sigmainv)
//' @export
//' @keywords
//' posterior
//'
// [[Rcpp::export]]
arma::vec update_U (arma::vec U, int y, arma::vec sys, arma::mat Sigmainv) {
  int Jm1 = U.size();
  bool above;
  double bound;
  double m;
  vec U_update = U;
  vec maxInd(2);
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
    U_update[i] = rtnorm(sys[i]+m, sqrt(1/Sigmainv(Jm1*i+i)), bound, above);
  }
  return (U_update);
}

//' Gibbs sampler for the (mixed) multinomial probit model
//'
//' @description
//' This function draws Gibbs samples from the posterior distribution of the
//' (mixed) multinomial probit model parameters.
//'
//' @details
//' This function is not supposed to be called directly, but rather via \code{\link{mcmc}}.
//'
//' @param sufficient_statistics
//' The output of \code{\link{sufficient_statistics}}.
//' @inheritParams mcmc
//' @param init
//' The output of \code{\link{set_initial_gibbs_values}}.
//' @return
//' A list of Gibbs samples for
//' \itemize{
//'   \item \code{Sigma},
//'   \item \code{alpha} (if \code{P_f>0}),
//'   \item \code{s} (if \code{P_r>0}),
//'   \item \code{b} (if \code{P_r>0}),
//'   \item \code{Omega} (if \code{P_r>0}),
//' }
//' and (if \code{P_r>0}) a vector \code{classification} of class memberships for each decider and
//' a vector \code{class_sequence} of length \code{R}, where the \code{r}th entry is the number of
//' latent classes after iteration \code{r}.
//'
//' @keywords
//' internal
//'
// [[Rcpp::export]]
List gibbs_sampling (List sufficient_statistics, List prior, List latent_classes,
                     List init, int R, int B, bool print_progress) {

  // extract 'sufficient_statistics' parameters
  int N = as<int>(sufficient_statistics["N"]);
  int J = as<int>(sufficient_statistics["J"]);
  int P_f = as<int>(sufficient_statistics["P_f"]);
  int P_r = as<int>(sufficient_statistics["P_r"]);
  vec Tvec = as<vec>(sufficient_statistics["Tvec"]);
  vec csTvec = as<vec>(sufficient_statistics["csTvec"]);
  List W;
  List X;
  mat y = as<mat>(sufficient_statistics["y"]);
  mat WkW;
  List XkX;
  if(P_f>0){
    W = as<List>(sufficient_statistics["W"]);
    WkW = as<mat>(sufficient_statistics["WkW"]);
  }
  if(P_r>0){
    X = as<List>(sufficient_statistics["X"]);
    XkX = as<List>(sufficient_statistics["XkX"]);
  }

  // extract 'latent_classes' parameters
  int C = as<int>(latent_classes["C"]);
  int Cmax = 10;
  int Cdrawsize;
  int buffer = 50;
  double epsmin = 0.01;
  double epsmax = 0.99;
  double distmin = 0.1;
  bool weight_update = as<bool>(latent_classes["weight_update"]);
  bool dp_update = as<bool>(latent_classes["dp_update"]);
  if(weight_update==false && dp_update==false){
    Cdrawsize = C;
  }
  else{
    Cmax = as<int>(latent_classes["Cmax"]);
    Cdrawsize = Cmax;
  }
  if(weight_update==true){
    buffer = as<int>(latent_classes["buffer"]);
    epsmin = as<double>(latent_classes["epsmin"]);
    epsmax = as<double>(latent_classes["epsmax"]);
    distmin = as<double>(latent_classes["distmin"]);
  }

  // extract 'prior' parameters
  int delta = 1;
  vec eta;
  mat Psiinv;
  int nu = P_r+2;
  mat Theta;
  vec xi;
  mat D;
  mat Dinv;
  int kappa = as<int>(prior["kappa"]);
  mat E = as<mat>(prior["E"]);
  if(P_f>0){
    eta = as<vec>(prior["eta"]);
    Psiinv = arma::inv(as<mat>(prior["Psi"]));
  }
  if(P_r>0){
    delta = as<int>(prior["delta"]);
    xi = as<vec>(prior["xi"]);
    D = as<mat>(prior["D"]);
    Dinv = arma::inv(D);
    nu = as<int>(prior["nu"]);
    Theta = as<mat>(prior["Theta"]);
  }

  // extract 'init' parameters
  vec m0;
  vec z0;
  vec alpha0;
  mat b0;
  mat Omega0;
  mat beta0;
  mat U0 = as<mat>(init["U0"]);
  mat Sigma0 = as<mat>(init["Sigma0"]);
  if(P_f>0){
    alpha0 = as<vec>(init["alpha0"]);
  }
  if(P_r>0){
    m0 = as<vec>(init["m0"]);
    z0 = as<vec>(init["z0"]);
    b0 = as<mat>(init["b0"]);
    Omega0 = as<mat>(init["Omega0"]);
    beta0 = as<mat>(init["beta0"]);
  }

  // define helper variables and functions
  vec s_cand;
  mat Omega_c_inv;
  vec b_c;
  mat S;
  mat IW;
  vec eps;
  int ind;
  char buf[50];
  int nprint = round(R/10);
  int Jm1 = J - 1;
  Rcpp::Environment package_env("package:RprobitB");
  Rcpp::Function update_classes_dp = package_env["update_classes_dp"];

  // allocate space for draws
  mat alpha_draws = zeros<mat>(R,P_f);
  mat s_draws = zeros<mat>(R,Cdrawsize);
  mat b_draws = zeros<mat>(R,P_r*Cdrawsize);
  mat Omega_draws = zeros<mat>(R,P_r*P_r*Cdrawsize);
  mat Sigma_draws = zeros<mat>(R,Jm1*Jm1);
  arma::vec class_sequence(R);

  // define updating variables and set initial values
  vec s(C);
  vec z = z0;
  vec m = m0;
  mat b = b0;
  mat Omega = Omega0;
  mat U = U0;
  vec alpha = alpha0;
  mat beta = beta0;
  mat Sigmainv = arma::inv(Sigma0);

  // start loop
  if(print_progress) start_timer();
  for(int r = 0; r<R; r++) {

    if(P_r>0){

      // update s, z, m, b, Omega by posterior draws only if no DP or outside burning period
      if(dp_update == false || r+1 > B){
        // update s (but only if draw is descending)
        arma::vec s_cand = update_s(delta,m);
        if(std::is_sorted(std::begin(s_cand),std::end(s_cand),std::greater<double>())){
          s = s_cand;
        }

        // update z
        z = update_z(s, beta, b, Omega);

        // update m
        m = update_m(C, z, true);

        // update b
        b = update_b(beta, Omega, z, m, xi, Dinv);

        // update Omega
        Omega = update_Omega(beta, b, z, m, nu, Theta);
      }

      // update beta
      for(int n = 0; n<N; n++){
        for(int c = 0; c<C; c++){
          if(z[n]==c+1){
            Omega_c_inv = arma::inv(reshape(Omega(span::all,c),P_r,P_r));
            b_c = b(span::all,c);
          }
        }
        mat XSigX = reshape(as<mat>(XkX[n])*reshape(Sigmainv,Jm1*Jm1,1),P_r,P_r);
        vec XSigU = zeros<vec>(P_r);
        for(int t = 0; t<Tvec[n]; t++){
          ind = csTvec[n]+t;
          if(P_f==0)
            XSigU += trans(as<mat>(X[ind]))*Sigmainv*U(span::all,ind);
          if(P_f>0)
            XSigU += trans(as<mat>(X[ind]))*Sigmainv*U(span::all,ind)-trans(as<mat>(X[ind]))*Sigmainv*as<mat>(W[ind])*alpha;
        }
        beta(span::all,n) = update_reg(b_c,Omega_c_inv,XSigX,XSigU);
      }

      // weight-based update of classes
      if(weight_update==true && (r+1)<=B && (r+1)%buffer==0){
        if(print_progress && r==0){
          sprintf(buf, "%9d started weight-based class updating\n", r+1);
          Rcout << buf;
        }
        List class_update = update_classes_wb(Cmax,epsmin,epsmax,distmin,s,b,Omega);
        s = as<vec>(class_update["s"]);
        C = s.size();
        b = as<mat>(class_update["b"]);
        Omega = as<mat>(class_update["Omega"]);
        z = update_z(s, beta, b, Omega);
        m = update_m(C, z, true);
        if(print_progress && r+1==B){
          sprintf(buf, "%9d ended weight-based class updating (C = %d)\n", r+1, C);
          Rcout << buf;
        }
      }

      // dp-based update of classes
      if(dp_update==true && (r+1)<=B){
        if(print_progress && r==0){
          sprintf(buf, "%9d started dp-based class updating\n", r+1);
          Rcout << buf;
        }
        List class_update = update_classes_dp(Cmax,beta,z,b,Omega,delta,xi,D,nu,Theta);
        z = as<vec>(class_update["z"]);
        b = as<mat>(class_update["b"]);
        Omega = as<mat>(class_update["Omega"]);
        s = as<vec>(class_update["s"]);
        C = s.size();
        m = update_m(C, z, true);
      }
    }

    if(P_f>0){
      // update alpha
      mat WSigW = reshape(WkW*reshape(Sigmainv,Jm1*Jm1,1),P_f,P_f);
      vec WSigU = zeros<vec>(P_f);
      for(int n = 0; n<N; n++){
        for(int t = 0; t<Tvec[n]; t++){
          ind = csTvec[n]+t;
          if(P_r==0)
            WSigU += trans(as<mat>(W[ind]))*Sigmainv*U(span::all,ind);
          if(P_r>0)
            WSigU += trans(as<mat>(W[ind]))*Sigmainv*(U(span::all,ind)-as<mat>(X[ind])*beta(span::all,n));
        }
      }
      alpha = update_reg(eta,Psiinv,WSigW,WSigU);
    }

    // update U
    for(int n = 0; n<N; n++){
      for(int t = 0; t<Tvec[n]; t++){
        ind = csTvec[n]+t;
        if(P_f>0 && P_r>0)
          U(span::all,ind) = update_U(U(span::all,ind), y(n,t), as<mat>(W[ind])*alpha+as<mat>(X[ind])*beta(span::all,n), Sigmainv);
        if(P_f>0 && P_r==0)
          U(span::all,ind) = update_U(U(span::all,ind), y(n,t), as<mat>(W[ind])*alpha, Sigmainv);
        if(P_f==0 && P_r>0)
          U(span::all,ind) = update_U(U(span::all,ind), y(n,t), as<mat>(X[ind])*beta(span::all,n), Sigmainv);
        if(P_f==0 && P_r==0){
          arma::vec mu_null(Jm1);
          U(span::all,ind) = update_U(U(span::all,ind), y(n,t), mu_null, Sigmainv);
        }
      }
    }

    // update Sigma
    S = zeros<mat>(Jm1,Jm1);
    for(int n = 0; n<N; n++){
      for(int t = 0; t<Tvec[n]; t++){
        ind = csTvec[n]+t;
        if(P_f>0 && P_r>0)
          eps = U(span::all,ind) - as<mat>(W[ind])*alpha - as<mat>(X[ind])*beta(span::all,n);
        if(P_f>0 && P_r==0)
          eps = U(span::all,ind) - as<mat>(W[ind])*alpha;
        if(P_f==0 && P_r>0)
          eps = U(span::all,ind) - as<mat>(X[ind])*beta(span::all,n);
        if(P_f==0 && P_r==0)
          eps = U(span::all,ind);
        S += eps * trans(eps);
      }
    }
    arma::mat Sigma = update_Sigma(kappa, E, sum(Tvec), S);
    Sigmainv = arma::inv(Sigma);

    // save draws
    if(P_f>0)
      alpha_draws(r,span::all) = trans(alpha);
    if(P_r>0){
      s_draws(r,span(0,s.size()-1)) = trans(s);
      vec vectorise_b = vectorise(b);
      b_draws(r,span(0,vectorise_b.size()-1)) = trans(vectorise_b);
      vec vectorise_Omega = vectorise(Omega);
      Omega_draws(r,span(0,vectorise_Omega.size()-1)) =
        trans(vectorise_Omega);
    }
    Sigma_draws(r,span::all) = trans(vectorise(Sigma));
    class_sequence[r] = C;

    // print time to completion
    if(print_progress)
      if((r+1)%nprint==0 && r+1 != R)
        update_timer(r, R);
  }

  if(print_progress)
    end_timer(R);

  // build and return output list 'out'
  List out;
  if(P_f>0 && P_r>0)
    out = List::create(Named("s") = s_draws,
                       Named("alpha") = alpha_draws,
                       Named("b") = b_draws,
                       Named("Omega") = Omega_draws,
                       Named("Sigma") = Sigma_draws,
                       Named("classification") = z,
                       Named("class_sequence") = class_sequence);
  if(P_f>0 && P_r==0)
    out = List::create(Named("alpha") = alpha_draws,
                       Named("Sigma") = Sigma_draws);
  if(P_f==0 && P_r>0)
    out = List::create(Named("s") = s_draws,
                       Named("b") = b_draws,
                       Named("Omega") = Omega_draws,
                       Named("Sigma") = Sigma_draws,
                       Named("classification") = z,
                       Named("class_sequence") = class_sequence);
  if(P_f==0 && P_r==0)
    out = List::create(Named("Sigma") = Sigma_draws);
  return out;
}
