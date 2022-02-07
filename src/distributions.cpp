// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
using namespace arma;
using namespace Rcpp;

//' Density of multivariate normal distribution
//' @description
//' This function computes the density of a multivariate normal distribution.
//' @param x
//' A quantile vector of length \code{n}.
//' @param mean
//' The mean vector of length \code{n}.
//' @param Sigma
//' The covariance matrix of dimension \code{n} x \code{n}.
//' @param log
//' A boolean, if \code{TRUE} the logarithm of the density value is returned.
//' @return
//' The density value.
//' @export
//' @examples
//' x = c(0,0)
//' mean = c(0,0)
//' Sigma = diag(2)
//' dmvnorm(x = x, mean = mean, Sigma = Sigma)
//' dmvnorm(x = x, mean = mean, Sigma = Sigma, log = TRUE)
//' @keywords
//' distribution
//'
// [[Rcpp::export]]
double dmvnorm(arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma, bool log = false) {
  int n = x.size();
  double sqrt2pi = std::sqrt(2.0*M_PI);
  arma::mat quadform  = trans(x-mean) * solve(Sigma,eye(n,n)) * (x-mean);
  double norm = pow(sqrt2pi,-n) * pow(det(Sigma),-0.5);
  double out = norm * exp(-0.5*quadform(0,0));
  if(log){
    out = std::log(out);
  }
  return(out);
}

//' Draw from multivariate normal distribution
//' @description
//' This function draws from a multivariate normal distribution.
//' @details
//' The function builds upon the following fact: If \eqn{\epsilon = (\epsilon_1,\dots,\epsilon_n)},
//' where each \eqn{\epsilon_i} is drawn independently from a standard normal distribution,
//' then \eqn{\mu+L\epsilon} is a draw from the multivariate normal distribution
//' \eqn{N(\mu,\Sigma)}, where \eqn{L} is the lower triangular factor of the
//' Choleski decomposition of \eqn{\Sigma}.
//' @param mu
//' The mean vector of length \code{n}.
//' @param Sigma
//' The covariance matrix of dimension \code{n} x \code{n}.
//' @return
//' A numeric vector of length \code{n}.
//' @export
//' @examples
//' mu <- c(0,0)
//' Sigma <- diag(2)
//' rmvnorm(mu = mu, Sigma = Sigma)
//' @keywords
//' distribution
//'
// [[Rcpp::export]]
arma::vec rmvnorm(arma::vec mu, arma::mat const& Sigma){
  int n = mu.size();
  arma::mat L = trans(chol(Sigma));
  arma::vec eps = Rcpp::rnorm(n,0.0,1.0);
  return(L * eps + mu);
}

//' Draw from Dirichlet distribution
//' @description
//' Function to draw from a Dirichlet distribution.
//' @param delta
//' A vector, the concentration parameter.
//' @return
//' A vector, the sample from the Dirichlet distribution of the same length as \code{delta}.
//' @export
//' @examples
//' rdirichlet(delta = 1:3)
//' @keywords
//' distribution
//'
// [[Rcpp::export]]
arma::vec rdirichlet(arma::vec delta) {
  int n = delta.n_elem;
  arma::vec draw = zeros(n);
  double sum_term = 0;
  for (int j = 0; j < n; ++j) {
    double cur = R::rgamma(delta[j],1.0);
    draw(j) = cur;
    sum_term += cur;
  }
  for (int j = 0; j < n; ++j) {
    draw(j) = draw(j)/sum_term;
  }
  return(draw);
}

//' Draw from Wishart distribution
//' @description
//' This function draws from a Wishart and inverted Wishart distribution.
//' @details
//' The Wishart distribution is a generalization to multiple dimensions of the
//' gamma distributions and draws from the space of covariance matrices.
//' Its expectation is \code{nu*V} and its variance increases both in \code{nu}
//' and in the values of \code{V}.
//' The Wishart distribution is the conjugate prior to the precision matrix of
//' a multivariate normal distribution and proper if \code{nu} is greater than
//' the number of dimensions.
//' @param nu
//' A numeric, the degrees of freedom. Must be at least the number of dimensions.
//' @param V
//' A matrix, the scale matrix.
//' @return
//' A list, the draws from the Wishart (\code{W}), inverted Wishart (\code{IW}), and
//' corresponding Choleski decomposition (\code{C} and \code{CI}).
//' @export
//' @examples
//' rwishart(nu = 2, V = diag(2))
//' @keywords
//' distribution
//'
// [[Rcpp::export]]
List rwishart(double nu, arma::mat const& V){
  int m = V.n_rows;
  arma::mat T = zeros(m,m);
  for(int i = 0; i < m; i++) {
    T(i,i) = std::sqrt(R::rchisq(nu-i));
  }
  for(int j = 0; j < m; j++) {
    for(int i = j+1; i < m; i++) {
      T(i,j) = rnorm(1)[0];
    }
  }
  arma::mat C = trans(T)*chol(V);
  arma::mat CI = solve(trimatu(C),eye(m,m));
  return List::create(Named("W") = trans(C) * C, Named("IW") = CI * trans(CI), Named("C") = C, Named("CI") = CI);
}
