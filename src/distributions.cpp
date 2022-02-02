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
//' A density value.
//' @export
//' @keywords
//' dist
//'
// [[Rcpp::export]]
double dmvnorm(arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma, bool log = false) {
  int n = x.size();
  double sqrt2pi = std::sqrt(2.0*M_PI);
  mat quadform  = trans(x-mean) * solve(Sigma,eye(n,n)) * (x-mean);
  double norm = pow(sqrt2pi,-n) * pow(det(Sigma),-0.5);
  double out = norm * exp(-0.5*quadform(0,0));
  if(log){
    out = std::log(out);
  }
  return(out);
}

//' Draw from Dirichlet distribution
//' @description
//' Function to draw from a Dirichlet distribution.
//' @param alpha
//' A vector, the concentration parameter.
//' @return
//' A vector, the sample from the Dirichlet distribution.
//' @export
//' @keywords
//' dist
//'
// [[Rcpp::export]]
arma::vec rdirichlet(arma::vec alpha) {
  int n = alpha.n_elem;
  arma::vec draw = zeros(n);
  double sum_term = 0;
  for (int j = 0; j < n; ++j) {
    double cur = R::rgamma(alpha[j],1.0);
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
//' @param nu
//' A double, the degrees of freedom.
//' @param V
//' A matrix, the scale matrix.
//' @return
//' A list, the draws from the Wishart (W), inverted Wishart (IW), and
//' corresponding Cholesky decompositions (C and CI).
//' @export
//' @keywords
//' dist
//'
// [[Rcpp::export]]
List rwishart(double nu, arma::mat const& V){
  int m = V.n_rows;
  mat T = zeros(m,m);
  for(int i = 0; i < m; i++) {
    T(i,i) = std::sqrt(R::rchisq(nu-i));
  }
  for(int j = 0; j < m; j++) {
    for(int i = j+1; i < m; i++) {
      T(i,j) = rnorm(1)[0];
    }}
  mat C = trans(T)*chol(V);
  mat CI = solve(trimatu(C),eye(m,m));
  return List::create(
    Named("W") = trans(C) * C,
    Named("IW") = CI * trans(CI),
    Named("C") = C,
    Named("CI") = CI);
}

//' Draw from multivariate normal distribution
//' @description
//' This function draws from a multivariate normal distribution.
//' @param mu
//' The mean vector of length \code{n}.
//' @param Sigma
//' The covariance matrix of dimension \code{n} x \code{n}.
//' @return
//' A numeric vector of length \code{n}.
//' @export
//' @keywords
//' dist
//'
// [[Rcpp::export]]
arma::vec rmvnorm(arma::vec mu, arma::mat const& Sigma){
  int n = mu.size();
  arma::mat L = trans(chol(Sigma));
  arma::vec eps = Rcpp::rnorm(n,0.0,1.0);
  return(L * eps + mu);
}
