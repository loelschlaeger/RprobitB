// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
using namespace arma;
using namespace Rcpp;

// Functions to draw from a truncated univariate normal distribution,
// based on implementations in {bayesm} 3.1-4 by Peter Rossi.

// Function to draw from standard normal truncated below via rejection sampling
double dexpr(double const& a) {
  double x,e,e1;
  int success;
  success = 0;
  while(success == 0){
    e = -std::log(R::runif(0.0,1.0));
    e1 = -std::log(R::runif(0.0,1.0));
    if(pow(e,2.0f) <= 2.0*e1*pow(a,2.0f)){
      x = a + e/a;
      success = 1;
    }
  }
  return(x);
}

// Function to draw from standard normal truncated below via inverse CDF method
double invCdfNorm(double const& a) {
  double Phia,unif,arg,z;
  Phia = R::pnorm(a,0.0,1.0,1,0);
  unif = R::runif(0.0,1.0);
  arg = unif*(1.0-Phia)+Phia;
  z = R::qnorm(arg,0.0,1.0,1,0);
  return(z);
}

// Function to draw from standard normal truncated below via rejection sampling
double dnr(double const& a) {
  double candz,z;
  int success;
  success = 0;
  while(success == 0){
    candz = R::rnorm(0.0,1.0);
    if(candz >= a){
      z = candz;
      success = 1;
    }
  }
  return(z);
}

// Function to draw from standard normal truncated below
double trunNormBelow(double const& a){
  double z;
  if (a > 4){
    z = dexpr(a);
  }
  else {
    if (a <= -4){
      z = dnr(a);
    }
    z = invCdfNorm(a);
  }
  return(z);
}

//' Draw from one-sided truncated normal
//' @description
//' This function draws from a one-sided truncated univariate normal
//' distribution.
//' @param mu
//' The mean.
//' @param sig
//' The standard deviation.
//' @param trunpt
//' The truncation point.
//' @param above
//' A boolean, if \code{TRUE} truncate from above, otherwise from below.
//' @return
//' A numeric value.
//' @export
//' @examples
//' ### samples from a standard normal truncated at 1 from above
//' R <- 1e4
//' draws <- replicate(R, rtnorm(0,1,1,TRUE))
//' plot(density(draws))
//' @keywords
//' internal distribution
//'
// [[Rcpp::export]]
double rtnorm(double mu, double sig, double trunpt, bool above){
  double a,z,draw;
  if(!above){
    a = (trunpt-mu)/sig;
    z = trunNormBelow(a);
    draw = sig*z + mu;
  }
  else {
    a = (mu-trunpt)/sig;
    z = trunNormBelow(a);
    draw = -sig*z + mu;
  }
  return(draw);
}

//' Draw from two-sided truncated normal
//' @description
//' This function draws from a two-sided truncated univariate normal
//' distribution.
//' @param mu
//' The mean.
//' @param sig
//' The standard deviation.
//' @param lower_bound
//' The lower truncation point.
//' @param upper_bound
//' The upper truncation point.
//' @return
//' A numeric value.
//' @export
//' @examples
//' ### samples from a standard normal truncated at -2 and 2
//' R <- 1e4
//' draws <- replicate(R, rttnorm(0,1,-2,2))
//' plot(density(draws))
//' @keywords
//' internal distribution
//'
// [[Rcpp::export]]
double rttnorm(double mu, double sig, double lower_bound, double upper_bound){
  double a = R::pnorm((lower_bound-mu)/sig,0,1,1,0);
  double b = R::pnorm((upper_bound-mu)/sig,0,1,1,0);
  return mu + sig * R::qnorm(R::runif(0.0,1.0)*(b-a)+a,0,1,1,0);
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
//' internal distribution
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
//' internal distribution
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
//' internal distribution
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
