// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
#include <time.h>
using namespace arma;
using namespace Rcpp;

// DRAW FROM TRUNCATED NORMAL
double dexpr(double const& a) {
  // rossi 10/2016
  // Function to draw from a truncated normal using rejection sampling
  double x,e,e1;
  int success;
  success=0;
  while(success == 0){
    e = -log(runif(1)[0]);
    e1 = -log(runif(1)[0]);
    if(pow(e,2) <= 2.0*e1*pow(a,2)){
      x = a + e/a;
      success = 1;
    }
  }
  return(x);
}

double invCdfNorm(double const& a) {
  // rossi 10/2016
  // Function to draw from a normal truncated below using inverse CDF method
  double Phia,unif,arg,z;
  Phia = R::pnorm(a,0.0,1.0,1,0);
  unif = runif(1)[0];
  arg = unif*(1.0-Phia)+Phia;
  z = R::qnorm(arg,0.0,1.0,1,0);
  return(z);
}

double dnr(double const& a) {
  // rossi 10/2016
  // Function to draw from a standard normal truncated below using rejection
  // sampling
  double candz,z;
  int success;
  success=0;
  while(success == 0){
    candz=rnorm(1)[0];
    if(candz >= a){
      z=candz;
      success =1;
    }
  }
  return(z);
}

double trunNormBelow(double const& a){
  // rossi 10/2016
  // Function to draw from standard normal truncated below
  double z;
  if (a > 4){
    // tail sampling using exponential rejection
    z=dexpr(a);
  }
  else {
    if (a <= -4){
      // normal rejection sampling
      z=dnr(a);
    }
    // -4 < a <=4
    z=invCdfNorm(a);
  }
  return(z);
}

double trunNorm(double mu, double sig, double trunpt, bool above){
  // rossi 10/2016
  // Function to draw draw from truncated normal
  double a,z,draw;
  if(!above){
    a=(trunpt-mu)/sig;
    z= trunNormBelow(a);
    draw = sig*z + mu;
  }
  else {
    a=(mu-trunpt)/sig;
    z= trunNormBelow(a);
    draw = -sig*z + mu;
  }
  return(draw);
}

// MULTIVARIATE NORMAL COMPUTATION
double mvnpdf(arma::vec const& x, arma::vec const& mean,
              arma::mat const& Sigma) {
  // oelschlaeger 04/2020
  // Function to compute the PDF of a multivariate normal
  int n = x.size();
  double sqrt2pi = sqrt(2*M_PI);
  mat quadform  = trans(x-mean) * solve(Sigma,eye(n,n)) * (x-mean);
  double norm = pow(sqrt2pi,-n) * pow(det(Sigma),-0.5);
  return(norm * exp(-0.5*quadform(0,0)));
}

//' Draw from Dirichlet
//' @description
//' Function to draw from a Dirichlet distribution.
//' @param alpha
//' A vector, the concentration parameter.
//' @return
//' A vector, the sample from the Dirichlet distribution.
//' @export
//' @keywords
//' utils
//'
// [[Rcpp::export]]
arma::vec rdirichlet(arma::vec alpha) {
  // oelschlaeger 04/2020
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

//' Draw from a Wishart
//' @description
//' Function to draw from Wishart and inverted Wishart distribution.
//' @param nu
//' A double, the degrees of freedom.
//' @param V
//' A matrix, the scale matrix.
//' @return
//' A list, the draw from the Wishart (W), inverted Wishart (IW), and
//' corresponding Cholesky decomposition (C and CI)
//' @export
//' @keywords
//' utils
//'
// [[Rcpp::export]]
List rwishart(double nu, arma::mat const& V){
  // Wayne Taylor 7/2015
  int m = V.n_rows;
  mat T = zeros(m,m);
  for(int i = 0; i < m; i++) {
    T(i,i) = sqrt(rchisq(1,nu-i)[0]);
  }
  for(int j = 0; j < m; j++) {
    for(int i = j+1; i < m; i++) {
      T(i,j) = rnorm(1)[0];
    }}
  mat C = trans(T)*chol(V);
  mat CI = solve(trimatu(C),eye(m,m));
  // W is Wishart draw, IW is W^-1
  return List::create(
    Named("W") = trans(C) * C,
    Named("IW") = CI * trans(CI),
    Named("C") = C,
    Named("CI") = CI);
}
