#include <Rcpp.h>

// Functions to draw from a truncated univariate normal distribution,
// based on implementations in the bayesm 3.1-4 R package by Peter Rossi.

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

//' Draw from truncated normal
//' @description
//' This function draws from a truncated univariate normal distribution.
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
//' ### draw R samples from a standard normal truncated at 1 from above
//' R <- 1e4
//' draws <- replicate(R, rtnorm(1,1,1,TRUE))
//' ### draw the density
//' plot(density(draws))
//' @keywords
//' distribution
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
