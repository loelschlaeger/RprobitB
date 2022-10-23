// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

//' Compute density of multivariate normal distribution
//'
//' This function computes the density of a multivariate normal distribution.
//'
//' @param x
//' A \code{numeric}, a quantile vector of length \code{n}.
//' @param mean
//' A \code{numeric}, the mean vector of length \code{n}.
//' @param Sigma
//' A \code{matrix}, the covariance matrix of dimension \code{n} x \code{n}.
//' @param log
//' A \code{logical}, if \code{TRUE} the logarithm of the density value is
//' returned.
//'
//' @return
//' A \code{numeric}, the density value.
//'
//' @examples
//' x <- c(0,0)
//' mean <- c(0,0)
//' Sigma <- diag(2)
//' dmvnorm(x = x, mean = mean, Sigma = Sigma)
//' dmvnorm(x = x, mean = mean, Sigma = Sigma, log = TRUE)
//'
//' @export
//' @keywords internal utils
//'
// [[Rcpp::export]]
double dmvnorm(
    arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
    bool log = false
  ) {
  int n = x.size();
  double sqrt2pi = std::sqrt(2.0*M_PI);
  arma::mat quadform  = trans(x-mean) * solve(Sigma,arma::eye(n,n)) * (x-mean);
  double norm = pow(sqrt2pi,-n) * pow(det(Sigma),-0.5);
  double density = norm * exp(-0.5*quadform(0,0));
  if(log) return(std::log(density));
  return(density);
}

