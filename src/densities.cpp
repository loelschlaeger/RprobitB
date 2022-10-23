// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

//' Compute density of Dirichlet distribution
//'
//' This function computes the density of a Dirichlet distribution.
//'
//' @details
//' This function performs no input checks. See \code{\link{ddirichlet}}
//' for the version with input checks.
//'
//' @param x
//' A \code{numeric}, a weight vector of length \code{n}.
//' Each vector element must be between \code{0} and \code{1}.
//' The sum of the vector elements must be \code{1}.
//' @param concentration
//' A \code{numeric}, the concentration vector of length \code{n}.
//' @param log
//' A \code{logical}, if \code{TRUE} the logarithm of the density value is
//' returned.
//' By default, \code{log = FALSE}.
//'
//' @return
//' A \code{numeric}, the density value.
//'
//' @examples
//' x <- c(0.5,0.3,0.2)
//' concentration <- 1:3
//' RprobitB:::ddirichlet_cpp(x = x, concentration = concentration)
//' RprobitB:::ddirichlet_cpp(x = x, concentration = concentration, log = TRUE)
//'
//' @keywords internal cpp
//'
// [[Rcpp::export]]
double ddirichlet_cpp(
    arma::vec const& x, arma::vec const& concentration, bool log = false
) {
  int n = x.size();
  double P = 1.0;
  for (int j = 0; j < n; ++j) {
    P *= pow(x(j), concentration(j) - 1);
  }
  double norm = 1.0;
  for (int j = 0; j < n; ++j) {
    norm *= std::tgamma(concentration(j));
  }
  norm = norm / std::tgamma(sum(concentration));
  double density = P / norm;
  if(log) return std::log(density);
  return density;
}

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
//' By default, \code{log = FALSE}.
//'
//' @return
//' A \code{numeric}, the density value.
//'
//' @examples
//' x <- c(0,0)
//' mean <- c(0,0)
//' Sigma <- diag(2)
//' RprobitB:::dmvnorm_cpp(x = x, mean = mean, Sigma = Sigma)
//' RprobitB:::dmvnorm_cpp(x = x, mean = mean, Sigma = Sigma, log = TRUE)
//'
//' @keywords internal cpp
//'
// [[Rcpp::export]]
double dmvnorm_cpp(
    arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
    bool log = false
  ) {
  int n = x.size();
  double sqrt2pi = std::sqrt(2.0 * M_PI);
  arma::mat quadform  = trans(x-mean) * solve(Sigma,arma::eye(n,n)) * (x-mean);
  double norm = pow(sqrt2pi,-n) * pow(arma::det(Sigma),-0.5);
  double density = norm * exp(-0.5 * quadform(0,0));
  if(log) return std::log(density);
  return density;
}

//' Compute density of (Inverse-) Wishart distribution
//'
//' This function computes the density of the (Inverse-) Wishart distribution.
//'
//' @param x
//' A \code{matrix}, a covariance matrix of dimension \code{n} x \code{n}.
//' @param df
//' An \code{integer}, the degrees of freedom.
//' Must be greater or equal \code{n}.
//' @param scale
//' A \code{matrix}, the scale matrix of dimension \code{n} x \code{n}.
//' Must be a covariance matrix.
//' @param log
//' A \code{logical}, if \code{TRUE} the logarithm of the density value is
//' returned.
//' By default, \code{log = FALSE}.
//' @param inv
//' A \code{logical}, if \code{TRUE} the density of the Inverse-Wishart
//' distribution is returned.
//' By default, \code{inv = FALSE}.
//'
//' @return
//' A \code{numeric}, the density value.
//'
//' @examples
//' x <- diag(2)
//' df <- 4
//' scale <- diag(2)
//' RprobitB:::dwishart_cpp(x = x, df = df, scale = scale)
//' RprobitB:::dwishart_cpp(x = x, df = df, scale = scale, log = TRUE)
//' RprobitB:::dwishart_cpp(x = x, df = df, scale = scale, inv = TRUE)
//'
//' @export
//' @keywords internal cpp
//'
// [[Rcpp::export]]
double dwishart_cpp(
    arma::mat const& x, int const& df, arma::mat const& scale,
    bool log = false, bool inv = false
) {
  int n = x.n_cols;
  double dfh = df * 0.5;
  double P = 1.0;
  double norm = 1.0;
  double gamma = pow(M_PI, (n * (n - 1) * 0.25));
  for (int j = 1; j < (n + 1); ++j) {
    gamma *= std::tgamma(dfh - ((j - 1.0) * 0.5));
  }
  if (inv) {
    P = pow(arma::det(scale), dfh);
    P *= std::exp(-0.5 * arma::trace(scale * x.i()));
    P *= pow(arma::det(x), (-0.5 * (df + n + 1.0)));
    norm = pow(2.0, (0.5 * (df * n))) * gamma;
  } else {
    P = pow(arma::det(x), ((df - n - 1.0) * 0.5));
    P *= std::exp(-0.5 * arma::trace(scale.i() * x));
    norm = pow(2.0, (dfh * n)) * pow(arma::det(scale), dfh) * gamma;
  }
  double density = P / norm;
  if(log) return std::log(density);
  return density;
}


