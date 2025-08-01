// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
#include "gibbs.h"
using namespace arma;
using namespace Rcpp;

//' Euclidean distance
//'
//' @description
//' This helper function computes the Euclidean distance between two vectors.
//'
//' @param a,b \[`numeric()`\]\cr
//' Numeric vectors of the same length.
//'
//' @return
//' The Euclidean distance.
//'
//' @export
//'
//' @keywords
//' utils
//'
//' @examples
//' euc_dist(c(0, 1), c(1, 0))
//'
// [[Rcpp::export]]
double euc_dist (arma::vec a, arma::vec b){
  int N = a.size();
  double euc_dist = 0;
  for(int n=0; n<N; n++){
    euc_dist += (a(n) - b(n)) * (a(n) - b(n));
  }
  return sqrt(euc_dist);
}

//' Weight-based class updates
//'
//' @description
//' This helper function updates the classes based on weights and locations.
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
//' s <- c(0.8,0.2)
//' b <- matrix(c(1,1,1,-1), ncol=2)
//' Omega <- matrix(c(0.5,0.3,0.3,0.5,1,-0.1,-0.1,0.8), ncol=2)
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
//' ### join classes 1 and 2
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
    arma::vec s, arma::mat b, arma::mat Omega,
    int Cmax = 10
) {

  bool flag = false;
  int C = b.n_cols;
  int P = b.n_rows;
  double deltashift = deltamin;
  mat stack = join_cols(trans(s),join_cols(b,Omega));

  // remove class
  int id_min = index_min(stack(0,span::all));
  if(C > 1 && stack(0,id_min) < epsmin){
    C -= 1;
    stack.shed_col(id_min);
    stack.row(0) = stack.row(0) / arma::accu(stack.row(0));
    flag = true;
  }

  // split class
  if(flag == false && C < Cmax){
    int id_max = index_max(stack(0,span::all));
    if(stack(0,id_max)>epsmax){
      mat max_class_Omega = reshape(stack(span(P+1,P+1+P*P-1),id_max),P,P);
      vec eigval;
      mat eigvec;
      eig_sym(eigval, eigvec, max_class_Omega);
      vec v = eigvec.col(P - 1);
      arma::vec mean_shift = deltashift * std::sqrt(eigval(P - 1)) * v;
      stack.insert_cols(id_max, stack(span::all, id_max));
      stack(0, span(id_max, id_max + 1)) /= 2;
      stack(span(1, 1 + P - 1), id_max) += mean_shift;
      stack(span(1, 1 + P - 1), id_max + 1) -= mean_shift;
      C += 1;
      flag = true;
    }
  }

  //join classes
  if(flag==false && C > 1){
    vec closest_classes = zeros<vec>(3);
    closest_classes(0) = std::numeric_limits<int>::max();
    for(int c1=0; c1<C; c1++){
      for(int c2=0; c2<c1; c2++){
        double dist = euc_dist(stack(span(1,1+P-1),c1),stack(span(1,1+P-1),c2));
        if(dist < closest_classes(0)) {
          closest_classes(0) = dist;
          closest_classes(1) = c1;
          closest_classes(2) = c2;
        }
      }
    }
    if(closest_classes(0)<deltamin){
      int c1 = closest_classes(1);
      int c2 = closest_classes(2);
      stack(0,c1) += stack(0,c2);
      stack(span(1,1+P-1),c1) += stack(span(1,1+P-1),c2);
      stack(span(1,1+P-1),c1) /=2;
      stack(span(1+P,1+P+P*P-1),c1) += stack(span(1+P,1+P+P*P-1),c2);
      stack(span(1+P,1+P+P*P-1),c1) /=2;
      stack.shed_col(c2);
      C -= 1;
      flag = true;
    }
  }

  //sort s ascending
  stack = stack.cols(sort_index(stack(0,span::all),"descend"));

  return List::create(
    Named("s") = stack.row(0),
    Named("b") = stack.rows(span(1,1+P-1)),
    Named("Omega") = stack.rows(span(P+1,1+P+P*P-1))
  );
}

//' Dirichlet process-based class updates
//'
//' @description
//' This helper function updates the classes based on a Dirichlet process.
//'
//' @param s_desc
//' If \code{TRUE}, sort the classes in descending class weight.
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
//' beta <- sapply(z, function(z) rmvnorm(b[,z], matrix(Omega[,z],2,2)))
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
    int Cmax, arma::mat beta, arma::vec z, arma::mat b, arma::mat Omega,
    double delta, arma::vec xi, arma::mat D, int nu, arma::mat Theta,
    bool s_desc = true
) {

  // helper variables and functions
  Rcpp::Function sample("sample");

  // sizes
  int N = z.size();
  int C = b.n_cols;
  int P_r = b.n_rows;

  // space allocation for class characteristics
  arma::mat b_full(P_r,Cmax);
  b_full(span::all,span(0,C-1)) = b;
  arma::mat Omega_full(P_r*P_r,Cmax);
  Omega_full(span::all,span(0,C-1)) = Omega;
  arma::vec m_full(Cmax);
  m_full(span(0,C-1)) = update_m(C, z, true);

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
      b_full(span::all,z[n]-1) = zeros<vec>(P_r);
      Omega_full(span::all,z[n]-1) = zeros<vec>(P_r*P_r);
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
      logp[c] = log(m_full[c]) + dmvnorm(beta(span::all,n), mu_b, sig_b + Omega_c, true);

      // save updates
      b_full(span::all,c) = mu_b;
      Omega_full(span::all,c) = reshape(Omega_c, P_r*P_r, 1);
    }

    // compute log-probability for new class
    arma::vec b_new = xi;
    arma::mat Omega_new = reshape(Omega_full(span::all,span(0,C-1)) * (m_full(span(0,C-1))/sum(m_full(span(0,C-1)))), P_r, P_r);
    logp[C] = log(delta) + dmvnorm(beta(span::all,n), b_new, D + Omega_new, true);

    // transform log-probabilities to probabilities
    arma::vec loc_probs = exp(logp - max(logp));
    loc_probs = loc_probs / sum(loc_probs);

    // draw new class membership (prevent 'Cmax' from exceeding)
    int newz;
    if(C == Cmax){
      newz = as<int>(sample(seq(1,C),1,false,loc_probs(span(0,C-1))));
    } else {
      newz = as<int>(sample(seq(1,C+1),1,false,loc_probs(span(0,C))));
    }
    // spawn new class (but only if 'Cmax' is not exceeded)
    if(newz == C+1) {
      b_full(span::all,C) = b_new;
      Omega_full(span::all,C) = reshape(Omega_new, P_r*P_r, 1);
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
  return(List::create(Named("z") = z,
                      Named("b") = b_full(span::all,span(0,C-1)),
                      Named("Omega") = Omega_full(span::all,span(0,C-1)),
                      Named("s") = s_full(span(0,C-1)),
                      Named("C") = C));
}
