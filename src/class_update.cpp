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
//' @description
//' This function computes the euclidean distance between two vectors.
//' @param a
//' A numeric vector.
//' @param b
//' Another numeric vector of the same length as \code{a}.
//' @return
//' The euclidean distance.
//' @export
//' @keywords
//' utils
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

//' Weight-based update of latent classes
//' @description
//' This function updates the latent classes based on their class weights.
//' @param Cmax
//' The maximum number of classes.
//' @param epsmin
//' The threshold weight (between 0 and 1) for removing a class.
//' @param epsmax
//' The threshold weight (between 0 and 1) for splitting a class.
//' @param distmin
//' The (non-negative) threshold difference in class means for joining two classes.
//' @inheritParams RprobitB_parameter
//' @details
//' The updating scheme bases on the following rules:
//' \itemize{
//'   \item We remove class \eqn{c}, if \eqn{s_c<\epsilon_{\text{min}}}, i.e. if the
//'         class weight \eqn{s_c} drops below some threshold \eqn{\epsilon_{\text{min}}}.
//'         This case indicates that class \eqn{c} has a negligible impact on the mixing distribution.
//'   \item We split class \eqn{c} into two classes \eqn{c_1} and \eqn{c_2}, if \eqn{s_c>\epsilon_\text{max}}.
//'         This case indicates that class \eqn{c} has a high influence on the mixing
//'         distribution whose approximation can potentially be improved by
//'         increasing the resolution in directions of high variance.
//'         Therefore, the class means \eqn{b_{c_1}} and \eqn{b_{c_2}} of the new classes
//'         \eqn{c_1} and \eqn{c_2} are shifted in opposite directions from the class mean
//'         \eqn{b_c} of the old class \eqn{c} in the direction of the highest variance.
//'   \item We join two classes \eqn{c_1} and \eqn{c_2} to one class \eqn{c}, if
//'         \eqn{\lVert b_{c_1} - b_{c_2} \rVert<\epsilon_{\text{distmin}}}, i.e. if
//'         the euclidean distance between the class means \eqn{b_{c_1}} and \eqn{b_{c_2}}
//'         drops below some threshold \eqn{\epsilon_{\text{distmin}}}. This case indicates
//'         location redundancy which should be repealed. The parameters of \eqn{c}
//'         are assigned by adding the values of \eqn{s} from \eqn{c_1} and \eqn{c_2} and
//'         averaging the values for \eqn{b} and \eqn{\Omega}.
//' }
//' The rules are executed in the above order, but only one rule per iteration
//' and only if \code{Cmax} is not exceeded.
//' @examples
//' ### parameter settings
//' s <- c(0.8,0.2)
//' b <- matrix(c(1,1,1,-1), ncol=2)
//' Omega <- matrix(c(0.5,0.3,0.3,0.5,1,-0.1,-0.1,0.8), ncol=2)
//'
//' ### Remove class 2
//' RprobitB:::update_classes_wb(Cmax = 10, epsmin = 0.3, epsmax = 0.9, distmin = 1,
//'                              s = s, b = b, Omega = Omega)
//'
//' ### Split class 1
//' RprobitB:::update_classes_wb(Cmax = 10, epsmin = 0.1, epsmax = 0.7, distmin = 1,
//'                              s = s, b = b, Omega = Omega)
//'
//' ### Join classes
//' RprobitB:::update_classes_wb(Cmax = 10, epsmin = 0.1, epsmax = 0.9, distmin = 3,
//'                              s = s, b = b, Omega = Omega)
//' @return
//' A list of updated values for \code{s}, \code{b}, and \code{Omega}.
//' @keywords
//' internal
//'
// [[Rcpp::export]]
Rcpp::List update_classes_wb (int Cmax, double epsmin, double epsmax, double distmin,
                              arma::vec s, arma::mat b, arma::mat Omega) {
  bool flag = false;
  int C = b.n_cols;
  int P = b.n_rows;
  mat stack = join_cols(trans(s),join_cols(b,Omega));

  // remove class (if smallest class weight < epsmin and C > 1)
  int id_min = index_min(stack(0,span::all));
  if(C > 1 && stack(0,id_min) < epsmin){
    C -= 1;
    double share_weight = stack(0,id_min) / C;
    stack.shed_col(id_min);
    stack(0,span::all) += share_weight;
    flag = true;
  }

  // split class (if no class removed, largest class weight > epsmax, and C < Cmax)
  if(flag==false){
    int id_max = index_max(stack(0,span::all));
    if(C < Cmax && stack(0,id_max)>epsmax){
      mat max_class_Omega = reshape(stack(span(P+1,P+1+P*P-1),id_max),P,P);

      // find largest variance (largest_var[0] is value, largest_var[1] is index)
      vec largest_var = zeros<vec>(2);
      for(int p=0; p<P; p++){
        if(max_class_Omega(p,p)>largest_var[0]){
          largest_var[0] = max_class_Omega(p,p);
          largest_var[1] = p;
        }
      }

      // insert new class
      stack.insert_cols(id_max,stack(span::all,id_max));
      stack(0,span(id_max,id_max+1)) /= 2;
      stack(1+largest_var[1],id_max) -= largest_var[0]/2;
      stack(1+largest_var[1],id_max+1) += largest_var[0]/2;
      stack(span(P+1,P+1+P*P-1),span(id_max,id_max+1)) /= 2;
      C += 1;
      flag = true;
    }
  }

  //join classes (if no class splitted or removed and C > 1)
  if(flag==false && C > 1){
    vec closest_classes = zeros<vec>(3);
    closest_classes(0) = std::numeric_limits<int>::max();
    for(int c1=0; c1<C; c1++){
      for(int c2=0; c2<c1; c2++){
        if(euc_dist(stack(span(1,1+P-1),c1),stack(span(1,1+P-1),c2)) < closest_classes(0)) {
          closest_classes(0) = euc_dist(stack(span(1,1+P-1),c1),stack(span(1,1+P-1),c2));
          closest_classes(1) = c1;
          closest_classes(2) = c2;
        }
      }
    }
    if(closest_classes(0)<distmin){
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
  rowvec s_update = stack.row(0);
  mat b_update = stack.rows(span(1,1+P-1));
  mat Omega_update = stack.rows(span(P+1,1+P+P*P-1));
  return List::create(Named("s") = s_update,
                      Named("b") = b_update,
                      Named("Omega") = Omega_update);
}

//' Dirichlet process-based update of latent classes
//' @description
//' This function updates the latent classes based on a Dirichlet process.
//' @details
//' To be added.
//' @param Cmax
//' The maximum number of classes.
//' @inheritParams RprobitB_parameter
//' @inheritParams check_prior
//' @param s_desc
//' If \code{TRUE}, sort the classes in descending class weight.
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
//' RprobitB:::update_classes_dp(Cmax = 10, beta = beta, z = z, b = b, Omega = Omega,
//'                              delta = delta, xi = xi, D = D, nu = nu, Theta = Theta)
//' @return
//' A list of updated values for \code{z}, \code{b}, \code{Omega}, and \code{s}.
//' @keywords
//' internal
//'
// [[Rcpp::export]]
Rcpp::List update_classes_dp (int Cmax, arma::mat beta, arma::vec z, arma::mat b, arma::mat Omega,
                              double delta, arma::vec xi, arma::mat D, int nu, arma::mat Theta,
                              bool s_desc = true) {

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
  m_full(span(0,C-1)) = update_m(C, z);

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
                      Named("s") = s_full(span(0,C-1))));
}
