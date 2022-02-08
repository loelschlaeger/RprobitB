// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
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

//' @examples
//' rep <- 1
//' Cmax <- 10
//' epsmin <- 0.1
//' epsmax <- 0.9
//' distmin <- 0.1
//' s <- 0.92
//' m <- 94
//' b <- matrix(1:3,ncol=1)
//' Omega <- matrix(1:9,ncol=1)
//' update_classes(rep = rep, Cmax = Cmax, epsmin = epsmin, epsmax = epsmax, distmin = distmin, s = s, m = m, b = b, Omega = Omega, print_progress = TRUE)
//'
//' @export
//' @keywords
//' internal
//'
// [[Rcpp::export]]
Rcpp::List update_classes (int rep, int Cmax,
                           double epsmin, double epsmax, double distmin,
                           arma::vec s, arma::vec m, arma::mat b, arma::mat Omega,
                           bool print_progress) {
  char buf[50];
  bool flag = false;
  int C = b.n_cols;
  int P = b.n_rows;
  mat stack = join_cols(trans(s),join_cols(trans(m),join_cols(b,Omega)));

  // remove class (if smallest class weight < epsmin and C > 1)
  int id_min = index_min(stack(0,span::all));
  if(C > 1 && stack(0,id_min)<epsmin){
    stack.shed_col(id_min);
    C -= 1;
    if(print_progress){
      sprintf(buf, "%9d removed class         \n", rep+1);
      Rcout <<  buf;
    }
    flag = true;
  }

  // split class (if no class removed, largest class weight > epsmax, and C < Cmax)
  if(flag==false){
    int id_max = index_max(stack(0,span::all));
    if(C<Cmax && stack(0,id_max)>epsmax){
      mat max_class_Omega = reshape(stack(span(P+2,P+2+P*P-1),id_max),P,P);

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
      stack(span(0,1),span(id_max,id_max+1)) /= 2;
      stack(2+largest_var[1],id_max) -= largest_var[0]/2;
      stack(2+largest_var[1],id_max+1) += largest_var[0]/2;
      stack(span(P+2,P+2+P*P-1),span(id_max,id_max+1)) /= 2;
      C += 1;
      if(print_progress){
        sprintf(buf, "%9d splitted class        \n", rep+1);
        Rcout << buf;
      }
      flag = true;
    }
  }

  //join classes (if no class splitted or removed and C > 1)
  if(flag==false && C > 1){
    vec closest_classes = zeros<vec>(3);
    closest_classes(0) = std::numeric_limits<int>::max();
    for(int c1=0; c1<C; c1++){
      for(int c2=0; c2<c1; c2++){
        if(euc_dist(stack(span(2,2+P-1),c1),stack(span(2,2+P-1),c2)) < closest_classes(0)) {
          closest_classes(0) = euc_dist(stack(span(2,2+P-1),c1),stack(span(2,2+P-1),c2));
          closest_classes(1) = c1;
          closest_classes(2) = c2;
        }
      }
    }
    if(closest_classes(0)<distmin){
      int c1 = closest_classes(1);
      int c2 = closest_classes(2);
      stack(span(0,1),c1) += stack(span(0,1),c2);
      stack(span(2,2+P-1),c1) += stack(span(2,2+P-1),c2);
      stack(span(2,2+P-1),c1) /=2;
      stack(span(2+P,2+P+P*P-1),c1) += stack(span(2+P,2+P+P*P-1),c2);
      stack(span(2+P,2+P+P*P-1),c1) /=2;
      stack.shed_col(c2);
      C -= 1;
      if(print_progress){
        sprintf(buf, "%9d joined classes        \n", rep+1);
        Rcout <<  buf;
      }
      flag = true;
    }
  }

  //sort s ascending
  stack = stack.cols(sort_index(stack(0,span::all),"descend"));
  rowvec s_update = stack.row(0);
  rowvec m_update = stack.row(1);
  mat b_update = stack.rows(span(2,P+1));
  mat Omega_update = stack.rows(span(P+2,1+P+P*P));
  return List::create(Named("C") = C,
                      Named("s") = s_update,
                      Named("m") = m_update,
                      Named("b") = b_update,
                      Named("Omega") = Omega_update,
                      Named("flag") = flag);
}
