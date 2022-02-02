// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include <stdio.h>
#include <float.h>
#include <Rmath.h>
#include <math.h>
#include <time.h>
#include "timer.h"
#include "distributions.h"
#include "truncatednormal.h"
#include "classupdate.h"
using namespace arma;
using namespace Rcpp;

//' Update class weight vector
//' @description
//' This function updates the class weight vector by drawing from its posterior.
//' @param delta
//' The concentration parameter of length 1 of the Dirichlet prior for \code{s}.
//' @param m
//' The vector of current class sizes.
//' @return
//' A draw from the Dirichlet posterior for \code{s}.
//' @export
//' @keywords
//' posterior
//'
// [[Rcpp::export]]
arma::vec update_s (int delta, arma::vec m) {
  int C = m.size();
  return(rdirichlet(delta*ones(C)+m));
}

//' Update class allocation vector
//' @description
//' This function updates the class allocation vector independently for all observations
//' by drawing from its conditional distribution.
//' @inheritParams RprobitB_parameter
//' @return
//' An updated class allocation vector.
//' @export
//' @keywords
//' posterior
//'
// [[Rcpp::export]]
arma::vec update_z (arma::vec s, arma::mat beta, arma::mat b, arma::mat Omega) {
  Rcpp::Function sample("sample");
  int N = beta.n_cols;
  int C = s.size();
  int P_r = b.n_rows;
  arma::vec z = zeros<vec>(N);
  arma::vec prob_z = zeros<vec>(C);
  for(int n = 0; n<N; n++){
    for(int c = 0; c<C; c++){
      prob_z[c] = s[c]*dmvnorm(beta(span::all,n),b(span::all,c),reshape(Omega(span::all,c),P_r,P_r));
    }
    z[n] = as<int>(sample(seq(0,C-1),1,false,prob_z));
  }
  return(z);
}

// Function to draw from posterior of linear regression
vec draw_reg (mat foo_B1, vec foo_b1, vec b_c, mat Omega_c_inv, int P, int Jm1) {
  mat B1 = arma::inv(Omega_c_inv + foo_B1);
  vec b1 = B1 * (Omega_c_inv * b_c + foo_b1);
  return (b1 + trans(chol(B1)) * vec(rnorm(P)));
}

// Function to compute conditional utility mean and standard deviation
vec cond_utility (vec U, vec mu, mat Sigmainv, int Jm1, int j) {
  vec out(2);
  int jm1 = j-1;
  int ind = Jm1*jm1;
  double tau_j = 1/Sigmainv(ind+jm1);
  double m = 0.0;
  for(int i = 0; i<Jm1; i++){
    if (i!=jm1){
      m += - tau_j*Sigmainv(ind+i)*(U[i]-mu[i]);
    }
  }
  out[0] = mu[jm1]+m;
  out[1] = sqrt(tau_j);
  return (out);
}

// Function to draw the utility vector
vec draw_utility (vec U, vec mu, mat Sigmainv, int Jm1, int y) {
  bool above;
  double bound;
  vec out_U_nt = U;
  vec maxInd(2);
  for(int i = 0; i<Jm1; i++){
    bound = 0.0;
    for(int j = 0; j<Jm1; j++) if(j!=i) {
      maxInd[0] = bound;
      maxInd[1] = out_U_nt[j];
      bound = max(maxInd);
    }
    if (y==(i+1))
      above = false;
    else
      above = true;
    //CMout[1] is mean, CMout[2] is sd
    vec CMout = cond_utility(out_U_nt,mu,Sigmainv,Jm1,i+1);
    out_U_nt[i] = rtnorm(CMout[0],CMout[1],bound,above);
  }
  return (out_U_nt);
}

//' Gibbs sampler.
//' @description
//' This function performs Gibbs sampling for the RprobitB package.
//' @inheritParams mcmc
//' @inheritParams RprobitB_data
//' @param sufficient_statistics
//' The output of \code{\link{sufficient_statistics}}.
//' @param init
//' The output of \code{\link{set_initial_gibbs_values}}.
//' @return
//' A list of Gibbs samples for \code{Sigma}, \code{alpha} (if \code{P_f>0})
//' and \code{s}, \code{b}, \code{Omega} and a vector of classifications
//' (if \code{P_r>0}).
//' @keywords
//' internal
//'
// [[Rcpp::export]]
List gibbs_sampling (int R, int B, bool print_progress,
                     int N, int J, int P_f, int P_r, List latent_classes,
                     List sufficient_statistics, List prior, List init) {

  // extract 'latent_classes' parameters
  int C = as<int>(latent_classes["C"]);
  int Cmax = 10;
  int Cdrawsize;
  int buffer = 50;
  double epsmin = 0.01;
  double epsmax = 0.99;
  double distmin = 0.1;
  bool update = as<bool>(latent_classes["update"]);
  if(update==false){
    Cdrawsize = C;
  }
  else{
    Cmax = as<int>(latent_classes["Cmax"]);
    Cdrawsize = Cmax;
    buffer = as<int>(latent_classes["buffer"]);
    epsmin = as<double>(latent_classes["epsmin"]);
    epsmax = as<double>(latent_classes["epsmax"]);
    distmin = as<double>(latent_classes["distmin"]);
  }

  // extract 'sufficient_statistics' parameters
  vec Tvec = as<vec>(sufficient_statistics["Tvec"]);
  vec csTvec = as<vec>(sufficient_statistics["csTvec"]);
  List W;
  List X;
  mat y = as<mat>(sufficient_statistics["y"]);
  mat WkW;
  List XkX;
  if(P_f>0){
    W = as<List>(sufficient_statistics["W"]);
    WkW = as<mat>(sufficient_statistics["WkW"]);
  }
  if(P_r>0){
    X = as<List>(sufficient_statistics["X"]);
    XkX = as<List>(sufficient_statistics["XkX"]);
  }

  // extract 'prior' parameters
  int delta = 1;
  vec eta;
  mat Psiinv;
  int nu = P_r+2;
  mat Theta;
  vec xi;
  mat Dinv;
  int kappa = as<int>(prior["kappa"]);
  mat E = as<mat>(prior["E"]);
  if(P_f>0){
    eta = as<vec>(prior["eta"]);
    Psiinv = arma::inv(as<mat>(prior["Psi"]));
  }
  if(P_r>0){
    delta = as<int>(prior["delta"]);
    xi = as<vec>(prior["xi"]);
    Dinv = arma::inv(as<mat>(prior["D"]));
    nu = as<int>(prior["nu"]);
    Theta = as<mat>(prior["Theta"]);
  }

  // extract 'init' parameters
  vec m0;
  vec alpha0;
  mat b0;
  mat Omega0;
  mat beta0;
  mat U0 = as<mat>(init["U0"]);
  mat Sigma0inv = arma::inv(as<mat>(init["Sigma0"]));
  if(P_f>0){
    alpha0 = as<vec>(init["alpha0"]);
  }
  if(P_r>0){
    m0 = as<vec>(init["m0"]);
    b0 = as<mat>(init["b0"]);
    Omega0 = as<mat>(init["Omega0"]);
    beta0 = as<mat>(init["beta0"]);
  }

  // define helper variables and functions
  vec s_cand;
  mat Omega_c_inv;
  mat foo_Omega;
  mat Omega_draw;
  vec b_c;
  mat S;
  mat IW;
  vec eps;
  List Wish;
  int ind;
  char buf[50];
  int nprint = round(R/10);
  int Jm1 = J - 1;

  // allocate space for draws
  mat alpha_draws = zeros<mat>(R,P_f);
  mat s_draws = zeros<mat>(R,Cdrawsize);
  mat b_draws = zeros<mat>(R,P_r*Cdrawsize);
  mat Omega_draws = zeros<mat>(R,P_r*P_r*Cdrawsize);
  mat Sigma_draws = zeros<mat>(R,Jm1*Jm1);

  // define updating variables and set initial values
  vec s(C);
  vec z(N);
  vec m = m0;
  mat b_bar(P_r,C);
  mat b = b0;
  mat Omega = Omega0;
  mat U = U0;
  vec alpha = alpha0;
  mat beta = beta0;
  mat Sigmainv = Sigma0inv;

  // start loop
  if(print_progress) start_timer();
  for(int rep = 0; rep<R; rep++) {

    if(P_r>0){
      // draw s, update only if draw is descending
      arma::vec s_cand = update_s(delta,m);
      if(std::is_sorted(std::begin(s_cand),std::end(s_cand),std::greater<double>())){
        s = s_cand;
      }

      // draw z
      z = update_z(s, beta, b, Omega);

      // update m
      m = ones(C);
      for(int c = 0; c<C; c++){
        for(int n = 0; n<N; n++){
          if(z[n]==c) m[c] += 1;
        }
      }

      // update b_bar
      b_bar = zeros<mat>(P_r,C);
      for(int c = 0; c<C; c++){
        for(int n = 0; n<N; n++){
          if(z[n]==c) b_bar(span::all,c) += beta(span::all,n);
        }
        b_bar(span::all,c) /= m[c];
      }

      // draw b
      for(int c = 0; c<C; c++){
        Omega_c_inv = arma::inv(reshape(Omega(span::all,c),P_r,P_r));
        b(span::all,c) = arma::inv(Dinv+m[c]*Omega_c_inv) *
          (Dinv*xi+m[c]*Omega_c_inv*b_bar(span::all,c)) +
          trans(chol(arma::inv(Dinv+m[c]*Omega_c_inv))) * vec(rnorm(P_r));
      }

      // draw Omega
      for(int c = 0; c<C; c++){
        foo_Omega = zeros<mat>(P_r,P_r);
        for(int n = 0; n<N; n++){
          if(z[n]==c)
            foo_Omega += (beta(span::all,n)-b(span::all,c)) *
              trans(beta(span::all,n)-b(span::all,c));
        }
        Omega_draw =
          as<mat>(rwishart(nu+m[c],arma::inv(Theta+foo_Omega))["IW"]);
        Omega(span::all,c) = reshape(Omega_draw,P_r*P_r,1);
      }

      // draw beta
      for(int n = 0; n<N; n++){
        for(int c = 0; c<C; c++){
          if(z[n]==c){
            Omega_c_inv = arma::inv(reshape(Omega(span::all,c),P_r,P_r));
            b_c = b(span::all,c);
          }
        }
        mat foo_B1 =
          reshape(as<mat>(XkX[n])*reshape(Sigmainv,Jm1*Jm1,1),P_r,P_r);
        vec foo_b1 = zeros<vec>(P_r);
        for(int t = 0; t<Tvec[n]; t++){
          ind = csTvec[n]+t;
          if(P_f==0)
            foo_b1 += trans(as<mat>(X[ind]))*Sigmainv*U(span::all,ind);
          if(P_f>0)
            foo_b1 += trans(as<mat>(X[ind]))*Sigmainv*U(span::all,ind)-
              trans(as<mat>(X[ind]))*Sigmainv*as<mat>(W[ind])*alpha;
        }
        beta(span::all,n) = draw_reg(foo_B1,foo_b1,b_c,Omega_c_inv,P_r,Jm1);
      }

      // update classes
      if(update==true && (rep+1)>=(B/2) && (rep+1)<=B && (rep+1)%buffer==0){
        if(print_progress && rep+1==B/2){
          sprintf(buf, "%9d started class updating\n", rep+1);
          Rcout << buf;
        }
        List class_update = update_classes(rep,Cmax,epsmin,epsmax,distmin,s,m,b,
                                           Omega,print_progress);
        C = as<int>(class_update["C"]);
        s = as<vec>(class_update["s"]);
        m = as<vec>(class_update["m"]);
        b = as<mat>(class_update["b"]);
        Omega = as<mat>(class_update["Omega"]);
        if(print_progress && rep+1==B){
          sprintf(buf, "%9d ended class updating (C = %d)\n", rep+1, C);
          Rcout << buf;
        }
      }
    }

    if(P_f>0){
      // draw alpha
      mat foo_A1 = reshape(WkW*reshape(Sigmainv,Jm1*Jm1,1),P_f,P_f);
      vec foo_a1 = zeros<vec>(P_f);
      for(int n = 0; n<N; n++){
        for(int t = 0; t<Tvec[n]; t++){
          ind = csTvec[n]+t;
          if(P_r==0)
            foo_a1 += trans(as<mat>(W[ind]))*Sigmainv*U(span::all,ind);
          if(P_r>0)
            foo_a1 += trans(as<mat>(W[ind]))*Sigmainv*(U(span::all,ind)-
              as<mat>(X[ind])*beta(span::all,n));
        }
      }
      alpha = draw_reg(foo_A1,foo_a1,eta,Psiinv,P_f,Jm1);
    }

    // draw U
    for(int n = 0; n<N; n++){
      for(int t = 0; t<Tvec[n]; t++){
        ind = csTvec[n]+t;
        if(P_f>0 && P_r>0)
          U(span::all,ind) = draw_utility(U(span::all,ind),
            as<mat>(W[ind])*alpha+as<mat>(X[ind])*beta(span::all,n), Sigmainv,
            Jm1, y(n,t));
        if(P_f>0 && P_r==0)
          U(span::all,ind) = draw_utility(U(span::all,ind),
            as<mat>(W[ind])*alpha, Sigmainv, Jm1, y(n,t));
        if(P_f==0 && P_r>0)
          U(span::all,ind) = draw_utility(U(span::all,ind),
            as<mat>(X[ind])*beta(span::all,n), Sigmainv, Jm1, y(n,t));
        if(P_f==0 && P_r==0){
          vec mu_null(Jm1);
          U(span::all,ind) = draw_utility(U(span::all,ind),mu_null, Sigmainv,
            Jm1, y(n,t));
        };
      }
    }

    // draw Sigma
    S = zeros<mat>(Jm1,Jm1);
    for(int n = 0; n<N; n++){
      for(int t = 0; t<Tvec[n]; t++){
        ind = csTvec[n]+t;
        if(P_f>0 && P_r>0)
          eps = U(span::all,ind) - as<mat>(W[ind])*alpha -
            as<mat>(X[ind])*beta(span::all,n);
        if(P_f>0 && P_r==0)
          eps = U(span::all,ind) - as<mat>(W[ind])*alpha;
        if(P_f==0 && P_r>0)
          eps = U(span::all,ind) - as<mat>(X[ind])*beta(span::all,n);
        if(P_f==0 && P_r==0)
          eps = U(span::all,ind);
        S += eps * trans(eps);
      }
    }
    Wish = rwishart(kappa+sum(Tvec),arma::inv(E+S));
    Sigmainv = as<mat>(Wish["W"]);

    // save draws
    if(P_f>0)
      alpha_draws(rep,span::all) = trans(alpha);
    if(P_r>0){
      s_draws(rep,span(0,s.size()-1)) = trans(s);
      vec vectorise_b = vectorise(b);
      b_draws(rep,span(0,vectorise_b.size()-1)) = trans(vectorise_b);
      vec vectorise_Omega = vectorise(Omega);
      Omega_draws(rep,span(0,vectorise_Omega.size()-1)) =
        trans(vectorise_Omega);
    }
    IW = as<mat>(Wish["IW"]);
    Sigma_draws(rep,span::all) = trans(vectorise(IW));

    // print time to completion
    if(print_progress)
      if((rep+1)%nprint==0 && rep+1 != R)
        update_timer(rep, R);
  }

  if(print_progress)
    end_timer(R);

  // build and return output list 'out'
  List out;
  if(P_f>0 && P_r>0)
    out = List::create(Named("s") = s_draws,
                       Named("alpha") = alpha_draws,
                       Named("b") = b_draws,
                       Named("Omega") = Omega_draws,
                       Named("Sigma") = Sigma_draws,
                       Named("classification") = z);
  if(P_f>0 && P_r==0)
    out = List::create(Named("alpha") = alpha_draws,
                       Named("Sigma") = Sigma_draws);
  if(P_f==0 && P_r>0)
    out = List::create(Named("s") = s_draws,
                       Named("b") = b_draws,
                       Named("Omega") = Omega_draws,
                       Named("Sigma") = Sigma_draws,
                       Named("classification") = z);
  if(P_f==0 && P_r==0)
    out = List::create(Named("Sigma") = Sigma_draws);
  return out;
}