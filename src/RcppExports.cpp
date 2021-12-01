// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rdirichlet
arma::vec rdirichlet(arma::vec alpha);
RcppExport SEXP _RprobitB_rdirichlet(SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(rdirichlet(alpha));
    return rcpp_result_gen;
END_RCPP
}
// rwishart
List rwishart(double nu, arma::mat const& V);
RcppExport SEXP _RprobitB_rwishart(SEXP nuSEXP, SEXP VSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type V(VSEXP);
    rcpp_result_gen = Rcpp::wrap(rwishart(nu, V));
    return rcpp_result_gen;
END_RCPP
}
// gibbs_sampling
List gibbs_sampling(int R, int B, bool print_progress, int N, int J, int P_f, int P_r, List latent_classes, List sufficient_statistics, List prior, List init);
RcppExport SEXP _RprobitB_gibbs_sampling(SEXP RSEXP, SEXP BSEXP, SEXP print_progressSEXP, SEXP NSEXP, SEXP JSEXP, SEXP P_fSEXP, SEXP P_rSEXP, SEXP latent_classesSEXP, SEXP sufficient_statisticsSEXP, SEXP priorSEXP, SEXP initSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type R(RSEXP);
    Rcpp::traits::input_parameter< int >::type B(BSEXP);
    Rcpp::traits::input_parameter< bool >::type print_progress(print_progressSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type J(JSEXP);
    Rcpp::traits::input_parameter< int >::type P_f(P_fSEXP);
    Rcpp::traits::input_parameter< int >::type P_r(P_rSEXP);
    Rcpp::traits::input_parameter< List >::type latent_classes(latent_classesSEXP);
    Rcpp::traits::input_parameter< List >::type sufficient_statistics(sufficient_statisticsSEXP);
    Rcpp::traits::input_parameter< List >::type prior(priorSEXP);
    Rcpp::traits::input_parameter< List >::type init(initSEXP);
    rcpp_result_gen = Rcpp::wrap(gibbs_sampling(R, B, print_progress, N, J, P_f, P_r, latent_classes, sufficient_statistics, prior, init));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RprobitB_rdirichlet", (DL_FUNC) &_RprobitB_rdirichlet, 1},
    {"_RprobitB_rwishart", (DL_FUNC) &_RprobitB_rwishart, 2},
    {"_RprobitB_gibbs_sampling", (DL_FUNC) &_RprobitB_gibbs_sampling, 11},
    {NULL, NULL, 0}
};

RcppExport void R_init_RprobitB(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
