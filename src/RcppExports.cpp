// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// euc_dist
double euc_dist(arma::vec a, arma::vec b);
RcppExport SEXP _RprobitB_euc_dist(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type a(aSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(euc_dist(a, b));
    return rcpp_result_gen;
END_RCPP
}
// update_classes_wb
Rcpp::List update_classes_wb(int Cmax, double epsmin, double epsmax, double distmin, arma::vec s, arma::mat b, arma::mat Omega);
RcppExport SEXP _RprobitB_update_classes_wb(SEXP CmaxSEXP, SEXP epsminSEXP, SEXP epsmaxSEXP, SEXP distminSEXP, SEXP sSEXP, SEXP bSEXP, SEXP OmegaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type Cmax(CmaxSEXP);
    Rcpp::traits::input_parameter< double >::type epsmin(epsminSEXP);
    Rcpp::traits::input_parameter< double >::type epsmax(epsmaxSEXP);
    Rcpp::traits::input_parameter< double >::type distmin(distminSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type s(sSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type b(bSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Omega(OmegaSEXP);
    rcpp_result_gen = Rcpp::wrap(update_classes_wb(Cmax, epsmin, epsmax, distmin, s, b, Omega));
    return rcpp_result_gen;
END_RCPP
}
// update_classes_dp
Rcpp::List update_classes_dp(int Cmax, arma::mat beta, arma::vec z, arma::mat b, arma::mat Omega, double delta, arma::vec xi, arma::mat D, int nu, arma::mat Theta, bool s_desc);
RcppExport SEXP _RprobitB_update_classes_dp(SEXP CmaxSEXP, SEXP betaSEXP, SEXP zSEXP, SEXP bSEXP, SEXP OmegaSEXP, SEXP deltaSEXP, SEXP xiSEXP, SEXP DSEXP, SEXP nuSEXP, SEXP ThetaSEXP, SEXP s_descSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type Cmax(CmaxSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type z(zSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type b(bSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Omega(OmegaSEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type D(DSEXP);
    Rcpp::traits::input_parameter< int >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Theta(ThetaSEXP);
    Rcpp::traits::input_parameter< bool >::type s_desc(s_descSEXP);
    rcpp_result_gen = Rcpp::wrap(update_classes_dp(Cmax, beta, z, b, Omega, delta, xi, D, nu, Theta, s_desc));
    return rcpp_result_gen;
END_RCPP
}
// dmvnorm
double dmvnorm(arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma, bool log);
RcppExport SEXP _RprobitB_dmvnorm(SEXP xSEXP, SEXP meanSEXP, SEXP SigmaSEXP, SEXP logSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec const& >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec const& >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type Sigma(SigmaSEXP);
    Rcpp::traits::input_parameter< bool >::type log(logSEXP);
    rcpp_result_gen = Rcpp::wrap(dmvnorm(x, mean, Sigma, log));
    return rcpp_result_gen;
END_RCPP
}
// rmvnorm
arma::vec rmvnorm(arma::vec mu, arma::mat const& Sigma);
RcppExport SEXP _RprobitB_rmvnorm(SEXP muSEXP, SEXP SigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< arma::mat const& >::type Sigma(SigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(rmvnorm(mu, Sigma));
    return rcpp_result_gen;
END_RCPP
}
// rdirichlet
arma::vec rdirichlet(arma::vec delta);
RcppExport SEXP _RprobitB_rdirichlet(SEXP deltaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type delta(deltaSEXP);
    rcpp_result_gen = Rcpp::wrap(rdirichlet(delta));
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
// update_s
arma::vec update_s(int delta, arma::vec m);
RcppExport SEXP _RprobitB_update_s(SEXP deltaSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(update_s(delta, m));
    return rcpp_result_gen;
END_RCPP
}
// update_z
arma::vec update_z(arma::vec s, arma::mat beta, arma::mat b, arma::mat Omega);
RcppExport SEXP _RprobitB_update_z(SEXP sSEXP, SEXP betaSEXP, SEXP bSEXP, SEXP OmegaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type s(sSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type b(bSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Omega(OmegaSEXP);
    rcpp_result_gen = Rcpp::wrap(update_z(s, beta, b, Omega));
    return rcpp_result_gen;
END_RCPP
}
// update_m
arma::vec update_m(int C, arma::vec z, bool nozero);
RcppExport SEXP _RprobitB_update_m(SEXP CSEXP, SEXP zSEXP, SEXP nozeroSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type C(CSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type z(zSEXP);
    Rcpp::traits::input_parameter< bool >::type nozero(nozeroSEXP);
    rcpp_result_gen = Rcpp::wrap(update_m(C, z, nozero));
    return rcpp_result_gen;
END_RCPP
}
// update_b
arma::mat update_b(arma::mat beta, arma::mat Omega, arma::vec z, arma::vec m, arma::vec xi, arma::mat Dinv);
RcppExport SEXP _RprobitB_update_b(SEXP betaSEXP, SEXP OmegaSEXP, SEXP zSEXP, SEXP mSEXP, SEXP xiSEXP, SEXP DinvSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Omega(OmegaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type z(zSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type m(mSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Dinv(DinvSEXP);
    rcpp_result_gen = Rcpp::wrap(update_b(beta, Omega, z, m, xi, Dinv));
    return rcpp_result_gen;
END_RCPP
}
// update_Omega
arma::mat update_Omega(arma::mat beta, arma::mat b, arma::vec z, arma::vec m, int nu, arma::mat Theta);
RcppExport SEXP _RprobitB_update_Omega(SEXP betaSEXP, SEXP bSEXP, SEXP zSEXP, SEXP mSEXP, SEXP nuSEXP, SEXP ThetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type b(bSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type z(zSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Theta(ThetaSEXP);
    rcpp_result_gen = Rcpp::wrap(update_Omega(beta, b, z, m, nu, Theta));
    return rcpp_result_gen;
END_RCPP
}
// update_reg
arma::vec update_reg(arma::vec mu0, arma::mat Tau0, arma::mat XSigX, arma::vec XSigU);
RcppExport SEXP _RprobitB_update_reg(SEXP mu0SEXP, SEXP Tau0SEXP, SEXP XSigXSEXP, SEXP XSigUSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type mu0(mu0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Tau0(Tau0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type XSigX(XSigXSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type XSigU(XSigUSEXP);
    rcpp_result_gen = Rcpp::wrap(update_reg(mu0, Tau0, XSigX, XSigU));
    return rcpp_result_gen;
END_RCPP
}
// update_Sigma
arma::mat update_Sigma(int kappa, arma::mat E, int N, arma::mat S);
RcppExport SEXP _RprobitB_update_Sigma(SEXP kappaSEXP, SEXP ESEXP, SEXP NSEXP, SEXP SSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type kappa(kappaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type E(ESEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type S(SSEXP);
    rcpp_result_gen = Rcpp::wrap(update_Sigma(kappa, E, N, S));
    return rcpp_result_gen;
END_RCPP
}
// update_U
arma::vec update_U(arma::vec U, int y, arma::vec sys, arma::mat Sigmainv);
RcppExport SEXP _RprobitB_update_U(SEXP USEXP, SEXP ySEXP, SEXP sysSEXP, SEXP SigmainvSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type U(USEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::vec >::type sys(sysSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Sigmainv(SigmainvSEXP);
    rcpp_result_gen = Rcpp::wrap(update_U(U, y, sys, Sigmainv));
    return rcpp_result_gen;
END_RCPP
}
// update_U_ranked
arma::vec update_U_ranked(arma::vec U, arma::vec sys, arma::mat Sigmainv);
RcppExport SEXP _RprobitB_update_U_ranked(SEXP USEXP, SEXP sysSEXP, SEXP SigmainvSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type U(USEXP);
    Rcpp::traits::input_parameter< arma::vec >::type sys(sysSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Sigmainv(SigmainvSEXP);
    rcpp_result_gen = Rcpp::wrap(update_U_ranked(U, sys, Sigmainv));
    return rcpp_result_gen;
END_RCPP
}
// d_to_gamma
arma::vec d_to_gamma(arma::vec d);
RcppExport SEXP _RprobitB_d_to_gamma(SEXP dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type d(dSEXP);
    rcpp_result_gen = Rcpp::wrap(d_to_gamma(d));
    return rcpp_result_gen;
END_RCPP
}
// ll_ordered
double ll_ordered(arma::vec d, arma::mat y, arma::mat mu, arma::vec Tvec);
RcppExport SEXP _RprobitB_ll_ordered(SEXP dSEXP, SEXP ySEXP, SEXP muSEXP, SEXP TvecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type d(dSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type mu(muSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Tvec(TvecSEXP);
    rcpp_result_gen = Rcpp::wrap(ll_ordered(d, y, mu, Tvec));
    return rcpp_result_gen;
END_RCPP
}
// update_d
List update_d(arma::vec d, arma::mat y, arma::mat mu, double ll, arma::vec zeta, arma::mat Z, arma::vec Tvec);
RcppExport SEXP _RprobitB_update_d(SEXP dSEXP, SEXP ySEXP, SEXP muSEXP, SEXP llSEXP, SEXP zetaSEXP, SEXP ZSEXP, SEXP TvecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type d(dSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type ll(llSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type zeta(zetaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Tvec(TvecSEXP);
    rcpp_result_gen = Rcpp::wrap(update_d(d, y, mu, ll, zeta, Z, Tvec));
    return rcpp_result_gen;
END_RCPP
}
// gibbs_sampling
List gibbs_sampling(List sufficient_statistics, List prior, List latent_classes, Rcpp::List fixed_parameter, List init, int R, int B, bool print_progress, bool ordered, bool ranked);
RcppExport SEXP _RprobitB_gibbs_sampling(SEXP sufficient_statisticsSEXP, SEXP priorSEXP, SEXP latent_classesSEXP, SEXP fixed_parameterSEXP, SEXP initSEXP, SEXP RSEXP, SEXP BSEXP, SEXP print_progressSEXP, SEXP orderedSEXP, SEXP rankedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type sufficient_statistics(sufficient_statisticsSEXP);
    Rcpp::traits::input_parameter< List >::type prior(priorSEXP);
    Rcpp::traits::input_parameter< List >::type latent_classes(latent_classesSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type fixed_parameter(fixed_parameterSEXP);
    Rcpp::traits::input_parameter< List >::type init(initSEXP);
    Rcpp::traits::input_parameter< int >::type R(RSEXP);
    Rcpp::traits::input_parameter< int >::type B(BSEXP);
    Rcpp::traits::input_parameter< bool >::type print_progress(print_progressSEXP);
    Rcpp::traits::input_parameter< bool >::type ordered(orderedSEXP);
    Rcpp::traits::input_parameter< bool >::type ranked(rankedSEXP);
    rcpp_result_gen = Rcpp::wrap(gibbs_sampling(sufficient_statistics, prior, latent_classes, fixed_parameter, init, R, B, print_progress, ordered, ranked));
    return rcpp_result_gen;
END_RCPP
}
// split_data
Rcpp::List split_data(Rcpp::List data);
RcppExport SEXP _RprobitB_split_data(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(split_data(data));
    return rcpp_result_gen;
END_RCPP
}
// determine_data_sizes
Rcpp::List determine_data_sizes(Rcpp::List dependent_variable, Rcpp::List independent_variable);
RcppExport SEXP _RprobitB_determine_data_sizes(SEXP dependent_variableSEXP, SEXP independent_variableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type dependent_variable(dependent_variableSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type independent_variable(independent_variableSEXP);
    rcpp_result_gen = Rcpp::wrap(determine_data_sizes(dependent_variable, independent_variable));
    return rcpp_result_gen;
END_RCPP
}
// reference_normalization
Rcpp::List reference_normalization(Rcpp::List independent_variable, int J, int reference_level);
RcppExport SEXP _RprobitB_reference_normalization(SEXP independent_variableSEXP, SEXP JSEXP, SEXP reference_levelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type independent_variable(independent_variableSEXP);
    Rcpp::traits::input_parameter< int >::type J(JSEXP);
    Rcpp::traits::input_parameter< int >::type reference_level(reference_levelSEXP);
    rcpp_result_gen = Rcpp::wrap(reference_normalization(independent_variable, J, reference_level));
    return rcpp_result_gen;
END_RCPP
}
// setup_data
Rcpp::List setup_data(Rcpp::List data, int J, int P_f, int P_r, int reference_level, bool ordered, bool ranked);
RcppExport SEXP _RprobitB_setup_data(SEXP dataSEXP, SEXP JSEXP, SEXP P_fSEXP, SEXP P_rSEXP, SEXP reference_levelSEXP, SEXP orderedSEXP, SEXP rankedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type J(JSEXP);
    Rcpp::traits::input_parameter< int >::type P_f(P_fSEXP);
    Rcpp::traits::input_parameter< int >::type P_r(P_rSEXP);
    Rcpp::traits::input_parameter< int >::type reference_level(reference_levelSEXP);
    Rcpp::traits::input_parameter< bool >::type ordered(orderedSEXP);
    Rcpp::traits::input_parameter< bool >::type ranked(rankedSEXP);
    rcpp_result_gen = Rcpp::wrap(setup_data(data, J, P_f, P_r, reference_level, ordered, ranked));
    return rcpp_result_gen;
END_RCPP
}
// rtnorm
double rtnorm(double mu, double sig, double trunpt, bool above);
RcppExport SEXP _RprobitB_rtnorm(SEXP muSEXP, SEXP sigSEXP, SEXP trunptSEXP, SEXP aboveSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sig(sigSEXP);
    Rcpp::traits::input_parameter< double >::type trunpt(trunptSEXP);
    Rcpp::traits::input_parameter< bool >::type above(aboveSEXP);
    rcpp_result_gen = Rcpp::wrap(rtnorm(mu, sig, trunpt, above));
    return rcpp_result_gen;
END_RCPP
}
// rttnorm
double rttnorm(double mu, double sig, double lower_bound, double upper_bound);
RcppExport SEXP _RprobitB_rttnorm(SEXP muSEXP, SEXP sigSEXP, SEXP lower_boundSEXP, SEXP upper_boundSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sig(sigSEXP);
    Rcpp::traits::input_parameter< double >::type lower_bound(lower_boundSEXP);
    Rcpp::traits::input_parameter< double >::type upper_bound(upper_boundSEXP);
    rcpp_result_gen = Rcpp::wrap(rttnorm(mu, sig, lower_bound, upper_bound));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RprobitB_euc_dist", (DL_FUNC) &_RprobitB_euc_dist, 2},
    {"_RprobitB_update_classes_wb", (DL_FUNC) &_RprobitB_update_classes_wb, 7},
    {"_RprobitB_update_classes_dp", (DL_FUNC) &_RprobitB_update_classes_dp, 11},
    {"_RprobitB_dmvnorm", (DL_FUNC) &_RprobitB_dmvnorm, 4},
    {"_RprobitB_rmvnorm", (DL_FUNC) &_RprobitB_rmvnorm, 2},
    {"_RprobitB_rdirichlet", (DL_FUNC) &_RprobitB_rdirichlet, 1},
    {"_RprobitB_rwishart", (DL_FUNC) &_RprobitB_rwishart, 2},
    {"_RprobitB_update_s", (DL_FUNC) &_RprobitB_update_s, 2},
    {"_RprobitB_update_z", (DL_FUNC) &_RprobitB_update_z, 4},
    {"_RprobitB_update_m", (DL_FUNC) &_RprobitB_update_m, 3},
    {"_RprobitB_update_b", (DL_FUNC) &_RprobitB_update_b, 6},
    {"_RprobitB_update_Omega", (DL_FUNC) &_RprobitB_update_Omega, 6},
    {"_RprobitB_update_reg", (DL_FUNC) &_RprobitB_update_reg, 4},
    {"_RprobitB_update_Sigma", (DL_FUNC) &_RprobitB_update_Sigma, 4},
    {"_RprobitB_update_U", (DL_FUNC) &_RprobitB_update_U, 4},
    {"_RprobitB_update_U_ranked", (DL_FUNC) &_RprobitB_update_U_ranked, 3},
    {"_RprobitB_d_to_gamma", (DL_FUNC) &_RprobitB_d_to_gamma, 1},
    {"_RprobitB_ll_ordered", (DL_FUNC) &_RprobitB_ll_ordered, 4},
    {"_RprobitB_update_d", (DL_FUNC) &_RprobitB_update_d, 7},
    {"_RprobitB_gibbs_sampling", (DL_FUNC) &_RprobitB_gibbs_sampling, 10},
    {"_RprobitB_split_data", (DL_FUNC) &_RprobitB_split_data, 1},
    {"_RprobitB_determine_data_sizes", (DL_FUNC) &_RprobitB_determine_data_sizes, 2},
    {"_RprobitB_reference_normalization", (DL_FUNC) &_RprobitB_reference_normalization, 3},
    {"_RprobitB_setup_data", (DL_FUNC) &_RprobitB_setup_data, 7},
    {"_RprobitB_rtnorm", (DL_FUNC) &_RprobitB_rtnorm, 4},
    {"_RprobitB_rttnorm", (DL_FUNC) &_RprobitB_rttnorm, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_RprobitB(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
