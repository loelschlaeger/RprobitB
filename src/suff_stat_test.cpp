#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//' Split data into dependent and independent variables
//'
//' @inheritParams setup_data
//'
//' @return
//' A \code{list} with two elements:
//' * \code{dependent_variable} is a \code{list} that contains the dependent
//'   variables. It is of length \code{N}, where \code{N} is the number of
//'   individuals. The \code{n}-th element of \code{dependent_variable} is a
//'   \code{vector} of length \code{Tp[n]}, where the \code{t}-th element is an
//'   \code{integer} that refers to the dependent variable from the underlying
//'   choice set.
//' * \code{independent_variable} is a \code{list} that contains the independent
//'   variables. It is of length \code{N}, where \code{N} is the number of
//'   individuals. The \code{n}-th element of \code{independent_variable} is a
//'   \code{list} of length \code{Tp[n]}, where the \code{t}-th element is the
//'   design \code{matrix} for time point \code{t}.
//'
// [[Rcpp::export]]

Rcpp::List split_data(Rcpp::List data) {

  // determine N
  int N = data.size();

  // initialize lists dependent_variable and independent_variable
  Rcpp::List dependent_variable = Rcpp::List(N);
  Rcpp::List independent_variable = Rcpp::List(N);

  // allocate variables
  for (int n = 0; n < N; n++) {

    // extract data for individual
    Rcpp::List data_n = data[n];

    // allocate dependent variables
    Rcpp::IntegerVector dependent_variable_n = data_n["y"];
    dependent_variable[n] = dependent_variable_n;

    // allocate independent variables
    Rcpp::List independent_variable_n = data_n["X"];
    independent_variable[n] = independent_variable_n;
  }

  // return splitted data
  return Rcpp::List::create(
   Rcpp::Named("dependent_variable") = dependent_variable,
   Rcpp::Named("independent_variable") = independent_variable
  );

}

//' Determine data sizes
//'
//' @param dependent_variable
//' A \code{list} that contains the dependent variables.
//' It is of length \code{N}, where \code{N} is the number of individuals.
//' The \code{n}-th element of \code{dependent_variable} is a \code{vector} of
//' length \code{Tp[n]}, where the \code{t}-th element is an \code{integer} that
//' refers to the dependent variable from the underlying choice set.
//'
//' @param independent_variable
//' A \code{list} that contains the independent variables.
//' It is of length \code{N}, where \code{N} is the number of individuals.
//' The \code{n}-th element of \code{independent_variable} is a \code{list} of
//' length \code{Tp[n]}, where the \code{t}-th element is the design
//' \code{matrix} for time point \code{t}.
//'
//' @return
//' A \code{list} with three elements:
//' * \code{N}, the number of individuals
//' * \code{Tp}, the \code{vector} of time points per individual
//' * \code{cs_Tp}, the cumulative sum of \code{Tp}
//'
// [[Rcpp::export]]

Rcpp::List determine_data_sizes(
    Rcpp::List dependent_variable,
    Rcpp::List independent_variable
) {

  // determine N
  int N = dependent_variable.size();
  if (N != independent_variable.size()) {
    Rcpp::stop("N mismatch");
  }

  // determine Tp
  Rcpp::IntegerVector Tp(N);
  for (int n = 0; n < N; n++) {
    Rcpp::NumericVector dependent_variable_n = dependent_variable[n];
    Rcpp::List independent_variable_n = independent_variable[n];
    if (dependent_variable_n.size() != independent_variable_n.size()) {
      Rcpp::stop("Tp mismatch");
    }
    Tp[n] = independent_variable_n.size();
  }

  // compute cs_Tp
  Rcpp::IntegerVector cs_Tp(N);
  cs_Tp[0] = 0;
  for (int n = 1; n < N; n++) {
    cs_Tp[n] = cs_Tp[n - 1] + Tp[n - 1];
  }

  // return sizes
  return Rcpp::List::create(
    Rcpp::Named("N") = N,
    Rcpp::Named("Tp") = Tp,
    Rcpp::Named("cs_Tp") = cs_Tp
  );
}

//' Normalize independent variables with respect to level
//'
//' @inheritParams setup_data
//' @inheritParams determine_data_sizes
//'
//' @return
//' The input \code{independent_variable}, where for each design \code{matrix}
//' differences with respect to the level \code{reference_level} are taken.
//'
// [[Rcpp::export]]

Rcpp::List reference_normalization(
    Rcpp::List independent_variable,
    int J,
    int reference_level
) {

  // extract function to generate delta operator
  Rcpp::Environment oeli = Rcpp::Environment::namespace_env("oeli");
  Rcpp::Function delta = oeli["delta"];
  arma::mat delta_level = Rcpp::as<arma::mat>(delta(reference_level, J));

  // loop over all individuals
  int N = independent_variable.size();
  for (int n = 0; n < N; n++) {

    Rcpp::List indenpendent_variable_n = independent_variable[n];
    int Tp_n = indenpendent_variable_n.size();

    // loop over all time points
    for (int t = 0; t < Tp_n; t++) {

      arma::mat independent_variable_n_t = Rcpp::as<arma::mat>(indenpendent_variable_n[t]);

      if (J != independent_variable_n_t.n_rows) {
        Rcpp::stop("J mismatch");
      }

      // normalize level
      indenpendent_variable_n[t] = delta_level * independent_variable_n_t;

    }

    independent_variable[n] = indenpendent_variable_n;

  }

  // return transformed independent variables
  return independent_variable;
}

//' Function to setup data for MCMC
//'
//' @description
//' 1. TODO
//' 2.
//' 3.
//'
//' @param data
//' A \code{list} that contains the data.
//' It is of length \code{N}, where \code{N} is the number of individuals.
//' The \code{n}-th element of \code{data} contains the data for the \code{n}-th
//' individual and is a \code{list} with two elements:
//' * \code{X} is a \code{list} of length \code{Tp[n]}, where the
//'   \code{t}-th element is the design \code{matrix} of \code{n}-th independent
//'   variables for time point \code{t}.
//'   Each design \code{matrix} must have \code{P_f + P_r} columns.
//'   In the ordered case, each design \code{matrix} must have a single row.
//'   In the non-ordered case, each design \code{matrix} have \code{J} rows.
//' * \code{y} is a \code{vector} of length \code{Tp[n]}, where the
//'   \code{t}-th element is an \code{integer} that refers to the dependent
//'   variable from the underlying choice set.
//'
//' @param J
//' An \code{integer}, the number of levels of the dependent variable.
//'
//' @param reference_level
//' An \code{integer} between \code{1} and \code{J} that determines the
//' reference level for level normalization. It is not relevant and ignored in
//' the ordered case.
//'
//' @param ordered
//' A \code{logical} that determines if the dependent variable is ordered.
//'
//' @param ranked
//' A \code{logical} that determines if the dependent variable is ranked.
//'
//' @return
//' A \code{list}... TODO
//'
// [[Rcpp::export]]

Rcpp::List setup_data(
    Rcpp::List data,
    int J = 2,
    int P_f = 0,
    int P_r = 0,
    int reference_level = 1,
    bool ordered = false,
    bool ranked = false
  ) {

  Rcpp::List data_split = split_data(data);
  Rcpp::List dependent_variable = data_split["dependent_variable"];
  Rcpp::List independent_variable = data_split["independent_variable"];
  Rcpp::List data_sizes = determine_data_sizes(
    dependent_variable, independent_variable
  );

  if (!ordered) {
    independent_variable = reference_normalization(
      independent_variable, J, reference_level
    );
  }

  return Rcpp::List::create(
    Rcpp::Named("dependent_variable") = dependent_variable,
    Rcpp::Named("independent_variable") = independent_variable,
    Rcpp::Named("data_sizes") = data_sizes
  );



  // return Rcpp::List::create(
  //   //Rcpp::Named("N") = reference_level
  //   // Named("J") = J,
  //   // Named("P_f") = P_f,
  //   // Named("P_r") = P_r,
  //   // Named("Tvec") = T,
  //   // Named("csTvec") = cumsum(T) - T,
  //   // Named("W") = W,
  //   // Named("X") = X,
  //   // Named("y") = y,
  //   // Named("WkW") = WkW,
  //   // Named("XkX") = XkX,
  //   // Named("rdiff") = rdiff
  // );

}































