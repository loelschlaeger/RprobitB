fit_model <- function(

  formula, RprobitB_data, alternatives, re = NULL, C = 1,
  R = 1000, chains = 1,

  priors = RprobitB_priors_conjugate(
    formula = formula, re = re, J = length(alternatives),
    ordered = RprobitB_data$ordered
  ),

  initials = RprobitB_initial_values(),

  fixed_parameter = list(),

  lcu = list(),

  seed = NULL,
  ncores = 1,
  verbose = TRUE

  ) {


  RprobitB_formula <- new_RprobitB_formula(
    formula = formula, re = re, ordered = RprobitB_data$ordered
  )

}
