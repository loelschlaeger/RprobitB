simulate_choices <- function(
    formula, N, T = 1, J = 3, alternatives = LETTERS[1:J], re = NULL, ordered = FALSE,
    ranked = FALSE,
    covariates = function(n,t) matrix(rnorm(P(formula, re) * J), nrow = P(formula, re), ncol = J),
    seed = NULL,
    true_parameter = RprobitB_parameter(
      formula = formula, N = N, J = J, ordered = ordered, seed = seed)
    ) {

}
