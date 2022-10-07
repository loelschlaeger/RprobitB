simulate_choices <- function(
    form, N, T = 1, J = 3, alternatives = LETTERS[1:J], re = NULL, ordered = FALSE,
    ranked = FALSE,
    covariates = function(n,t) matrix(rnorm(P(form, re) * J), nrow = P(form, re), ncol = J),
    seed = NULL,
    true_parameter = RprobitB_parameter(
      form = form, N = N, J = J, ordered = ordered, seed = seed)
    ) {

}
