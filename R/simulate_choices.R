#' Simulate choice data
#'
#' @description
#' This function simulates choice data from a probit model. It helps to create
#' an \code{\link{RprobitB_data}} object.
#'
#' @param RprobitB_covariates
#' An \code{\link{RprobitB_covariates}} object, which contains the covariate
#' matrices used for the choice data simulation.
#' @param true_parameter
#' An \code{\link{RprobitB_parameter}} object, which contains the model
#' parameters used for the choice data simulation.
#' By default, \code{RprobitB_parameter = RprobitB_parameter()}, i.e. default
#' parameters are used.
#' @param ranked
#' TODO
#'
#' @return
#' An \code{\link{RprobitB_data}} object.
#'
#' @inheritSection RprobitB_formula Model formula
#' @inheritSection RprobitB_formula Random effects
#'
#' @examples
#' ### simulate data from a binary probit model with two latent classes
#' data <- simulate_choices(
#'   formula = choice ~ cost | income | time, N = 10, J = 2, T = 1:10,
#'   alternatives = c("car", "bus"), re = c("cost", "time"),
#'   true_parameter = RprobitB_parameter(C = 2)
#' )
#'
#' ### simulate data from an ordered probit model
#' data <- simulate_choices(
#'   formula = opinion ~ age + gender, N = 50, J = 5,
#'   alternatives = c("very bad", "bad", "indifferent", "good", "very good"),
#'   ordered = TRUE
#' )
#'
#' ### simulate data from a ranked probit model
#' data <- simulate_choices(
#'   formula = product ~ price, N = 10, J = 3, T = 1:10, ranked = TRUE
#' )
#'
#' @export
#'
#' @importFrom glue glue glue_collapse
#'
#' @seealso
#' \itemize{
#'   \item TODO
#' }

simulate_choices <- function(
  RprobitB_covariates = simulate_RprobitB_covariates(
    formula, N, J, T = 1, alternatives = LETTERS[1:J], re = NULL,
    ordered = FALSE
  ), true_parameter = RprobitB_parameter(), ranked = FALSE, seed = NULL,
  column_choice = "choice", column_decider = "id", column_occasion = "idc"
  ) {

  ### construct objects
  T <- expand_T(N = N, T = T)
  RprobitB_formula <- RprobitB_formula(
    formula = formula, re = re, ordered = ordered
  )
  RprobitB_parameter <- simulate_RprobitB_parameter(
    x = true_parameter, formula = formula, re = re, ordered = ordered,
    J = J, N = N, seed = seed
  )
  RprobitB_alternatives <- RprobitB_alternatives(
    J = J, alternatives = alternatives, ordered = ordered
  )
  effects <- overview_effects(
    RprobitB_formula = RprobitB_formula,
    RprobitB_alternatives = RprobitB_alternatives
  )


  # TODO check 'ranked', maybe 'RprobitB_choice_set()'?


  if (!is.null(seed)) {
    set.seed(seed)
  }
  data_list <- lapply(1:N, function(n) {
    z_n <- RprobitB_parameter$z[n]
    coef <- c(RprobitB_parameter$alpha[,z_n], RprobitB_parameter$beta[,z_n])
    out <- lapply(1:T[n], function(t) {
      X_nt <- cov_vec_to_mat(
        cov_vec = covariates(n, t),
        RprobitB_effects = RprobitB_effects
      )
      U_nt <- rmvnorm_cpp(
        mean = as.vector(X_nt %*% coef),
        Sigma = RprobitB_parameter$Sigma
      )
      y_nt <- which.max(U_nt)
      list(X = X_nt, y = y_nt)
    })
    list(X = lapply(out, `[[`, "X"), y = lapply(out, `[[`, "y"))
  })
  data <- structure(
    cbind(
      data.frame(
        id = rep(1:N, times = T),
        idc = unlist(sapply(T, seq.int, simplify = FALSE)),
        choice = RprobitB_alternatives$alternatives[unlist(lapply(data_list, `[[`, "y"))]
      ),
      data.frame(
        matrix(unlist(lapply(data_list, `[[`, "X")), ncol = P*J, byrow = TRUE)
      )
    )
  )


  # TODO

  RprobitB_data(
    data = data,
    ordered = ordered,
    ranked = ranked,
    simulated = TRUE,
    true_parameter = RprobitB_parameter
  )



}
