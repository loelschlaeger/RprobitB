#' Simulate choice data
#'
#' This function simulates choice data from a probit model, see details.
#'
#' @inheritParams RprobitB_formula
#' @param N
#' An \code{integer}, the number of deciders.
#' @param T
#' An \code{integer} of length \code{N}, the number of choice occasions per
#' decider.
#' Can also be a single \code{integer} for a constant number of choice occasions
#' per decider.
#' By default, \code{T = 1}.
#' @inheritParams RprobitB_alternatives
#' @inheritParams RprobitB_data
#' @param covariates
#' A \code{function} with two inputs, say \code{n} and \code{t}.
#' It should return the \code{matrix} of covariates of decider \code{n} at
#' choice occasion \code{t}.
#' The matrix should have dimension \code{J} x \code{P}, where \code{J} is the
#' number of choice alternatives and \code{P} the number of effects.
#' See \code{\link{compute_P}} to determine \code{P}.
#' Each \code{matrix} column corresponds to one effect, see
#' \code{\link{RprobitB_effects}} for the ordering.
#' For more information see the details.
#' @param true_parameter
#' An \code{\link{RprobitB_parameter}} object, which contains the model
#' parameters used for the choice data simulation.
#' By default, \code{RprobitB_parameter = RprobitB_parameter()}, i.e. default
#' parameters are used.
#' @inheritParams RprobitB_parameter
#'
#' @return
#' An \code{\link{RprobitB_data}} object.
#'
#' @details
#' # Choice simulation
#' TODO
#'
#' @inheritSection RprobitB_formula Model formula
#' @inheritSection RprobitB_formula Random effects
#'
#' @examples
#' ### simulate data from a binary probit model with two latent classes
#' data <- simulate_choices(
#'   formula = choice ~ cost | income | time, N = 100, J = 2, T = 10,
#'   alternatives = c("car", "bus"), re = c("cost", "time"),
#'   true_parameter = RprobitB_parameter(C = 2)
#' )
#'
#' ### simulate data from an ordered probit model
#' data <- simulate_choices(
#'   formula = opinion ~ age + gender, N = 100, J = 5,
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
#' @importFrom glue glue glue_collapse

simulate_choices <- function(
    formula, N, J, T = 1, alternatives = LETTERS[1:J], re = NULL,
    ordered = FALSE, ranked = FALSE,
    covariates = function(n, t) {
      P <- compute_P(formula, re, J)
      matrix(rnorm(P * J, mean = 0, sd = 9), nrow = J, ncol = P)
    },
    true_parameter = RprobitB_parameter(), seed = NULL
  ) {
  # TODO check 'ranked', maybe 'RprobitB_choice_set()'?
  stopifnot(is_pos_int(N))
  if (is.numeric(T) && length(T) == 1) T <- rep(T, N)
  stopifnot(
    is_pos_int(J), is.numeric(T), length(T) == N, sapply(T, is_pos_int),
    is.function(covariates), is.RprobitB_parameter(true_parameter)
  )
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
  RprobitB_effects <- RprobitB_effects(
    RprobitB_formula = RprobitB_formula,
    RprobitB_alternatives = RprobitB_alternatives
  )
  P <- compute_P(formula = formula, re = re, J = J, ordered = ordered)
  n_test <- sample.int(N, size = 1)
  t_test <- sample.int(T[n_test], size = 1)
  test_cov <- try(covariates(n_test, t_test), silent = TRUE)
  if (inherits(test_cov, "try-error") || !is.matrix(test_cov) ||
      nrow(test_cov) != J || ncol(test_cov) != P) {
    RprobitB_stop(
      "There is an issue with the input 'covariates'.",
      glue::glue("I called 'covariates({n_test},{t_test})', i.e. I tried to ",
      "compute the covariates for decider {n_test} at choice occasion {t_test}."),
      glue::glue("However, the output is not the expected {J} x {P} matrix."),
      "Please check yourself."
    )
  }
  if (!is.null(seed)) set.seed(seed)


  data <- structure(
    lapply(1:N, function(n) {
      out <- lapply(1:T, function(t) {
        X_nt <- X()
        U_nt <- X_nt %*% beta[[n]] + L %*% stats::rnorm(J)
        y_nt <- which.max(U_nt)
        list(X = X_nt, y = y_nt)
      })
      list(X = lapply(out, `[[`, "X"), y = sapply(out, `[[`, "y"))
    })
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
