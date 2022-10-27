#' Simulate choice data
#'
#' This function simulates choice data from a probit model.
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
#' It should return the \code{vector} of covariates of decider \code{n} at
#' choice occasion \code{t}.
#' The vector should have length...
#' TODO
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
#' @importFrom glue glue glue_collapse
#' @seealso [compute_no_cov()] to compute the number of covariates per decider.

simulate_choices <- function(
    formula, N, J, T = 1, alternatives = LETTERS[1:J], re = NULL,
    ordered = FALSE, ranked = FALSE,
    covariates = function(n, t) {
      P <- ... # TODO
      rnorm(P, mean = 0, sd = 9)
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
  no_cov <- ... # TODO
  n_test <- sample.int(N, size = 1)
  t_test <- sample.int(T[n_test], size = 1)
  test_cov <- try(covariates(n_test, t_test), silent = TRUE)
  if (inherits(test_cov, "try-error") || !is.vector(test_cov) ||
      !is.numeric(test_cov) || length(test_cov) != P_without_ASCs) {
    RprobitB_stop(
      "There is an issue with the input 'covariates'.",
      glue::glue("I called 'covariates({n_test}, {t_test})', i.e. I tried to ",
      "compute the covariates for decider {n_test} at choice occasion {t_test}. ",
      "However, the output is not the expected numeric vector of length {P_without_ASCs}."),
      "Please check."
    )
  }
  if (!is.null(seed)) set.seed(seed)
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
