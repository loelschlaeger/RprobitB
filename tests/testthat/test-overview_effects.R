test_that("effect overview can be created", {
  expect_error(
    overview_effects(),
    "Please specify the input 'RprobitB_formula'."
  )
  expect_error(
    overview_effects(RprobitB_formula = choice ~ A),
    "It should be an `RprobitB_formula` object."
  )
  expect_error(
    overview_effects(RprobitB_formula = RprobitB_formula(formula = A ~ B)),
    "Please specify the input 'RprobitB_alternatives'."
  )
  expect_error(
    overview_effects(
      RprobitB_formula = RprobitB_formula(formula = A ~ B),
      RprobitB_alternatives = 2
    ),
    "It should be an `RprobitB_alternatives` object."
  )
  expect_equal(
    overview_effects(
      RprobitB_formula = RprobitB_formula(
        formula = choice ~ cov,
        re = c("cov+", "ASC+")
      ),
      RprobitB_alternatives = RprobitB_alternatives(
        J = 3,
        alternatives = c("C", "B", "A"),
        base = "B"
      )
    ),
    structure(
      list(
        name = c("cov", "ASC_A", "ASC_C"),
        as_cov = c(TRUE, FALSE, FALSE),
        as_coef = c(FALSE, TRUE, TRUE),
        random = c(TRUE, TRUE, TRUE),
        log_norm = c(TRUE, TRUE, TRUE)
      ),
      row.names = 1:3,
      class = "data.frame"
    )
  )
  expect_equal(
    overview_effects(
      RprobitB_formula = RprobitB_formula(
        formula = choice ~ A | B + 0 | C,
        re = NULL
      ),
      RprobitB_alternatives = RprobitB_alternatives(
        J = 2,
        alternatives = c("A", "B")
      )
    ),
    structure(
      list(
        name = c("A", "B_B", "C_A", "C_B"),
        as_cov = c(TRUE, FALSE, TRUE, TRUE),
        as_coef = c(FALSE, TRUE, TRUE, TRUE),
        random = c(FALSE, FALSE, FALSE, FALSE),
        log_norm = c(FALSE, FALSE, FALSE, FALSE)
      ),
      row.names = 1:4,
      class = "data.frame"
    )
  )
  expect_equal(
    overview_effects(
      RprobitB_formula = RprobitB_formula(
        formula = choice ~ A + B + C,
        re = "A+"
      ),
      RprobitB_alternatives = RprobitB_alternatives(
        J = 3,
        ordered = TRUE
      )
    ),
    structure(
      list(
        name = c("B", "C", "A"),
        as_cov = c(FALSE, FALSE, FALSE),
        as_coef = c(FALSE, FALSE, FALSE),
        random = c(FALSE, FALSE, TRUE),
        log_norm = c(FALSE, FALSE, TRUE)
      ),
      row.names = c(NA, -3L),
      class = "data.frame"
    )
  )
})

test_that("number of effects can be computed", {
  formula <- choice ~ A | B + 0 | C + D
  re <- c("A", "D+")
  J <- 3
  expect_equal(compute_P(formula, re, J), 9)
  expect_equal(compute_P_f(formula, re, J), 5)
  expect_equal(compute_P_r(formula, re, J), 4)
})

test_that("number of covariates per decider can be computed", {
  expect_equal(
    number_covariates(formula <- choice ~ cost | income | time, J = 3),
    8
  )
})
