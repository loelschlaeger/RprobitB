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
  expect_error(
    overview_effects(
      RprobitB_formula = RprobitB_formula(formula = A ~ B),
      RprobitB_alternatives = RprobitB_alternatives(J = 3),
      delimiter = 1
    ),
    "It should be a single `character`."
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
        covariate = c("cov", NA, NA),
        alternative = c(NA, "A", "C"),
        as_covariate = c(TRUE, FALSE, FALSE),
        as_effect = c(FALSE, TRUE, TRUE),
        random = c(TRUE, TRUE, TRUE),
        log_normal = c(TRUE, TRUE, TRUE)
      ),
      row.names = c(NA, -3L),
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
      ),
      delimiter = "*"
    ),
    structure(
      list(
        name = c("A", "B*B", "C*A", "C*B"),
        covariate = c("A", "B", "C", "C"),
        alternative = c(NA, "B", "A", "B"),
        as_covariate = c(TRUE, FALSE, TRUE, TRUE),
        as_effect = c(FALSE, TRUE, TRUE, TRUE),
        random = c(FALSE, FALSE, FALSE, FALSE),
        log_normal = c(FALSE, FALSE, FALSE, FALSE)
      ),
      row.names = c(NA, -4L),
      class = "data.frame"
    )
  )
  expect_equal(
    overview_effects(
      RprobitB_formula = RprobitB_formula(
        formula = choice ~ A + B + C,
        re = "A+", ordered = TRUE
      ),
      RprobitB_alternatives = RprobitB_alternatives(
        J = 3, ordered = TRUE
      )
    ),
    structure(
      list(
        name = c("B", "C", "A"),
        covariate = c("B", "C", "A"),
        alternative = c(NA_character_, NA_character_, NA_character_),
        as_covariate = c(FALSE, FALSE, FALSE),
        as_effect = c(FALSE, FALSE, FALSE),
        random = c(FALSE, FALSE, TRUE),
        log_normal = c(FALSE, FALSE, TRUE)
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
