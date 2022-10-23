test_that("RprobitB_effects can be created", {
  expect_equal(
    RprobitB_effects(
       formula = choice ~ cov,
       re = c("cov+", "ASC+"),
       alternatives = c("C", "B", "A"),
       base = "B"
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
    RprobitB_effects(
      formula = choice ~ A | B + 0 | C,
      re = NULL,
      alternatives = c("A", "B"),
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
})

test_that("Number of effects can be computed", {
  formula <- choice ~ A | B + 0 | C + D
  re <- c("A", "D+")
  J <- 3
  expect_equal(compute_P(formula, re, J), 9)
  expect_equal(compute_P_f(formula, re, J), 5)
  expect_equal(compute_P_r(formula, re, J), 4)
})
