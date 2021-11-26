test_that("building of RprobitB_normalization works", {
  J <- 5
  P_f <- 5
  expect_snapshot(RprobitB_normalization(J = J, P_f = P_f))
  expect_error(
    RprobitB_normalization(J = J, P_f = P_f, level = J + 1),
    "'level' must be equal to 'J'."
  )
  scale <- list("parameter" = "a", "index" = 1, "value" = 1)
  expect_snapshot(RprobitB_normalization(J = J, P_f = P_f, scale = scale))
  expect_error(
    RprobitB_normalization(J = J, P_f = 0, scale = scale),
    "Cannot use 'alpha' for normalization because the model has no fixed coefficients."
  )
})
