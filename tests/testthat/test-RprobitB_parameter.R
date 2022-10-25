test_that("RprobitB_parameter can be created and validated", {
  x <- RprobitB_parameter()
  expect_true(is.RprobitB_parameter(x))
  expect_s3_class(
    x,
    "RprobitB_parameter"
  )
  formula <- choice ~ A | B
  re <- "A"
  J <- 3
  N <- 100
  x <- simulate_RprobitB_parameter(x, formula = formula, re = re, J = J, N = N)
  expect_s3_class(
    x,
    "RprobitB_parameter"
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        "alpha" = 1:2, "b" = 1, "Omega" = 1, "Sigma" = diag(3)
      ),
      formula = formula, re = re, J = J, N = N
    )
  )
})
