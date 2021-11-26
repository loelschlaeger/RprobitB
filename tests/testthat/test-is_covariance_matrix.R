test_that("checks for covariance matrix work", {
  expect_true(is_covariance_matrix(diag(3)))
  expect_false(is_covariance_matrix(matrix(-1, 3, 3)))
})
