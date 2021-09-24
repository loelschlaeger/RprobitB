test_that("creation of labels works", {
  expect_snapshot(create_labels(P_f = 1, P_r = 2, J = 3, C = 4, symmetric = TRUE))
  expect_snapshot(create_labels(P_f = 1, P_r = 2, J = 3, C = 4, symmetric = FALSE))
  expect_error(create_labels(P_f = 1, P_r = 2, J = 3, C = 0, symmetric = TRUE),
               "'C' must be a number greater or equal 1.")
})
