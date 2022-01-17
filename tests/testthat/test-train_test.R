test_that("splitting data set by N works", {
  x <- simulate_choices(
    form = choice ~ covariate, N = 10, T = 1:10, J = 2,
    re = "covariate", C = 2, seed = 1
  )
  expect_snapshot(train_test(x, test_proportion = 0.3, by = "N"))
  expect_snapshot(train_test(x, test_proportion = 0, by = "N"))
  expect_snapshot(train_test(x, test_proportion = 1, by = "N"))
  expect_snapshot(train_test(x, test_proportion = 0.5, by = "N", random = TRUE, seed = 1))
  expect_snapshot(train_test(x, test_number = 1, by = "N"))
  expect_snapshot(train_test(x, test_number = 2, by = "N"))
  expect_snapshot(train_test(x, test_number = 1, by = "N", random = TRUE, seed = 1))
})

test_that("splitting data set by T works", {
  x <- simulate_choices(
    form = choice ~ covariate, N = 10, T = 10, J = 2,
    re = "covariate", C = 2, seed = 1
  )
  expect_snapshot(train_test(x, test_proportion = 0.3, by = "T"))
  expect_snapshot(train_test(x, test_proportion = 0.5, by = "T", random = TRUE, seed = 1))
  expect_snapshot(train_test(x, test_number = 1, by = "T"))
  expect_snapshot(train_test(x, test_number = 2, by = "T"))
  expect_snapshot(train_test(x, test_number = 1, by = "T", random = TRUE, seed = 1))
})
