test_that("RprobitB_covariates can be simulated", {
  x <- simulate_RprobitB_covariates(
    formula = choice ~ cost | 0,
    N = 3, J = 2, T = 1:3, alternatives = c("apple", "windows"),
    ordered = FALSE, seed = 1
  )
  expect_s3_class(x, "RprobitB_covariates")
  expect_true(is.RprobitB_covariates(x))
  expect_true(is.list(x))
  expect_length(x, 3)
  for (i in 1:3) expect_length(x[[i]], i)
  expect_snapshot(x)
})

test_that("custom covariates can be specified for the simulation", {
  x <- simulate_RprobitB_covariates(
    x = RprobitB_covariates(),
    formula = choice ~ cost | price | time,
    alternatives = c("car", "plane", "ship"),
    N = 3, J = 3, T = 1:3,
    re = "price", ordered = FALSE, seed = 1,
    custom_covariates = list(
      "cost" = function() 1:3,
      "price_car" = function() 1
    ),
    overwrite = FALSE, truncate = TRUE
  )
  expect_s3_class(x, "RprobitB_covariates")
  expect_true(is.RprobitB_covariates(x))
  expect_true(is.list(x))
  expect_length(x, 3)
  for (i in 1:3) expect_length(x[[i]], i)
  expect_snapshot(x)
})

test_that("RprobitB_covariates can be expanded", {

})

test_that("T can be expanded", {
  expect_error(
    expand_T(),
    "Please specify the input 'N'."
  )
  expect_error(
    expand_T(N = 3.5),
    "It should be a positive `integer`, the number of deciders."
  )
  expect_error(
    expand_T(N = 10, T = "one"),
    "Input 'T' is misspecified."
  )
  expect_error(
    expand_T(N = 10, T = 1:9),
    "It should be a `vector` of length 'N = 10'."
  )
  expect_error(
    expand_T(N = 10, T = 1.5),
    "It should be a `vector` of `integer` only."
  )
  expect_equal(
    expand_T(N = 10, T = 1),
    c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
  )
  expect_equal(
    expand_T(N = 10, T = 1:10),
    1:10
  )
})
