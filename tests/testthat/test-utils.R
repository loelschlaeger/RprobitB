test_that("checking booleans works", {
  expect_false(is_bool("TRUE"))
  expect_true(is_bool(FALSE))
})

test_that("checking single numeric works", {
  expect_true(is_single_numeric(1))
  expect_false(is_single_numeric("1"))
  expect_false(is_single_numeric(NA_real_))
})

test_that("checking positive integer works", {
  expect_false(is_pos_int(-1))
  expect_true(is_pos_int(1))
  expect_false(is_pos_int(NA_real_))
  expect_false(is_pos_int(1.1))
})

test_that("sampling and checking covariance matrix works", {
  expect_true(is_cov_matrix(diag(10)))
  x <- sample_cov_matrix(dim = 3)
  expect_true(is_cov_matrix(x))
})

test_that("Extraction of function body as character works", {
  test_fun <- function(x) {
    stopifnot(is.numeric(x))
    {x + 1}
  }
  expect_equal(
    function_body(test_fun),
    "stopifnot(is.numeric(x)) { x + 1 }")
  expect_equal(
    function_body(test_fun, braces = TRUE),
    "{ stopifnot(is.numeric(x)) { x + 1 } }")
  expect_equal(
    function_body(test_fun, nchar = 20),
    "stopifnot(is.nume..."
  )
})



### TODO Not revised from here

test_that("Gelman-Rubin statistic can be computed", {
  set.seed(1)
  no_chains <- 2
  length_chains <- 1e3
  samples <- matrix(NA_real_, length_chains, no_chains)
  samples[1, ] <- 1
  Gamma <- matrix(c(0.8, 0.1, 0.2, 0.9), 2, 2)
  for (c in 1:no_chains) {
    for (t in 2:length_chains) {
      samples[t, c] <- sample(1:2, 1, prob = Gamma[samples[t - 1, c], ])
    }
  }
  expect_snapshot(R_hat(samples))
})

test_that("printing abbreviated matrices and vectors works", {
  expect_snapshot(RprobitB:::pprint(x = 1, name = "single integer"))
  expect_snapshot(RprobitB:::pprint(x = LETTERS[1:26], name = "letters"))
  expect_snapshot(RprobitB:::pprint(
    x = matrix(rnorm(100), ncol = 1),
    name = "single column matrix"
  ))
  expect_snapshot(RprobitB:::pprint(
    x = matrix(1:100, nrow = 1),
    name = "single row matrix"
  ))
  expect_snapshot(RprobitB:::pprint(
    x = matrix(LETTERS[1:24], ncol = 6),
    name = "big matrix"
  ))
})

test_that("computation of permutations works", {
  expect_snapshot(RprobitB:::permutations(x = c("a", "b", "c")))
})
