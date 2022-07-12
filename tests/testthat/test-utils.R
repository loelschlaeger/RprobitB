test_that("building delta matrix works", {
  expect_snapshot(RprobitB:::delta(3, 1))
})

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

test_that("check for covariance matrix works", {
  expect_true(is_covariance_matrix(diag(3)))
  expect_false(is_covariance_matrix(matrix(-1, 3, 3)))
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
