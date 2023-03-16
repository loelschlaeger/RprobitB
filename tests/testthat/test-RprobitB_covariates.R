test_that("checks for custom sampler in 'sample_RprobitB_covariates' work", {
  formula <- choice ~ A | B | C
  N <- 3
  T <- 1:N
  J <- 3
  alternatives <- LETTERS[1:J]
  expect_error(
    sample_RprobitB_covariates(
      formula = formula, N = N, J = J, T = T, alternatives = alternatives,
      base = alternatives[1], re = NULL, ordered = FALSE, seed = NULL,
      sampler = function(n, t) rnorm(n = 1, mean = 0, sd = 9),
      "something_else"
    ),
    "Please make sure it is named according to a covariate."
  )
  expect_error(
    sample_RprobitB_covariates(
      formula = formula, N = N, J = J, T = T,
      "not_a_covariate" = 1
    ),
    "But there is no covariate 'not_a_covariate'."
  )
  expect_error(
    sample_RprobitB_covariates(
      formula = formula, N = N, J = J, T = T,
      A = 1
    ),
    "Sampler for 'A' is not a function."
  )
  expect_error(
    sample_RprobitB_covariates(
      formula = formula, N = N, J = J, T = T,
      A = function() 1
    ),
    "It should have the two arguments 'n' and 't'."
  )
  expect_error(
    sample_RprobitB_covariates(
      formula = formula, N = N, J = J, T = T,
      A = function(n, t, j = 1) 1
    ),
    "It should have the two arguments 'n' and 't'."
  )
  expect_error(
    sample_RprobitB_covariates(
      formula = formula, N = N, J = J, T = T,
      A = function(n, t) "A"
    ),
    "The return value was not the expected `numeric` `vector`."
  )
  expect_error(
    sample_RprobitB_covariates(
      formula = formula, N = N, J = J, T = T,
      A = function(n, t) 1
    ),
    "The return value was not of length 3."
  )
})

test_that("RprobitB_covariates can be simulated", {
  formula <- choice ~ cost | age | time
  N <- 3
  T <- 1:N
  J <- 3
  alternatives <- letters[1:J]
  base <- alternatives[1]
  re <- NULL
  ordered <- FALSE
  seed <- 1
  sampler <- function(n, t) rnorm(n = 1, mean = 0, sd = 9)
  x <- sample_RprobitB_covariates(
    formula = formula, N = N, J = J, T = T, alternatives = alternatives,
    base = base, re = re, ordered = ordered, seed = seed, sampler = sampler,
    age = function(n, t) sample(30:80, 1)
  )
  expect_s3_class(x, "RprobitB_covariates")
  expect_true(is.RprobitB_covariates(x))
  expect_true(is.list(x))
  expect_length(x, N)
  for (n in 1:N) {
    expect_length(x[[n]], T[n])
  }
})

test_that("RprobitB_covariates example in details works", {
  x <- sample_RprobitB_covariates(
    formula = choice ~ cost | age | time, N = 3, J = 3, T = 1:3,
    cost = function(n, t) {
      runif(J, 1:3, 2:4)
    },
    age = function(n, t) {
      set.seed(t)
      sample(30:80, 1)
    }
  )
  expect_s3_class(x, "RprobitB_covariates")
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
