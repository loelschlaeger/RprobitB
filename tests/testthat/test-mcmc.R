test_that("Gibbs sampling works", {
  data <- simulate_choices(
    form = choice ~ a | b | c,
    N = 10, T = 1:10, J = 2,
    seed = 1
  )
  model <- mcmc(data, R = 2000, print_progress = FALSE, seed = 1)
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
  expect_snapshot(print(coef(model)))
})

### The following are consistency tests and are only run on request.
skip_consistency_test <- function() if (T) skip("Skip consistency test.")

test_that("consistency of MNP", {
  skip_consistency_test()
  data <- simulate_choices(
    form = choice ~ a | b | c,
    N = 100, T = 50, J = 3,
    seed = 1,
  )
  model <- mcmc(data, R = 10000, print_progress = FALSE, seed = 1)
  expect_snapshot(summary(model))
})

test_that("consistency of MMNP", {
  skip_consistency_test()
  data <- simulate_choices(
    form = choice ~ a | b | c,
    N = 100, T = 50, J = 3,
    re = c("a", "b", "c", "ASC"),
    seed = 1
  )
  model <- mcmc(data, R = 10000, print_progress = FALSE, seed = 1)
  expect_snapshot(summary(model))
})

test_that("consistency of LCMMNP", {
  skip("Not implemented.")
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 30,
    T = 5,
    J = 3,
    re = c("cost", "ASC"),
    alternatives = c("train", "bus", "car"),
    seed = 1,
    C = 2
  )
  model <- mcmc(data,
    R = 500, print_progress = FALSE, seed = 1,
    latent_classes = list("C" = 2)
  )
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
  expect_snapshot(print(coef(model)))
})

test_that("consistency of LCMMNP weight update", {
  skip("Not implemented.")
  data <- simulate_choices(
    form = choice ~ cost,
    N = 100,
    T = 10,
    J = 3,
    re = c("cost", "ASC"),
    alternatives = c("train", "bus", "car"),
    seed = 1,
    C = 2
  )
  model <- mcmc(data,
    R = 500, print_progress = FALSE, seed = 1,
    latent_classes = list(
      "C" = 8, "weight_update" = TRUE, "epsmin" = 0.1, "epsmax" = 0.9
    )
  )
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
  expect_snapshot(print(coef(model)))
})

test_that("consistency of LCMMNP DP update", {
  skip("Not implemented.")
  data <- simulate_choices(
    form = choice ~ cost,
    N = 100,
    T = 10,
    J = 3,
    re = c("cost", "ASC"),
    alternatives = c("train", "bus", "car"),
    seed = 1,
    C = 2
  )
  model <- mcmc(data,
    R = 500, print_progress = FALSE, seed = 1,
    latent_classes = list(
      "C" = 8, "Cmax" = 10, "dp_update" = TRUE
    )
  )
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
  expect_snapshot(print(coef(model)))
})

test_that("consistency of LCMMNP weight and DP update", {
  skip("Not implemented.")
  data <- simulate_choices(
    form = choice ~ cost,
    N = 100,
    T = 10,
    J = 3,
    re = c("cost", "ASC"),
    alternatives = c("train", "bus", "car"),
    seed = 1,
    C = 2
  )
  model <- mcmc(data,
    R = 500, print_progress = FALSE, seed = 1,
    latent_classes = list(
      "C" = 8, "weight_update" = TRUE, "dp_update" = TRUE, "epsmin" = 0.1, "epsmax" = 0.9
    )
  )
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
  expect_snapshot(print(coef(model)))
})
