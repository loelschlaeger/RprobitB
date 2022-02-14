test_that("P", {
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 10,
    T = 1:10,
    J = 2,
    alternatives = c("bus", "car"),
    seed = 1,
    alpha = 1:5, Sigma = 1
  )
  model <- mcmc(data, R = 2000, print_progress = FALSE, seed = 1)
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
})

test_that("MNP", {
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 10,
    T = 1:10,
    J = 3,
    alternatives = c("train", "bus", "car"),
    seed = 1,
    alpha = 1:8
  )
  model <- mcmc(data, R = 1000, print_progress = FALSE, seed = 1)
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
})

test_that("MMNP", {
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 10,
    T = 1:10,
    J = 3,
    re = c("cost", "ASC"),
    alternatives = c("train", "bus", "car"),
    seed = 1,
    alpha = 1:5, b = 1:3, Omega = as.numeric(diag(3)),
    Sigma = diag(2)
  )
  model <- mcmc(data, R = 1000, print_progress = FALSE, seed = 1)
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
})

test_that("LCMMNP", {
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
})

test_that("LCMMNP weight update", {
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
})

test_that("LCMMNP DP update", {
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
})

test_that("LCMMNP weight and DP update", {
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
})
