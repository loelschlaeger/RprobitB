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
  model <- mcmc(data, R = 1000, print_progress = FALSE, seed = 1)
  expect_snapshot(log_likelihood(x = model))
  expect_snapshot(log_likelihood(x = model, par_set = "true"))
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
  expect_snapshot(log_likelihood(x = model))
  expect_snapshot(log_likelihood(x = model, par_set = "true"))
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
  expect_snapshot(log_likelihood(x = model))
  expect_snapshot(log_likelihood(x = model, par_set = "true"))
})

test_that("LCMMNP", {
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 30,
    T = 10,
    J = 3,
    re = c("cost", "ASC"),
    alternatives = c("train", "bus", "car"),
    seed = 1,
    C = 2
  )
  model <- mcmc(data,
    R = 1000, print_progress = FALSE, seed = 1,
    latent_classes = list("C" = 2)
  )
  expect_snapshot(log_likelihood(x = model))
  expect_snapshot(log_likelihood(x = model, par_set = "true"))
})

test_that("ULCMMNP", {
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 100,
    T = 5,
    J = 3,
    re = c("cost", "ASC"),
    alternatives = c("train", "bus", "car"),
    seed = 1,
    C = 2
  )
  model <- mcmc(data,
    R = 2000, print_progress = FALSE, seed = 1,
    latent_classes = list(
      "C" = 8, "weight_update" = TRUE, "epsmin" = 0.1,
      "epsmax" = 0.9
    )
  )
  expect_snapshot(log_likelihood(x = model))
  expect_snapshot(log_likelihood(x = model, par_set = "true"))
})
