test_that("choice prediction works", {
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 10,
    T = 1:10,
    J = 2,
    alternatives = c("bus", "car"),
    seed = 1,
    alpha = 1:5, Sigma = 1
  )
  data <- train_test(data, test_proportion = 0.3)
  model <- mcmc(data$train, R = 1000, print_progress = FALSE, seed = 1)
  expect_snapshot(predict(model, overview = TRUE))
  expect_snapshot(predict(model, overview = FALSE))
  expect_snapshot(predict(model, data = data$test, overview = TRUE))
})
