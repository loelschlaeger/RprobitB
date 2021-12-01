test_that("P", {
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 10,
    T = 1:10,
    J = 2,
    alternatives = c("bus", "car"),
    seed = 1,
    alpha = 1:5
  )
  model <- mcmc(data, print_progress = FALSE)
  expect_true(is.data.frame(choice_probs(model)))
})

test_that("P train test", {
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 10,
    T = 1:10,
    J = 2,
    alternatives = c("bus", "car"),
    seed = 1,
    test_prop = 0.2,
    alpha = 1:5
  )
  model <- mcmc(data$train, print_progress = FALSE)
  expect_true(is.data.frame(choice_probs(model, data = data$test)))
})
