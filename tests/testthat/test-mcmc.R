test_that("P", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 2,
                  alternatives = c("bus","car"),
                  seed = 1,
                  alpha = 1:5, Sigma = 1)
  model = mcmc(data, R = 1000, print_progress = FALSE, seed = 1)
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
})

test_that("MNP", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 3,
                  alternatives = c("train","bus","car"),
                  seed = 1,
                  alpha = 1:8)
  model = mcmc(data, R = 1000, print_progress = FALSE, seed = 1)
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
})
