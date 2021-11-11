test_that("P", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 2,
                  alternatives = c("bus","car"),
                  seed = 1,
                  alpha = 1:5, Sigma = 1)
  model = mcmc(data, R = 1000, print_progress = FALSE, seed = 1)
  expect_warning(classify(model))
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
  expect_warning(classify(model))
})

test_that("MMNP", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 3,
                  re = c("cost","ASC"),
                  alternatives = c("train","bus","car"),
                  seed = 1,
                  alpha = 1:5, b = 1:3, Omega = as.numeric(diag(3)),
                  Sigma = diag(2))
  model = mcmc(data, R = 1000, print_progress = FALSE, seed = 1)
  expect_snapshot(classify(model))
})
