test_that("probit model fitting works", {
  p = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2, seed = 1)
  m = mcmc(data = p, seed = 1, print_progress = FALSE, R = 1e3)
  expect_snapshot(m$gibbs_samples)
  expect_snapshot(summary(m))
})
