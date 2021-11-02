test_that("probit model fitting works", {
  skip_on_cran()
  p = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2, seed = 1)
  m1 = mcmc(data = p, seed = 1, print_progress = FALSE)
  expect_snapshot(unclass(m1$gibbs_samples))
  expect_snapshot(summary(m1))
})
