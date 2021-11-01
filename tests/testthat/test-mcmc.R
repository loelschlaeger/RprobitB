test_that("probit model fitting works", {
  skip_on_cran()
  p = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2, seed = 1)
  m1 = mcmc(data = p, seed = 1, print_progress = FALSE)
  expect_snapshot(unclass(m1$gibbs_samples))
  expect_snapshot(summary(m1))
})

test_that("multinomial probit model fitting works", {
  skip_on_cran()
  mnp = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 3, seed = 1)
  m2 = mcmc(data = mnp, seed = 1, print_progress = FALSE)
  expect_snapshot(unclass(m2$gibbs_samples))
  expect_snapshot(summary(m2))
})

# test_that("mixed multinomial probit model fitting works", {
#   skip_on_cran()
#   mmnp = simulate(form = choice ~ 0 | var, N = 100, T = 10, J = 3,
#                   re = "var", seed = 1)
#   m3 = mcmc(data = mmnp, seed = 1, print_progress = FALSE)
#   expect_snapshot(unclass(m3$gibbs_samples))
#   expect_snapshot(summary(m3))
# })

# test_that("mixed multinomial probit model with 2 latent classes fitting works", {
#   skip_on_cran()
#   lcmmnp = simulate(form = choice ~ 0 | var, N = 100, T = 10, J = 3,
#                     re = "var", C = 2, seed = 1)
#   m4 = mcmc(data = lcmmnp, latent_classes = list("C" = 2), seed = 1,
#            print_progress = FALSE)
#   expect_snapshot(unclass(m4$gibbs_samples))
#   expect_snapshot(summary(m4))
# })

# test_that("mixed multinomial probit model with updated latent classes fitting works", {
#   skip_on_cran()
#   lcmmnp = simulate(form = choice ~ 0 | var, N = 100, T = 10, J = 3,
#                     re = "var", C = 2, seed = 1)
#   m5 = mcmc(data = lcmmnp, latent_classes = list("update" = TRUE), seed = 1,
#            print_progress = FALSE)
#   expect_snapshot(unclass(m5$gibbs_samples))
#   expect_snapshot(summary(m5))
# })
