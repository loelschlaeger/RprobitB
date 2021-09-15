test_that("model fitting works", {
  ### probit model
  p = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2, seed = 1)
  m1 = fit(data = p, seed = 1, print_progress = FALSE)
  expect_snapshot(summary(m1))
  ### multinomial probit model
  mnp = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 3, seed = 1)
  m2 = fit(data = mnp, seed = 1, print_progress = FALSE)
  expect_snapshot(summary(m2))
  ### mixed multinomial probit model
  mmnp = simulate(form = choice ~ 0 | var, N = 100, T = 10, J = 3, re = "var",
                  seed = 1)
  m3 = fit(data = mmnp, seed = 1, print_progress = FALSE)
  expect_snapshot(summary(m3))
  ### mixed multinomial probit model with 2 latent classes
  lcmmnp = simulate(form = choice ~ 0 | var, N = 100, T = 10, J = 3,
                    re = "var", C = 2, seed = 1)
  m4 = fit(data = lcmmnp, latent_classes = list("C" = 2), seed = 1,
           print_progress = FALSE)
  expect_snapshot(summary(m4))
  ### update of latent classes
  #m5 = fit(data = lcmmnp, latent_classes = list("update" = TRUE),
  #         print_progress = TRUE)
  #expect_snapshot(summary(m5))
})
