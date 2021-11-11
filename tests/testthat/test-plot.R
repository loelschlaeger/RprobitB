test_that("P", {
  skip_on_ci()
  skip_on_cran()
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 2,
                  alternatives = c("bus","car"),
                  seed = 1,
                  alpha = 1:5, Sigma = 1)
  model = mcmc(data, R = 1000, print_progress = FALSE, seed = 1)
  build_plot = function(type) plot(model, type = type)
  sink(tempfile())
  on.exit(sink())
  vdiffr::expect_doppelganger("P_effects", build_plot("effects"))
  expect_warning(build_plot("mixture"))
  vdiffr::expect_doppelganger("P_trace", build_plot("trace"))
  vdiffr::expect_doppelganger("P_acf", build_plot("acf"))
})

test_that("MNP", {
  skip_on_ci()
  skip_on_cran()
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 3,
                  alternatives = c("train","bus","car"),
                  seed = 1,
                  alpha = 1:8)
  model = mcmc(data, R = 1000, print_progress = FALSE, seed = 1)
  build_plot = function(type) plot(model, type = type)
  sink(tempfile())
  on.exit(sink())
  vdiffr::expect_doppelganger("MNP_effects", build_plot("effects"))
  expect_warning(build_plot("mixture"))
  vdiffr::expect_doppelganger("MNP_trace", build_plot("trace"))
  vdiffr::expect_doppelganger("MNP_acf", build_plot("acf"))
})

test_that("MMNP", {
  skip_on_ci()
  skip_on_cran()
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
  sink(tempfile())
  on.exit(sink())
  build_plot = function(type) plot(model, type = type)
  vdiffr::expect_doppelganger("MMNP_effects", build_plot("effects"))
  vdiffr::expect_doppelganger("MMNP_mixture", build_plot("mixture"))
  vdiffr::expect_doppelganger("MMNP_trace", build_plot("trace"))
  vdiffr::expect_doppelganger("MMNP_acf", build_plot("acf"))
})
