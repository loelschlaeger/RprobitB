options("RprobitB_progress" = FALSE)

test_that("estimated covariance matrix of mixing distribution can be extracted", {
  set.seed(1)
  form <- choice ~ var | 0
  data <- simulate_choices(form = form, N = 100, T = 10, J = 3, re = "var")
  model <- fit_model(data = data, R = 100, latent_classes = list(C = 2))
  checkmate::expect_list(cov_mix(model), len = 2)
})
