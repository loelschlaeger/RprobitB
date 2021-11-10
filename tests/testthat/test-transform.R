test_that("P", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 2,
                  alternatives = c("bus","car"),
                  seed = 1,
                  alpha = 1:5, Sigma = 1)
  model = mcmc(data, R = 1000, print_progress = FALSE, seed = 1)
  model_new_B = transform(model, B = 2)
  model_new_Q = transform(model, Q = 2)
  model_new_scale = transform(model, scale = list("parameter" = "a",
                                                  "index" = 1, "value" = 1))
  expect_s3_class(model_new_B, "RprobitB_model")
  expect_s3_class(model_new_Q, "RprobitB_model")
  expect_s3_class(model_new_scale, "RprobitB_model")
})
