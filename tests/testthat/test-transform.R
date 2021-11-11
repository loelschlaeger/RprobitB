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
                                                  "index" = 1, "value" = 1),
                              check_preference_flip = FALSE)
  expect_s3_class(model_new_B, "RprobitB_model")
  expect_s3_class(model_new_Q, "RprobitB_model")
  expect_s3_class(model_new_scale, "RprobitB_model")
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
  model_new_B = transform(model, B = 2)
  model_new_Q = transform(model, Q = 2)
  model_new_scale = transform(model, scale = list("parameter" = "a",
                                                  "index" = 1, "value" = 1),
                              check_preference_flip = FALSE)
  expect_s3_class(model_new_B, "RprobitB_model")
  expect_s3_class(model_new_Q, "RprobitB_model")
  expect_s3_class(model_new_scale, "RprobitB_model")
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
  model_new_B = transform(model, B = 2)
  model_new_Q = transform(model, Q = 2)
  model_new_scale = transform(model, scale = list("parameter" = "a",
                                                  "index" = 1, "value" = 1),
                              check_preference_flip = FALSE)
  expect_s3_class(model_new_B, "RprobitB_model")
  expect_s3_class(model_new_Q, "RprobitB_model")
  expect_s3_class(model_new_scale, "RprobitB_model")
})

test_that("LCMMNP", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 30,
                  T = 10,
                  J = 3,
                  re = c("cost","ASC"),
                  alternatives = c("train","bus","car"),
                  seed = 1,
                  C = 2)
  model = mcmc(data, R = 1000, print_progress = FALSE, seed = 1,
               latent_classes = list("C" = 2))
  model_new_B = transform(model, B = 2)
  model_new_Q = transform(model, Q = 2)
  model_new_scale = transform(model, scale = list("parameter" = "a",
                                                  "index" = 1, "value" = 1),
                              check_preference_flip = FALSE)
  expect_s3_class(model_new_B, "RprobitB_model")
  expect_s3_class(model_new_Q, "RprobitB_model")
  expect_s3_class(model_new_scale, "RprobitB_model")
})
