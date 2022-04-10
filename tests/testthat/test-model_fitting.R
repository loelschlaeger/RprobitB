options("RprobitB_progress" = FALSE)

test_that("setting prior parameter works", {
  prior <- check_prior(P_f = 1, P_r = 2, J = 3)
  expect_snapshot(prior)
  expect_s3_class(prior, "RprobitB_prior")
})

test_that("setting of initial Gibbs values works", {
  init <- RprobitB:::set_initial_gibbs_values(
    N = 2, T = 3, J = 3, P_f = 1, P_r = 2, C = 2
  )
  expect_snapshot(init)
})

test_that("RprobitB_latent_class setting works", {
  expect_error(
    RprobitB_latent_classes("not a list")
  )
  expect_snapshot(
    RprobitB_latent_classes(list("C" = 2))
  )
  expect_error(
    RprobitB_latent_classes(list("C" = -1))
  )
  expect_snapshot(
    (out <- RprobitB_latent_classes(list(
      "weight_update" = TRUE,
      "dp_update" = TRUE
    )))
  )
  expect_snapshot(str(out))
})

test_that("building of RprobitB_normalization works", {
  J <- 5
  P_f <- 5
  expect_snapshot(RprobitB_normalization(J = J, P_f = P_f))
  expect_error(
    RprobitB_normalization(J = J, P_f = P_f, level = J + 1),
    "'level' must be equal to 'J'."
  )
  scale <- list("parameter" = "a", "index" = 1, "value" = 1)
  expect_snapshot(RprobitB_normalization(J = J, P_f = P_f, scale = scale))
  expect_error(RprobitB_normalization(J = J, P_f = 0, scale = scale))
})

test_that("Gibbs sampling works", {
  data <- simulate_choices(
    form = choice ~ a | b | c,
    N = 10, T = 1:10, J = 2,
    seed = 1
  )
  model <- mcmc(data, R = 2000, seed = 1)
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
  expect_snapshot(print(coef(model)))
})

test_that("computation of sufficient statistics works", {
  ss <- RprobitB:::sufficient_statistics(
    data = simulate_choices(
      choice ~ v1 | v2,
      N = 2, T = 1:2, J = 3, re = "v2", seed = 1
    ),
    normalization = RprobitB:::RprobitB_normalization(J = 3, P_f = 3)
  )
  expect_snapshot(ss)
})

test_that("estimating a nested model works", {
  mod <- nested_model(model_train,
    form = choice ~ time, R = 100, B = 50
  )
  expect_s3_class(mod, "RprobitB_fit")
})

test_that("transforming B in RprobitB_fit works", {
  model <- RprobitB::model_train
  model_new_B <- transform(model, B = 2)
  expect_s3_class(model_new_B, "RprobitB_fit")
  expect_s3_class(model_new_B$gibbs_samples, "RprobitB_gibbs_samples")
  expect_equal(model_new_B$B, 2)
})

test_that("transforming Q in RprobitB_fit works", {
  model <- RprobitB::model_train
  model_new_Q <- transform(model, Q = 1)
  expect_s3_class(model_new_Q, "RprobitB_fit")
  expect_s3_class(model_new_Q$gibbs_samples, "RprobitB_gibbs_samples")
  expect_equal(model_new_Q$Q, 1)
})

test_that("transforming scale in RprobitB_fit works", {
  model <- RprobitB::model_train
  scale_new <- list("parameter" = "s", "index" = 1, "value" = 1)
  model_new_scale <- transform(
    model,
    scale = scale_new, check_preference_flip = FALSE
  )
  expect_s3_class(model_new_scale, "RprobitB_fit")
  expect_s3_class(model_new_scale$gibbs_samples, "RprobitB_gibbs_samples")
})

test_that("check for preference flip in RprobitB_fit works", {
  model <- RprobitB::model_train
  scale_bad <- list("parameter" = "a", "index" = 1, "value" = 1)
  expect_error(transform(model, scale = scale_bad))
})

test_that("transforming parameter to new scale works", {
  par_old <- RprobitB_parameter(P_f = 1, P_r = 1, J = 3, N = 10)
  normalization <- RprobitB_normalization(
    J = 3, P_f = 1, scale = list("parameter" = "s", "index" = 1, "value" = 1)
  )
  par_transf <- transform_parameter(par_old, normalization)
  expect_s3_class(par_transf, "RprobitB_parameter")
})

test_that("Sigma backtransformation after differencing works", {
  J <- sample(2:10, 1)
  i <- sample(1:J, 1)
  Sigma_full <- rwishart(J, diag(J))$W
  Sigma <- delta(J, i) %*% Sigma_full %*% t(delta(J, i))
  Sigma_back <- undiff_Sigma(Sigma = Sigma, i = i)
  expect_equal(Sigma, delta(J, i) %*% Sigma_back %*% t(delta(J, i)))
})
