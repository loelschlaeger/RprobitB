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
