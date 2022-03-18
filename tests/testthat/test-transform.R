test_that("model transformation work", {
  data("model_train", package = "RprobitB")
  model <- model_train
  model_new_B <- transform.RprobitB_fit(model, B = 2)
  model_new_Q <- transform.RprobitB_fit(model, Q = 2)
  model_new_scale <- transform.RprobitB_fit(model,
    scale = list(
      "parameter" = "s",
      "index" = 1, "value" = 1
    ),
    check_preference_flip = FALSE
  )
  expect_s3_class(model_new_B, "RprobitB_fit")
  expect_s3_class(model_new_Q, "RprobitB_fit")
  expect_s3_class(model_new_scale, "RprobitB_fit")
})
