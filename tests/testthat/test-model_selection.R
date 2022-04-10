options("RprobitB_progress" = FALSE)

test_that("model_selection works", {
  data("model_train", package = "RprobitB")
  data("model_train_sparse", package = "RprobitB")
  criteria <- c("npar", "LL", "AIC", "BIC", "WAIC", "MMLL", "BF", "pred_acc")
  expect_snapshot(model_selection(model_train, model_train_sparse, criteria = criteria))
})

test_that("AIC computation works", {
  data("model_train", package = "RprobitB")
  expect_snapshot(AIC(model_train))
})

test_that("BIC computation works", {
  data("model_train", package = "RprobitB")
  expect_snapshot(BIC(model_train))
})

test_that("WAIC computation works", {
  data("model_train", package = "RprobitB")
  expect_snapshot(WAIC(model_train))
})

test_that("nobs works", {
  data("model_train", package = "RprobitB")
  expect_snapshot(nobs(model_train))
})

test_that("logLik computation works", {
  data("model_train", package = "RprobitB")
  expect_snapshot(logLik(model_train, recompute = TRUE))
})

test_that("npar works", {
  data("model_train", package = "RprobitB")
  expect_snapshot(npar(model_train))
})
