options("RprobitB_progress" = FALSE)

test_that("computing model selection criteria works", {
  set.seed(1)
  form <- choice ~ price + time + change + comfort | 0
  data <- prepare_data(
    form = form,
    choice_data = train_choice,
    id = "deciderID",
    idc = "occasionID"
  )
  model_train <- fit_model(
    data = data,
    scale = "price := -1",
    R = 100,
    B = 90
  )
  model_train <- compute_p_si(model_train, ncores = 1)
  criteria <- c("npar", "LL", "AIC", "BIC", "WAIC", "MMLL", "BF", "pred_acc")
  expect_snapshot(model_selection(model_train, criteria = criteria))
  expect_snapshot(AIC(model_train))
  expect_snapshot(BIC(model_train))
  expect_snapshot(WAIC(model_train))
  expect_snapshot(nobs(model_train))
  expect_snapshot(logLik(model_train, recompute = TRUE))
  expect_snapshot(npar(model_train))
})
