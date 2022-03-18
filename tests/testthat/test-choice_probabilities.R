test_that("choice probabilities can be computed", {
  data("model_train", package = "RprobitB")
  expect_snapshot(choice_probabilities(model_train))
})
