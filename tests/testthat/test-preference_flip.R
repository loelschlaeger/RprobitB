options("RprobitB_progress" = FALSE)

test_that("flip in preferences can be detected", {
  set.seed(1)
  form <- choice ~ A + B | 0
  data <- simulate_choices(form = form, N = 100, T = 10, J = 3, re = "B")
  model <- fit_model(data = data, R = 100, scale = "A := 1")
  model_new <- transform(model, scale = "A := -1", check_preference_flip = FALSE)
  expect_error(
    preference_flip(model, model_new),
    "This transformation seems to flip the preferences."
  )
})
