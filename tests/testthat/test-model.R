test_that("print.RprobitB_fit prints a concise model overview", {
  model <- make_test_fit()
  output <- capture.output(returned <- print(model))
  expect_identical(returned, model)
  expect_true(any(grepl("Bayesian probit choice model", output)))
  expect_true(any(grepl("4 deciders, 4 choice occasions", output)))
})

test_that("formula.RprobitB_fit returns the fitted formula", {
  model <- make_test_fit()
  expect_identical(formula(model), model$model$formula)
})

test_that("model.frame.RprobitB_fit returns plain fitted data", {
  model <- make_test_fit()
  expect_s3_class(model.frame(model), "data.frame", exact = TRUE)
  expect_identical(nrow(model.frame(model)), 4L)
})

test_that("nobs.RprobitB_fit counts independent likelihood units", {
  panel <- data.frame(
    deciderID = c(1L, 1L, 2L, 2L),
    occasionID = c(1L, 2L, 1L, 2L),
    choice = c("A", NA, "B", "A"),
    x_A = c(0, 1, 0, 1),
    x_B = c(1, 0, 1, 0)
  )
  fixed <- make_test_fit(panel)
  expect_identical(nobs(fixed), 3L)
  mixed <- make_test_fit(panel, random_effects = "x")
  expect_identical(nobs(mixed), 2L)
})

test_that("update.RprobitB_fit refits with replaced arguments", {
  model <- make_test_fit()
  updated <- update(model, iterations = 30L)
  expect_s3_class(updated, "RprobitB_fit")
  expect_identical(updated$sampler$iterations, 30L)
  expect_identical(
    as.data.frame(updated$data), as.data.frame(model$data)
  )
})

test_that("update.RprobitB_fit updates the formula part by part", {
  data <- data.frame(
    deciderID = 1:4,
    choice = c("A", "B", "A", "B"),
    x_A = c(0, 1, 0, 1),
    x_B = c(1, 0, 1, 0),
    y = c(1, 0, 1, 0)
  )
  model <- make_test_fit(data)
  updated <- update(model, . ~ . | . + y | .)
  expect_identical(
    deparse1(formula(updated)), "choice ~ x | y - 1 | 1 - 1"
  )
})

test_that("update.RprobitB_fit returns the call if not evaluated", {
  model <- make_test_fit()
  call <- update(model, chains = 2L, evaluate = FALSE)
  expect_true(is.call(call))
  expect_identical(call$chains, 2L)
  expect_identical(call$data, as.name("data"))
})

test_that("update.RprobitB_fit reuses simulated choice data", {
  set.seed(1)
  model <- fit(
    choice ~ x | 0, n_deciders = 10L, iterations = 20L, warmup = 10L,
    chains = 1L, progress = FALSE
  )
  updated <- update(model, iterations = 30L)
  expect_identical(
    as.data.frame(updated$data), as.data.frame(model$data)
  )
  expect_null(updated$simulation)
})

test_that("update.RprobitB_fit accepts other choice data", {
  model <- make_test_fit()
  data <- data.frame(
    deciderID = 1:2,
    choice = c("A", "B"),
    x_A = c(0, 1),
    x_B = c(1, 0)
  )
  updated <- update(model, data = data)
  expect_identical(nobs(updated), 2L)
})

test_that("update.RprobitB_fit fits a simulated model to supplied data", {
  model <- fit(
    choice ~ x | 0,
    n_deciders = 8L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  subset <- as.data.frame(model$data)[1:5, ]

  result <- update(model, data = subset)

  expect_identical(nobs(result), 5L)
  expect_null(result$simulation)
  expect_false("n_deciders" %in% names(result$call))
})

