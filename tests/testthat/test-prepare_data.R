test_that("P", {
  data("Train", package = "mlogit")
  data <- prepare_data(
    form = choice ~ price | 0 | time + comfort + change,
    choice_data = Train
  )
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("MMNP", {
  data("Train", package = "mlogit")
  data <- prepare_data(
    form = choice ~ price | 0 | time + comfort + change,
    choice_data = Train,
    re = c("price", "time")
  )
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("without choice variable", {
  data("Train", package = "mlogit")
  Train[["choice"]] <- NULL
  expect_warning(
    data <- prepare_data(
      form = choice ~ price | 0 | time + comfort + change,
      choice_data = Train,
      re = c("price", "time"),
      alternatives = c("A", "B")
    )
  )
  expect_error(mcmc(data))
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("train and test data", {
  data("Train", package = "mlogit")
  data <- prepare_data(
    form = choice ~ price | 0 | time + comfort + change,
    choice_data = Train,
    re = c("price", "time")
  )
  data <- train_test(data, test_proportion = 0.3)
  expect_type(data, "list")
  expect_snapshot(print(data))
  for (i in 1:2) {
    expect_snapshot(print(data[[i]]))
    expect_snapshot(summary(data[[i]]))
  }
})
