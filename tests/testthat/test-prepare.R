test_that("P", {
  data("Train", package = "mlogit")
  data <- prepare(
    form = choice ~ price | 0 | time + comfort + change,
    choice_data = Train
  )
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("MMNP", {
  data("Train", package = "mlogit")
  data <- prepare(
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
  data <- prepare(
    form = choice ~ price | 0 | time + comfort + change,
    choice_data = Train,
    re = c("price", "time"),
    alternatives = c("A", "B")
  )
  expect_error(mcmc(data))
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("train and test data", {
  data("Train", package = "mlogit")
  data <- prepare(
    form = choice ~ price | 0 | time + comfort + change,
    choice_data = Train,
    re = c("price", "time"),
    test_prop = 0.2
  )
  expect_type(data, "list")
  expect_snapshot(print(data))
  for (i in 1:2) {
    expect_snapshot(print(data[[i]]))
    expect_snapshot(summary(data[[i]]))
  }
})
