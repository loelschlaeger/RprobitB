test_that("P", {
  data("Train", package = "mlogit")
  data = prepare(form = choice ~ price | 0 | time + comfort + change,
                 choice_data = Train)
  expect_snapshot(data$data)
  expect_snapshot(data$choice_data)
  expect_snapshot(data$true_parameter)
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

