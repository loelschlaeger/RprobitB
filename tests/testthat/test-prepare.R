test_that("P", {
  data("Train", package = "mlogit")
  data = prepare(form = choice ~ price | 0 | time + comfort + change,
                 choice_data = Train)
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("MMNP", {
  data("Train", package = "mlogit")
  data = prepare(form = choice ~ price | 0 | time + comfort + change,
                 choice_data = Train,
                 re = c("price","time"))
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

