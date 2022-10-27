test_that("choice simulation works", {
  # TODO
  expect_s3_class(
    simulate_choices(
      formula = choice ~ cost | income | time, N = 10, J = 2, T = 1:10,
      alternatives = c("car", "bus"), re = c("cost", "time"),
      true_parameter = RprobitB_parameter(C = 2)
    ),
    "RprobitB_data"
  )
})
