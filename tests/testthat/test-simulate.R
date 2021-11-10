test_that("P", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 2,
                  alternatives = c("bus","car"),
                  seed = 1,
                  alpha = 1:5)
  expect_snapshot(data$data)
  expect_snapshot(data$choice_data)
  expect_snapshot(data$true_parameter)
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})
