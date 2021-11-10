test_that("P", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 2,
                  alternatives = c("bus","car"),
                  seed = 1,
                  alpha = 1:5)
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("MNP", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 3,
                  alternatives = c("train","bus","car"),
                  seed = 1,
                  alpha = 1:8)
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})
