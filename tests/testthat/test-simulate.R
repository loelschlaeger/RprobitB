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

test_that("P_train_test", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 2,
                  alternatives = c("bus","car"),
                  test_prop = 0.2,
                  seed = 1,
                  alpha = 1:5)
  expect_type(data, "list")
  expect_snapshot(print(data))
  for(i in 1:2){
    expect_snapshot(print(data[[i]]))
    expect_snapshot(summary(data[[i]]))
  }
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

test_that("MNP_train_test", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 3,
                  alternatives = c("train","bus","car"),
                  test_prop = 0.7,
                  seed = 1,
                  alpha = 1:8)
  expect_type(data, "list")
  expect_snapshot(print(data))
  for(i in 1:2){
    expect_snapshot(print(data[[i]]))
    expect_snapshot(summary(data[[i]]))
  }
})

test_that("MMNP", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 3,
                  re = c("cost","ASC"),
                  alternatives = c("train","bus","car"),
                  seed = 1,
                  alpha = 1:5, b = 1:3, Omega = as.numeric(diag(3)),
                  Sigma = diag(2))
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("LCMMNP", {
  data = simulate(form = choice ~ cost | income | time,
                  N = 10,
                  T = 1:10,
                  J = 3,
                  re = c("cost","ASC"),
                  alternatives = c("train","bus","car"),
                  seed = 1,
                  C = 2)
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})
