test_that("parameter checks work", {
  P_f <- sample(0:10, 1)
  P_r <- sample(0:10, 1)
  J <- sample(2:10, 1)
  N <- sample(1:1000, 1)
  parm <- RprobitB_parameter(P_f = P_f, P_r = P_r, J = J, N = N)
  if (P_f > 0) {
    expect_length(parm$alpha, P_f)
  } else {
    expect_equal(parm$alpha, NA)
  }
  if (P_r > 0) {
    expect_true(is.numeric(parm$C))
    expect_true(parm$C %% 1 == 0)
    expect_true(parm$C >= 0)
  } else {
    expect_equal(parm$C, NA)
  }
})

test_that("parameter reproducibility works", {
  x <- RprobitB_parameter(P_f = 2, P_r = 2, J = 2, N = 100, seed = 1)
  expect_snapshot(unclass(x))
})

test_that("parameter printing works", {
  x <- RprobitB_parameter(P_f = 1, P_r = 1, J = 2, N = 100, seed = 1)
  expect_snapshot(x)
})
