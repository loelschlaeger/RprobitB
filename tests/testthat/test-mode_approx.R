test_that("mode can be calculated", {
  samples <- oeli::rmixnorm(
    n = 1e5, mean = matrix(c(-2, 2), ncol = 2),
    Sigma = matrix(c(1, 1), ncol = 2), proportions = c(0.7, 0.3)
  )
  expect_equal(mode_approx(samples), -2, tolerance = 0.5)
})

