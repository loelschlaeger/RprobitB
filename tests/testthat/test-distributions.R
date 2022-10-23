test_that("Dirichlet density can be computed", {
  x <- c(0.5,0.3,0.2)
  concentration <- 1:3
  factor <- 1000
  expect_equal(
    round(ddirichlet(x = x, concentration = concentration, log = FALSE) * factor) / factor,
    0.72
  )
  expect_equal(
    round(ddirichlet(x = x, concentration = concentration, log = TRUE) * factor) / factor,
    -0.329
  )
  expect_error(
    ddirichlet(x = c(0.5, 0.3, 0.3), concentration = concentration)
  )
})

test_that("Multivariate normal density can be computed", {
  x <- c(0,0)
  mean <- c(0,0)
  Sigma <- diag(2)
  factor <- 1000
  expect_equal(
    round(dmvnorm(x = x, mean = mean, Sigma = Sigma, log = FALSE) * factor) / factor,
    0.159
  )
  expect_equal(
    round(dmvnorm(x = x, mean = mean, Sigma = Sigma, log = TRUE) * factor) / factor,
    -1.838
  )
  expect_error(
    dmvnorm(x = 1:2, mean = 1:3, Sigma = diag(4))
  )
})

test_that("(Inverse-) Wishart density can be computed", {
  x <- diag(2)
  df <- 4
  scale <- matrix(c(1,0.2,0.2,2), 2, 2)
  factor <- 1000
  expect_equal(
    round(dwishart(x = x, df = df, scale = scale, log = FALSE, inv = FALSE) * factor) / factor,
    0.005
  )
  expect_equal(
    round(dwishart(x = x, df = df, scale = scale, log = TRUE, inv = FALSE) * factor) / factor,
    -5.335
  )
  expect_equal(
    round(dwishart(x = x, df = df, scale = scale, log = FALSE, inv = TRUE) * factor) / factor,
    0.034
  )
  expect_equal(
    round(dwishart(x = x, df = df, scale = scale, log = TRUE, inv = TRUE) * factor) / factor,
    -3.378
  )
  expect_error(
    dmvnorm(dwishart(x = x, df = 1, scale = scale))
  )
})
