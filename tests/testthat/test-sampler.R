class_kernel_fixture <- function() {
  list(
    s = c(0.7, 0.3),
    beta = matrix(c(-1, 0, 1, 2), nrow = 2),
    z = c(1, 1),
    b = matrix(c(0, 0, 1, -1), nrow = 2),
    Omega = cbind(c(1, 0, 0, 1), c(1, 0, 0, 1))
  )
}

ordered_kernel_fixture <- function() {
  list(
    d = rep(0, 3),
    y = matrix(c(1, 2, 1, NA), nrow = 2),
    sys = matrix(c(0, 0, 0, NA), nrow = 2),
    Tvec = c(2, 1)
  )
}

test_that("sample_allocation samples a class label", {
  kernel <- getExportedValue("RprobitB", "sample_allocation")
  expect_identical(kernel(c(0, 1)), 2L)
})

test_that("update_s returns normalized class weights", {
  kernel <- getExportedValue("RprobitB", "update_s")
  weights <- kernel(0.5, c(4, 3, 2))
  expect_identical(dim(weights), c(3L, 1L))
  expect_equal(sum(weights), 1)
  expect_true(all(weights > 0))
})

test_that("update_z returns class allocations", {
  kernel <- getExportedValue("RprobitB", "update_z")
  allocation <- kernel(
    c(1, 0), matrix(c(0, 1), nrow = 1), matrix(c(0, 1), nrow = 1),
    matrix(c(1, 1), nrow = 1)
  )
  expect_identical(dim(allocation), c(2L, 1L))
  expect_equal(as.numeric(allocation), c(1, 1))
})

test_that("update_m returns class sizes", {
  kernel <- getExportedValue("RprobitB", "update_m")
  sizes <- kernel(4L, c(1, 1, 1, 2, 2, 3))
  non_empty_sizes <- kernel(4L, c(1, 1, 1, 2, 2, 3), TRUE)
  expect_identical(dim(sizes), c(4L, 1L))
  expect_equal(as.numeric(sizes), c(3, 2, 1, 0))
  expect_equal(as.numeric(non_empty_sizes), c(3, 2, 1, 1))
})

test_that("update_classes_wb reproduces the weight-based class moves", {
  kernel <- getExportedValue("RprobitB", "update_classes_wb")
  s <- c(0.7, 0.3)
  b <- matrix(c(1, 1, 1, -1), ncol = 2)
  Omega <- matrix(c(0.5, 0.3, 0.3, 0.5, 1, -0.1, -0.1, 0.8), ncol = 2)

  unchanged <- kernel(s, b, Omega)
  removed <- kernel(s, b, Omega, epsmin = 0.31)
  split <- kernel(s, b, Omega, epsmax = 0.69)
  merged <- kernel(s, b, Omega, deltamin = 3)

  expect_identical(unchanged$update_type, 0L)
  expect_identical(removed$update_type, 1L)
  expect_identical(split$update_type, 2L)
  expect_identical(merged$update_type, 3L)
  expect_identical(ncol(removed$b), 1L)
  expect_identical(ncol(split$b), 3L)
  expect_identical(ncol(merged$b), 1L)
  expect_equal(sum(split$s), 1)
})

test_that("update_classes_dp returns a bounded class update", {
  kernel <- getExportedValue("RprobitB", "update_classes_dp")
  set.seed(1)
  result <- kernel(
    beta = matrix(c(-1, -0.5, 0.5, 1), nrow = 1),
    z = c(1, 1, 2, 2),
    b = matrix(c(-0.5, 0.5), nrow = 1),
    Omega = matrix(c(1, 1), nrow = 1),
    delta = 1, mu_b_0 = 0, Sigma_b_0 = matrix(1),
    n_Omega_0 = 3L, V_Omega_0 = matrix(1), Cmax = 3L
  )
  expect_named(result, c("z", "b", "Omega", "C"))
  expect_true(result$C >= 1L && result$C <= 3L)
  expect_identical(dim(result$z), c(4L, 1L))
})

test_that("update_b_c returns a finite class mean", {
  kernel <- getExportedValue("RprobitB", "update_b_c")
  set.seed(1)
  result <- kernel(0, matrix(1), 2L, matrix(1), 0)
  expect_identical(dim(result), c(1L, 1L))
  expect_true(all(is.finite(result)))
})

test_that("update_b returns class means", {
  kernel <- getExportedValue("RprobitB", "update_b")
  set.seed(1)
  result <- kernel(
    matrix(c(-1, 1), nrow = 1), matrix(c(1, 1), nrow = 1),
    c(1, 2), c(1, 1), matrix(1), 0
  )
  expect_identical(dim(result), c(1L, 2L))
  expect_true(all(is.finite(result)))
})

test_that("update_Omega_c returns a covariance matrix", {
  kernel <- getExportedValue("RprobitB", "update_Omega_c")
  set.seed(1)
  result <- kernel(matrix(1), 2L, 3L, matrix(1), TRUE)
  expect_identical(dim(result), c(1L, 1L))
  expect_gt(result[1, 1], 0)
})

test_that("update_Omega returns class covariances", {
  kernel <- getExportedValue("RprobitB", "update_Omega")
  set.seed(1)
  result <- kernel(
    matrix(c(-1, 1), nrow = 1), matrix(c(-0.5, 0.5), nrow = 1),
    c(1, 2), c(1, 1), 3L, matrix(1), TRUE
  )
  expect_identical(dim(result), c(1L, 2L))
  expect_true(all(result > 0))
})

test_that("update_coefficient returns a coefficient draw", {
  kernel <- getExportedValue("RprobitB", "update_coefficient")
  set.seed(1)
  result <- kernel(c(0, 0), diag(2), diag(2), c(0, 0))
  expect_identical(dim(result), c(2L, 1L))
  expect_true(all(is.finite(result)))
})

test_that("update_Sigma returns a covariance draw", {
  kernel <- getExportedValue("RprobitB", "update_Sigma")
  set.seed(1)
  result <- kernel(4L, diag(2), 2L, diag(2))
  expect_identical(dim(result), c(2L, 2L))
  expect_equal(result, t(result))
})

test_that("update_U returns finite latent utilities", {
  kernel <- getExportedValue("RprobitB", "update_U")
  set.seed(1)
  result <- kernel(c(0, 0), 1L, c(0, 0), diag(2), rep(TRUE, 3))
  expect_identical(dim(result), c(2L, 1L))
  expect_true(all(is.finite(result)))
})

test_that("update_U imputes utilities of unavailable alternatives", {
  kernel <- getExportedValue("RprobitB", "update_U")
  set.seed(1)
  draws <- replicate(200, as.numeric(kernel(
    c(0, 0), y = 1, sys = c(0, 0), Sigma_inv = diag(2),
    available = c(TRUE, FALSE, TRUE)
  )))
  expect_true(all(draws[1, ] > 0))
  expect_true(any(draws[2, ] > draws[1, ]))
})

test_that("update_U_ranked returns ranked utilities", {
  kernel <- getExportedValue("RprobitB", "update_U_ranked")
  set.seed(1)
  result <- kernel(c(0, 0), c(0, 0), diag(2))
  expect_identical(dim(result), c(2L, 1L))
  expect_true(all(result <= 0))
})

test_that("d_to_gamma returns ordered thresholds", {
  kernel <- getExportedValue("RprobitB", "d_to_gamma")
  thresholds <- kernel(c(0, log(2)))
  expect_identical(dim(thresholds), c(5L, 1L))
  expect_equal(as.numeric(thresholds), c(-Inf, 0, 1, 3, Inf))
})

test_that("log_likelihood_ordered returns a log-likelihood", {
  kernel <- getExportedValue("RprobitB", "log_likelihood_ordered")
  fixture <- ordered_kernel_fixture()
  result <- kernel(fixture$d, fixture$y, fixture$sys, fixture$Tvec)
  expect_true(is.finite(result))
  expect_true(result > -2.5 && result < -2.4)
})

test_that("update_d returns a threshold update", {
  kernel <- getExportedValue("RprobitB", "update_d")
  fixture <- ordered_kernel_fixture()
  set.seed(1)
  result <- kernel(
    fixture$d, fixture$y, fixture$sys, rep(0, 3), diag(3), fixture$Tvec,
    step_scale = rep(0.1, 3)
  )
  expect_identical(dim(result), c(3L, 1L))
  expect_true(all(is.finite(result)))
  expect_error(
    kernel(
      fixture$d, fixture$y, fixture$sys, rep(0, 3), diag(3), fixture$Tvec,
      step_scale = 0.1
    ),
    "one entry per log-increment"
  )
})
