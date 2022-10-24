test_that("RprobitB_prior can be created and validated", {
  expect_s3_class(
    RprobitB_prior(formula = choice ~ A, J = 2),
    "RprobitB_prior"
  )
  expect_s3_class(
    RprobitB_prior(
      formula = choice ~ A | B + 0, re = "A", J = 4, C = 2,
      alpha_prior_mean = c(-10, 0, 10),
      s_prior_concentration = c(1, 2),
      b_prior_Sigma = matrix(2)
    ),
    "RprobitB_prior"
  )
  expect_s3_class(
    RprobitB_prior(
      formula = choice ~ A | B | C, re = "C", J = 3,
      alpha_prior_custom = function(x) {
        stats::dnorm(x[1], mean = 0, sd = 1) * dunif(x[2], min = 0, max = 1)
      }
    ),
    "RprobitB_prior"
  )
})

test_that("RprobitB_prior_alpha can be created and validated", {
  expect_equal(RprobitB_prior_alpha(P_f = 0), NA)
  expect_s3_class(RprobitB_prior_alpha(P_f = 2), "RprobitB_prior_alpha")
  expect_s3_class(RprobitB_prior_alpha(
    P_f = 2,
    alpha_prior_custom = function(x) {
      dnorm(x[1], mean = 0, sd = 1) * dunif(x[2], min = 0, max = 1)
    }
  ), "RprobitB_prior_alpha")
})

test_that("RprobitB_prior_s can be created and validated", {
  expect_equal(RprobitB_prior_s(C = 1), NA)
  expect_s3_class(RprobitB_prior_s(C = 2), "RprobitB_prior_s")
  expect_s3_class(RprobitB_prior_s(
      C = 2,
      s_prior_custom = function(x) {
        stats::dunif(x[1], min = 0.5, max = 1) * (x[2] == 1 - x[1])
      },
      s_prior_custom_test_par = c(0.6, 0.4)
    ), "RprobitB_prior_s")
})

test_that("RprobitB_prior_b can be created and validated", {
  expect_equal(RprobitB_prior_b(P_r = 0), NA)
  expect_s3_class(RprobitB_prior_b(P_r = 2), "RprobitB_prior_b")
  expect_s3_class(RprobitB_prior_b(
    P_r = 2,
    b_prior_custom = function(x) {
      dnorm(x[1], mean = 0, sd = 1) * dunif(x[2], min = 0, max = 1)
    }
  ), "RprobitB_prior_b")
})

test_that("RprobitB_prior_Omega can be created and validated", {
  expect_equal(RprobitB_prior_Omega(P_r = 0), NA)
  expect_s3_class(RprobitB_prior_Omega(P_r = 2), "RprobitB_prior_Omega")
  expect_s3_class(RprobitB_prior_Omega(
    P_r = 2,
    Omega_prior_custom = function(x) {
      dwishart(x, df = 4, scale = diag(2), inv = TRUE) * (x[1,1] == x[2,2])
    }
  ), "RprobitB_prior_Omega")
})

test_that("RprobitB_prior_Sigma can be created and validated", {
  expect_equal(RprobitB_prior_Sigma(ordered = FALSE, J = 3), NA)
  expect_s3_class(RprobitB_prior_Sigma(ordered = TRUE, J = 3), "RprobitB_prior_Sigma")
  expect_s3_class(RprobitB_prior_Sigma(
    ordered = TRUE,
    J = 3,
    Sigma_prior_custom = function(x) dunif(x)
  ), "RprobitB_prior_Sigma")
})

test_that("RprobitB_prior_Sigma_diff can be created and validated", {
  expect_equal(RprobitB_prior_Sigma_diff(ordered = TRUE, J = 3), NA)
  expect_s3_class(RprobitB_prior_Sigma_diff(J = 3), "RprobitB_prior_Sigma_diff")
  expect_s3_class(RprobitB_prior_Sigma_diff(
    J = 3,
    Sigma_diff_prior_custom = function(x) {
      dwishart(x, df = 4, scale = diag(2), inv = TRUE) *
      all(x[row(x) != col(x)] == 0)
    }
  ), "RprobitB_prior_Sigma_diff")
})

test_that("RprobitB_prior_d can be created and validated", {
  expect_equal(RprobitB_prior_d(ordered = FALSE, J = 4), NA)
  expect_s3_class(RprobitB_prior_d(ordered = TRUE, J = 4), "RprobitB_prior_d")
  expect_s3_class(RprobitB_prior_d(
    ordered = TRUE,
    J = 4,
    d_prior_custom = function(x) {
      dnorm(x[1], mean = 0, sd = 1) * dunif(x[2], min = 0, max = 1)
    }
  ), "RprobitB_prior_d")
})

