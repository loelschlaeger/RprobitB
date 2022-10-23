test_that("RprobitB_prior can be created and validated", {
  # TODO
})

test_that("RprobitB_prior_alpha can be created and validated", {
  expect_s3_class(RprobitB_prior_alpha(P_f = 2), "RprobitB_prior_alpha")
  expect_s3_class(RprobitB_prior_alpha(
    P_f = 2,
    alpha_prior_custom = function(x) {
      dnorm(x[1], mean = 0, sd = 1) * dunif(x[2], min = 0, max = 1)
    }
  ), "RprobitB_prior_alpha")
})

test_that("RprobitB_prior_s can be created and validated", {
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
  expect_s3_class(RprobitB_prior_b(P_r = 2), "RprobitB_prior_b")
  expect_s3_class(RprobitB_prior_b(
    P_r = 2,
    b_prior_custom = function(x) {
      dnorm(x[1], mean = 0, sd = 1) * dunif(x[2], min = 0, max = 1)
    }
  ), "RprobitB_prior_b")
})
