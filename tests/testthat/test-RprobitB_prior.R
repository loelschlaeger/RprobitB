test_that("RprobitB_prior can be created and validated", {
  expect_error(
    RprobitB_prior(),
    "Please specify the input 'formula'."
  )
  expect_error(
    RprobitB_prior(formula = A ~ B),
    "Please specify the input 'J'."
  )
  expect_error(
    RprobitB_prior(formula = A ~ B, J = 3, C = 0),
    "Input 'C' is misspecified."
  )
  expect_s3_class(
    RprobitB_prior(formula = choice ~ A, J = 2),
    "RprobitB_prior"
  )
  expect_true(
    is.RprobitB_prior(RprobitB_prior(formula = choice ~ A, J = 2))
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
  expect_error(
    print.RprobitB_prior(1),
    "Input 'x' is not of class `RprobitB_prior`."
  )
  expect_snapshot(print(RprobitB_prior(formula = A ~ B, J = 4)))
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
  expect_error(
    RprobitB_prior_alpha(P_f = 3, alpha_prior_custom = "no_function"),
    "Custom prior for 'alpha' is misspecified."
  )
  expect_error(
    RprobitB_prior_alpha(
      P_f = 3, alpha_prior_custom = function(x) 1,
      alpha_prior_custom_test_par = 1:4
    ),
    "'alpha_prior_custom_test_par' should be a `numeric` vector of length 3."
  )
  expect_error(
    RprobitB_prior_alpha(
      P_f = 3, alpha_prior_custom = function(x) 1:2,
      alpha_prior_custom_test_par = 1:3
    ),
    "Custom prior for 'alpha' is misspecified."
  )
  expect_error(
    RprobitB_prior_alpha(P_f = 3, alpha_prior_mean = "1"),
    "'alpha_prior_mean' should be a `numeric` vector."
  )
  expect_error(
    RprobitB_prior_alpha(P_f = 3, alpha_prior_mean = 1:2),
    "'alpha_prior_mean' should have length 3."
  )
  expect_error(
    RprobitB_prior_alpha(P_f = 3, alpha_prior_Sigma = "1"),
    "It is not a proper covariance matrix."
  )
  expect_error(
    RprobitB_prior_alpha(P_f = 3, alpha_prior_Sigma = diag(2)),
    "'alpha_prior_Sigma' should have dimension 3."
  )
  expect_error(
    print.RprobitB_prior_alpha(1),
    "Input 'x' is not of class `RprobitB_prior_alpha`."
  )
  expect_snapshot(RprobitB_prior_alpha(P_f = 3))
  expect_snapshot(
    RprobitB_prior_alpha(
      P_f = 2,
      alpha_prior_custom = function(x) {
        stats::dnorm(x[1]) * stats::dunif(x[2])
      }
    )
  )
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
  expect_error(
    RprobitB_prior_s(C = 3, s_prior_custom = "no_function"),
    "Custom prior for 's' is misspecified."
  )
  expect_error(
    RprobitB_prior_alpha(P_f = 3, alpha_prior_custom = "no_function"),
    "Custom prior for 'alpha' is misspecified."
  )
  expect_error(
    RprobitB_prior_s(
      C = 3, s_prior_custom = function(x) 1,
      s_prior_custom_test_par = 1:4
    ),
    "'s_prior_custom_test_par' should be a `numeric` vector of length 3."
  )
  expect_error(
    RprobitB_prior_s(
      C = 3, s_prior_custom = function(x) 1:2,
      s_prior_custom_test_par = 1:3
    ),
    "Custom prior for 's' is misspecified."
  )
  expect_error(
    RprobitB_prior_s(C = 3, s_prior_concentration = "1"),
    "'s_prior_concentration' should be a `numeric` vector."
  )
  expect_error(
    RprobitB_prior_s(C = 4, s_prior_concentration = 1:5),
    "'s_prior_concentration' should have length 4."
  )
  expect_error(
    print.RprobitB_prior_s(1),
    "Input 'x' is not of class `RprobitB_prior_s`."
  )
  expect_snapshot(RprobitB_prior_s(C = 2))
  expect_snapshot(
    RprobitB_prior_s(
      C = 2,
      s_prior_custom = function(x) {
        stats::dunif(x[1], min = 0.5, max = 1) * (x[2] == 1 - x[1])
      },
      s_prior_custom_test_par = c(0.6, 0.4)
    )
  )
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
  expect_error(
    RprobitB_prior_b(P_r = 3, b_prior_custom = "no_function"),
    "Custom prior for 'b' is misspecified."
  )
  expect_error(
    RprobitB_prior_b(
      P_r = 3, b_prior_custom = function(x) 1,
      b_prior_custom_test_par = 1:4
    ),
    "'b_prior_custom_test_par' should be a `numeric` vector of length 3."
  )
  expect_error(
    RprobitB_prior_b(
      P_r = 3, b_prior_custom = function(x) 1:2,
      b_prior_custom_test_par = 1:3
    ),
    "Custom prior for 'b' is misspecified."
  )
  expect_error(
    RprobitB_prior_b(P_r = 3, b_prior_mean = "1"),
    "'b_prior_mean' should be a `numeric` vector."
  )
  expect_error(
    RprobitB_prior_b(P_r = 3, b_prior_mean = 1:2),
    "'b_prior_mean' should have length 3."
  )
  expect_error(
    RprobitB_prior_b(P_r = 3, b_prior_Sigma = "1"),
    "It is not a proper covariance matrix."
  )
  expect_error(
    RprobitB_prior_b(P_r = 3, b_prior_Sigma = diag(2)),
    "'b_prior_Sigma' should have dimension 3."
  )
  expect_error(
    print.RprobitB_prior_b(1),
    "Input 'x' is not of class `RprobitB_prior_b`."
  )
  expect_snapshot(RprobitB_prior_b(P_r = 3))
  expect_snapshot(
    RprobitB_prior_b(
      P_r = 2,
      b_prior_custom = function(x) {
        stats::dnorm(x[1]) * stats::dunif(x[2])
      }
    )
  )
})

test_that("RprobitB_prior_Omega can be created and validated", {
  expect_equal(RprobitB_prior_Omega(P_r = 0), NA)
  expect_s3_class(RprobitB_prior_Omega(P_r = 2), "RprobitB_prior_Omega")
  expect_s3_class(RprobitB_prior_Omega(
    P_r = 2,
    Omega_prior_custom = function(x) {
      dwishart(x, df = 4, scale = diag(2), inv = TRUE) * (x[1, 1] == x[2, 2])
    }
  ), "RprobitB_prior_Omega")
  expect_error(
    RprobitB_prior_Omega(P_r = 3, Omega_prior_custom = "no_function"),
    "Custom prior for 'Omega' is misspecified."
  )
  expect_error(
    RprobitB_prior_Omega(
      P_r = 3, Omega_prior_custom = function(x) 1,
      Omega_prior_custom_test_par = 1:3
    ),
    "'Omega_prior_custom_test_par' should be a `matrix`"
  )
  expect_error(
    RprobitB_prior_Omega(
      P_r = 3, Omega_prior_custom = function(x) 1:2,
      Omega_prior_custom_test_par = diag(3)
    ),
    "Custom prior for 'Omega' is misspecified."
  )
  expect_error(
    RprobitB_prior_Omega(P_r = 3, Omega_prior_df = "1"),
    "'Omega_prior_df' should be a positive `integer`."
  )
  expect_error(
    RprobitB_prior_Omega(P_r = 3, Omega_prior_df = 1),
    "'Omega_prior_df' should be greater or equal 5."
  )
  expect_error(
    RprobitB_prior_Omega(P_r = 3, Omega_prior_scale = "1"),
    "It is not a proper covariance matrix."
  )
  expect_error(
    RprobitB_prior_Omega(P_r = 3, Omega_prior_scale = diag(2)),
    "'Omega_prior_scale' should have dimension 3."
  )
  expect_error(
    print.RprobitB_prior_Omega(1),
    "Input 'x' is not of class `RprobitB_prior_Omega`."
  )
  expect_snapshot(RprobitB_prior_Omega(P_r = 2))
  expect_snapshot(
    RprobitB_prior_Omega(
      P_r = 2,
      Omega_prior_custom = function(x) {
        dwishart(x, df = 4, scale = diag(2), inv = TRUE) * (x[1, 1] == x[2, 2])
      }
    )
  )
})

test_that("RprobitB_prior_Sigma can be created and validated", {
  expect_equal(RprobitB_prior_Sigma(ordered = FALSE, J = 3), NA)
  expect_s3_class(RprobitB_prior_Sigma(ordered = TRUE, J = 3), "RprobitB_prior_Sigma")
  expect_s3_class(RprobitB_prior_Sigma(
    ordered = TRUE,
    J = 3,
    Sigma_prior_custom = function(x) dunif(x)
  ), "RprobitB_prior_Sigma")
  expect_error(
    RprobitB_prior_Sigma(ordered = TRUE, J = 3, Sigma_prior_custom = "no_function"),
    "Custom prior for 'Sigma' is misspecified."
  )
  expect_error(
    RprobitB_prior_Sigma(
      ordered = TRUE, J = 3, Sigma_prior_custom = function(x) 1,
      Sigma_prior_custom_test_par = 1:3
    ),
    "'Sigma_prior_custom_test_par' should be a `matrix`"
  )
  expect_error(
    RprobitB_prior_Sigma(
      ordered = TRUE, J = 3, Sigma_prior_custom = function(x) 1:2,
      Sigma_prior_custom_test_par = matrix(1)
    ),
    "Custom prior for 'Sigma' is misspecified."
  )
  expect_error(
    RprobitB_prior_Sigma(ordered = TRUE, J = 3, Sigma_prior_df = "1"),
    "'Sigma_prior_df' should be a positive `integer`."
  )
  expect_error(
    RprobitB_prior_Sigma(ordered = TRUE, J = 3, Sigma_prior_df = 1),
    "'Sigma_prior_df' should be greater or equal 3."
  )
  expect_error(
    RprobitB_prior_Sigma(ordered = TRUE, J = 3, Sigma_prior_scale = "1"),
    "It is not a proper covariance matrix."
  )
  expect_error(
    RprobitB_prior_Sigma(ordered = TRUE, J = 3, Sigma_prior_scale = diag(3)),
    "'Sigma_prior_scale' should have dimension 1."
  )
  expect_error(
    print.RprobitB_prior_Sigma(1),
    "Input 'x' is not of class `RprobitB_prior_Sigma`."
  )
  expect_snapshot(RprobitB_prior_Sigma(J = 4, ordered = TRUE))
  expect_snapshot(
    RprobitB_prior_Sigma(
      ordered = TRUE,
      J = 4,
      Sigma_prior_custom = function(x) dunif(x)
    )
  )
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
  expect_error(
    RprobitB_prior_Sigma_diff(ordered = FALSE, J = 3, Sigma_diff_prior_custom = "no_function"),
    "Custom prior for 'Sigma_diff' is misspecified."
  )
  expect_error(
    RprobitB_prior_Sigma_diff(
      ordered = FALSE, J = 3, Sigma_diff_prior_custom = function(x) 1,
      Sigma_diff_prior_custom_test_par = 1:3
    ),
    "'Sigma_diff_prior_custom_test_par' should be a `matrix`"
  )
  expect_error(
    RprobitB_prior_Sigma_diff(
      ordered = FALSE, J = 3, Sigma_diff_prior_custom = function(x) 1:2,
      Sigma_diff_prior_custom_test_par = diag(2)
    ),
    "Custom prior for 'Sigma_diff' is misspecified."
  )
  expect_error(
    RprobitB_prior_Sigma_diff(ordered = FALSE, J = 3, Sigma_diff_prior_df = "1"),
    "'Sigma_diff_prior_df' should be a positive `integer`."
  )
  expect_error(
    RprobitB_prior_Sigma_diff(ordered = FALSE, J = 3, Sigma_diff_prior_df = 1),
    "'Sigma_diff_prior_df' should be greater or equal 4."
  )
  expect_error(
    RprobitB_prior_Sigma_diff(ordered = FALSE, J = 3, Sigma_diff_prior_scale = "1"),
    "It is not a proper covariance matrix."
  )
  expect_error(
    RprobitB_prior_Sigma_diff(ordered = FALSE, J = 3, Sigma_diff_prior_scale = diag(3)),
    "'Sigma_diff_prior_scale' should have dimension 2."
  )
  expect_error(
    print.RprobitB_prior_Sigma_diff(1),
    "Input 'x' is not of class `RprobitB_prior_Sigma_diff`."
  )
  expect_snapshot(RprobitB_prior_Sigma_diff(J = 4, ordered = FALSE))
  expect_snapshot(
    RprobitB_prior_Sigma_diff(
      J = 3,
      Sigma_diff_prior_custom = function(x) {
        dwishart(x, df = 4, scale = diag(2), inv = TRUE) *
          all(x[row(x) != col(x)] == 0)
      }
    )
  )
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
  expect_error(
    RprobitB_prior_d(ordered = TRUE, J = 3, d_prior_custom = "no_function"),
    "Custom prior for 'd' is misspecified."
  )
  expect_error(
    RprobitB_prior_d(
      ordered = TRUE, J = 3, d_prior_custom = function(x) 1,
      d_prior_custom_test_par = 1:2
    ),
    "'d_prior_custom_test_par' should be a `numeric` vector of length 1."
  )
  expect_error(
    RprobitB_prior_d(
      ordered = TRUE, J = 3, d_prior_custom = function(x) 1:2,
      d_prior_custom_test_par = 1
    ),
    "Custom prior for 'd' is misspecified."
  )
  expect_error(
    RprobitB_prior_d(ordered = TRUE, J = 3, d_prior_mean = "1"),
    "'d_prior_mean' should be a `numeric` vector."
  )
  expect_error(
    RprobitB_prior_d(ordered = TRUE, J = 4, d_prior_mean = 1:3),
    "'d_prior_mean' should have length 2."
  )
  expect_error(
    RprobitB_prior_d(ordered = TRUE, J = 4, d_prior_Sigma = "1"),
    "It is not a proper covariance matrix."
  )
  expect_error(
    RprobitB_prior_d(ordered = TRUE, J = 4, d_prior_Sigma = diag(3)),
    "'d_prior_Sigma' should have dimension 2."
  )
  expect_error(
    print.RprobitB_prior_d(1),
    "Input 'x' is not of class `RprobitB_prior_d`."
  )
  expect_snapshot(RprobitB_prior_d(J = 4, ordered = TRUE))
  expect_snapshot(
    RprobitB_prior_d(
      ordered = TRUE,
      J = 4,
      d_prior_custom = function(x) {
        stats::dnorm(x[1]) * stats::dunif(x[2])
      }
    )
  )
})
