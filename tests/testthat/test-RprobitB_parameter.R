test_that("input checks for RprobitB_parameter() work", {
  expect_error(
    RprobitB_parameter(C = 3.1),
    "Input 'C' must be a positive `integer`."
  )
  expect_error(
    RprobitB_parameter(s = "not_a_numeric"),
    "Input 's' must be `numeric`."
  )
  expect_error(
    RprobitB_parameter(alpha = "not_a_numeric"),
    "Input 'alpha' must be `numeric`."
  )
  expect_error(
    RprobitB_parameter(b = "not_a_numeric"),
    "Input 'b' must be `numeric`."
  )
  expect_error(
    RprobitB_parameter(Omega = "not_a_numeric"),
    "Input 'Omega' must be `numeric`."
  )
  expect_error(
    RprobitB_parameter(Sigma = "not_a_numeric"),
    "Input 'Sigma' must be `numeric`."
  )
  expect_error(
    RprobitB_parameter(Sigma_diff = "not_a_numeric"),
    "Input 'Sigma_diff' must be `numeric`."
  )
  expect_error(
    RprobitB_parameter(diff_alt = 3.1),
    "Input 'diff_alt' must be a positive `integer`."
  )
  expect_error(
    RprobitB_parameter(beta = "not_a_numeric"),
    "Input 'beta' must be `numeric`."
  )
  expect_error(
    RprobitB_parameter(z = "not_a_numeric"),
    "Input 'z' must be `numeric`."
  )
  expect_error(
    RprobitB_parameter(d = "not_a_numeric"),
    "Input 'd' must be `numeric`."
  )
})

test_that("RprobitB_parameter can be created", {
  x <- RprobitB_parameter()
  expect_true(is.RprobitB_parameter(x))
  expect_s3_class(
    x, "RprobitB_parameter"
  )
  formula <- choice ~ A | B
  re <- "A"
  J <- 3
  N <- 100
  x <- simulate_RprobitB_parameter(
    x,
    formula = formula, re = re, J = J, N = N, seed = 1
  )
  expect_snapshot(print(x))
  expect_snapshot(print(x, "beta"))
  expect_error(
    print.RprobitB_parameter(1),
    "Input 'x' is not of class `RprobitB_parameter`."
  )
  expect_s3_class(
    x, "RprobitB_parameter"
  )
})

test_that("missing probit model parameters can be simulated", {
  expect_error(
    simulate_RprobitB_parameter(x = "bad_object"),
    "Input 'x' is not of class `RprobitB_parameter`."
  )
  expect_error(
    simulate_RprobitB_parameter(x = RprobitB_parameter()),
    "Please specify the input 'formula'."
  )
  expect_error(
    simulate_RprobitB_parameter(
      x = RprobitB_parameter(), formula = A ~ B
    ),
    "Please specify input 'J'."
  )
  expect_error(
    simulate_RprobitB_parameter(
      x = RprobitB_parameter(), formula = A ~ B, J = 3.1
    ),
    "Input 'J' must be a positive `integer`."
  )
  expect_error(
    simulate_RprobitB_parameter(
      x = RprobitB_parameter(), formula = A ~ B, J = 3
    ),
    "Please specify input 'N'."
  )
  expect_error(
    simulate_RprobitB_parameter(
      x = RprobitB_parameter(), formula = A ~ B, J = 3, N = 3.1
    ),
    "Input 'N' must be a positive `integer`."
  )
  expect_true(
    is.RprobitB_parameter(
      simulate_RprobitB_parameter(
        x = RprobitB_parameter(C = 2), formula = A ~ B, J = 3, N = 10
      )
    )
  )
  expect_true(
    is.RprobitB_parameter(
      simulate_RprobitB_parameter(
        x = RprobitB_parameter(C = 2), formula = A ~ B, J = 3, N = 10,
        ordered = TRUE
      )
    )
  )
  expect_true(
    is.RprobitB_parameter(
      simulate_RprobitB_parameter(
        x = RprobitB_parameter(C = 2, Sigma = diag(3)), formula = A ~ B, J = 3,
        N = 10, ordered = FALSE
      )
    )
  )
})

test_that("RprobitB_parameter can be validated", {
  expect_error(
    validate_RprobitB_parameter(x = "bad_object"),
    "Input 'x' is not of class `RprobitB_parameter`."
  )
  expect_error(
    validate_RprobitB_parameter(x = RprobitB_parameter()),
    "Please specify the input 'formula'."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(), formula = A ~ B
    ),
    "Please specify input 'J'."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(), formula = A ~ B, J = 3.1
    ),
    "Input 'J' must be a positive `integer`."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(), formula = A ~ B, J = 3
    ),
    "Please specify input 'N'."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(), formula = A ~ B, J = 3, N = 3.1
    ),
    "Input 'N' must be a positive `integer`."
  )
  expect_error(
    {
      x <- RprobitB_parameter()
      x$C <- 0
      validate_RprobitB_parameter(
        x = x, formula = A ~ B, J = 3, N = 10
      )
    },
    "'C' is expected to be a positive `integer`."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(C = 3, s = 1:3), formula = A ~ B, J = 3, N = 10
    ),
    "'s' is expected to be a descending `numeric` `vector` of length 3 which sums up to 1."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(alpha = 1:2), formula = A ~ B, J = 3, N = 10
    ),
    "'alpha' is expected to be a `numeric` `matrix` of dimension 3 x 1."
  )
  expect_s3_class(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        alpha = 1:3, Sigma = diag(3), Sigma_diff = diff_Sigma(diag(3), 1),
        z = rep(1, 10)
      ), formula = A ~ B, J = 3, N = 10
    ),
    "RprobitB_parameter"
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(), formula = A ~ B | 0, re = "B", J = 3, N = 10
    ),
    "'b' is expected to be a `numeric` `matrix` of dimension 1 x 1."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(b = 1), formula = A ~ B | 0, re = "B", J = 3, N = 10
    ),
    "'Omega' is expected to be a `numeric` `matrix` of dimension 1 x 1."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        C = 4, s = c(0.5, 0.3, 0.1, 0.1), b = 1:4, Omega = c(1, 1, -1, 2)
      ), formula = A ~ B | 0, re = "B", J = 3, N = 10
    ),
    "Column 3 in 'Omega' is expected to be a proper covariance matrix."
  )
  expect_s3_class(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        alpha = 1, b = 1, Omega = 1, Sigma = diag(3), Sigma_diff = diff_Sigma(diag(3), 1),
        beta = rep(1, 10), z = rep(1, 10)
      ), formula = A ~ A + B | 0, re = "B", J = 3, N = 10
    ),
    "RprobitB_parameter"
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        C = 1, b = 1:2, Omega = c(1, 0, 0, 1), Sigma = diag(3), Sigma_diff = diff_Sigma(diag(3), 1),
        beta = rep(1, 10), z = rep(1, 10), diff_alt = 4
      ), formula = A ~ 0 | B + 0, re = "B", J = 3, N = 10
    ),
    "'diff_alt' is expected to be a positive `integer` smaller or equal 3."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        b = 1, Omega = 1, Sigma = diag(3), Sigma_diff = diff_Sigma(diag(3), 1),
        beta = rep(1, 10), z = rep(1, 10)
      ), formula = A ~ B, re = "B", J = 3, N = 10, ordered = TRUE
    ),
    "'Sigma' is expected to be a `numeric` 1 x 1 `matrix` with a positive entry."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        b = 1, Omega = 1, Sigma = diag(2), Sigma_diff = diff_Sigma(diag(3), 1),
        beta = rep(1, 10), z = rep(1, 10)
      ), formula = A ~ B | 0, re = "B", J = 3, N = 10, ordered = FALSE
    ),
    "'Sigma' is expected to be a `numeric` `matrix` of dimension 3 x 3."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        b = 1, Omega = 1, Sigma = matrix(-1, 3, 3), Sigma_diff = diff_Sigma(diag(3), 1),
        beta = rep(1, 10), z = rep(1, 10)
      ), formula = A ~ B | 0, re = "B", J = 3, N = 10, ordered = FALSE
    ),
    "'Sigma' is expected to be a proper covariance matrix."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        b = 1, Omega = 1, Sigma = matrix(1, 3, 3), Sigma_diff = matrix(-1, 2, 2),
        beta = rep(1, 10), z = rep(1, 10)
      ), formula = A ~ B | 0, re = "B", J = 3, N = 10, ordered = FALSE
    ),
    "'Sigma_diff' is expected to be a proper covariance matrix."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        b = 1, Omega = 1, Sigma = matrix(1, 3, 3), Sigma_diff = matrix(1, 3, 3),
        beta = rep(1, 10), z = rep(1, 10)
      ), formula = A ~ B | 0, re = "B", J = 3, N = 10, ordered = FALSE
    ),
    "'Sigma_diff' is expected to be a `numeric` `matrix` of dimension 2 x 2."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        b = 1, Omega = 1, Sigma = matrix(1, 3, 3), Sigma_diff = matrix(1, 2, 2),
        beta = rep(1, 10), z = rep(1, 10)
      ), formula = A ~ B | 0, re = "B", J = 3, N = 10, ordered = FALSE
    ),
    "Differencing 'Sigma' with respect to alternative 1 is expected to yield 'Sigma_diff'."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        b = 1, Omega = 1, Sigma = diag(3), Sigma_diff = diff_Sigma(diag(3), 1)
      ), formula = A ~ B | 0, re = "B", J = 3, N = 10, ordered = FALSE
    ),
    "'beta' is expected to be a `numeric` `matrix` of dimension 1 x 10."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        b = 1:3, Omega = matrix(diag(3), 9, 1), Sigma = diag(3), Sigma_diff = diff_Sigma(diag(3), 1),
        beta = rep(1, 3)
      ), formula = A ~ 0 | 0 | B, re = "B", J = 3, N = 1, ordered = FALSE
    ),
    "'z' is expected to be an `integer` `vector` with values 1."
  )
  expect_error(
    validate_RprobitB_parameter(
      x = RprobitB_parameter(
        b = 1, Omega = 1, Sigma = 1,
        beta = rep(1, 10), z = rep(1, 10)
      ), formula = A ~ B, re = "B", J = 3, N = 10, ordered = TRUE
    ),
    "'d' is expected to be a `numeric` `vector` of length 1."
  )
})
