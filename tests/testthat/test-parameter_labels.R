test_that("creation of labels works", {
  P_f <- sample(0:10, 1)
  P_r <- sample(0:10, 1)
  J <- sample(2:10, 1)
  C <- sample(1:5, 1)
  cov_sym <- sample(c(TRUE, FALSE), 1)
  drop_par <- if (runif(1) < 0.5) NULL else sample(c("alpha", "s", "b", "Omega", "Sigma"), sample(1:5, 1))
  out <- parameter_labels(
    P_f = P_f, P_r = P_r, J = J, C = C, cov_sym = cov_sym,
    drop_par = drop_par
  )
  expect_setequal(names(out), setdiff(c(if (P_f > 0) "alpha", if (P_r > 0) c("s", "b", "Omega"), "Sigma"), drop_par))
})
