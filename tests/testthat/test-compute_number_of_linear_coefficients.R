test_that("computation of number of linear coefficients works", {
  vars = list("price", character(0), c("time","comfort","change"))
  ASC = TRUE
  J = 2
  re = c("price")
  expect_equal(
    compute_number_of_linear_coefficients(vars = vars, ASC = ASC, J = J, re = re),
    list("P_f" = 7, "P_r" = 1))
})
