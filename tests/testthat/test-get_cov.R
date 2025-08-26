test_that("covariates of choice occasions can be extracted", {
  data <- simulate_choices(
    form = product ~ price,
    N = 10,
    T = 1:10,
    J = 3,
    ranked = TRUE
  )
  checkmate::expect_data_frame(get_cov(data, id = 2), nrows = 2)
})
