test_that("log_likelihood_draws returns draws by independent likelihood unit", {
  model <- make_test_fit(data.frame(
    deciderID = rep(1:2, each = 2L),
    occasionID = rep(1:2, 2L),
    choice = c("A", "B", "B", "A"),
    x_A = c(0, 1, 1, 0),
    x_B = c(1, 0, 0, 1)
  ))

  mixed_model <- make_test_fit(
    data.frame(
      deciderID = rep(1:3, each = 3L),
      occasionID = rep(1:3, 3L),
      choice = c("A", "B", "B", "A", "A", "B", "B", "A", "A"),
      x_A = c(0, 1, 1, 0, 1, 0, 1, 0, 1),
      x_B = c(1, 0, 0, 1, 0, 1, 0, 1, 0)
    ),
    random_effects = "x"
  )

  result <- RprobitB:::log_likelihood_draws(model)
  simulated <- RprobitB:::log_likelihood_draws(mixed_model)
  precise <- RprobitB:::log_likelihood_draws(mixed_model, ghk_draws = 2000L)

  expect_identical(dim(result), c(2L, 10L))
  expect_identical(rownames(result), c("1", "2"))
  expect_match(colnames(result)[1L], "chain_1.iteration_1", fixed = TRUE)
  expect_true(all(is.finite(result)))
  expect_identical(dim(simulated), c(3L, 10L))
  expect_equal(simulated, precise, tolerance = 0.05)
})
