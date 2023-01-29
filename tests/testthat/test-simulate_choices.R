test_that("choice simulation works", {
  simulate_choices(
    formula = choice ~ cost, N = 10, J = 2, T = 1:10
  )
})
