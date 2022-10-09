test_that("RprobitB_effects can be created", {
  # TODO
  expect_equal(
    RprobitB_effects(
       formula = choice ~ cov,
       re = "cov+",
       alternatives = c("A", "B")
    ),
    data.frame(matrix(c("cov", FALSE, FALSE, TRUE, TRUE), nrow = 1))
  )
  expect_equal(
    RprobitB_effects(
      formula = choice ~ price + time + comfort + change | 1,
      re = c("price+", "time"),
      alternatives = c("A", "B"),
      base = "A"
    ),
    data.frame()
  )
})
