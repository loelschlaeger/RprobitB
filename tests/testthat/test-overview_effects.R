test_that("P", {
  form = choice ~ cost | income | time
  alternatives = c("bus","car")
  out = overview_effects(form = form, alternatives = alternatives)
  expect_snapshot(out)
})
