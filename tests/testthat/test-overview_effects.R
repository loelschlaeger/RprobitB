test_that("P", {
  form = choice ~ cost | income | time
  alternatives = c("bus","car")
  out = overview_effects(form = form, alternatives = alternatives)
  expect_snapshot(out)
})

test_that("MNP", {
  form = choice ~ cost | income | time
  alternatives = c("train","bus","car")
  out = overview_effects(form = form, alternatives = alternatives)
  expect_snapshot(out)
})

test_that("MMNP", {
  form = choice ~ cost | income | time
  re = c("cost","ASC")
  alternatives = c("train","bus","car")
  out = overview_effects(form = form, re = re, alternatives = alternatives)
  expect_snapshot(out)
})

