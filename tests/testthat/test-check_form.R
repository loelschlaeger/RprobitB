test_that("formula is correctly interpreted", {
  form = choice ~ price | 0 | time + comfort + change
  re = c("price","time")
  out = check_form(form = form, re = re)
  expect_type(out, "list")
  expect_length(out, 4)
  expect_equal(names(out), c("choice","re","vars","ASC"))
  expect_equal(out$choice, "choice")
  expect_equal(out$re, re)
  expect_equal(out$vars,
               list("price", character(0), c("time","comfort","change")))
  expect_false(out$ASC)
  expect_warning(
    expect_equal(
      check_form(form = form, re = c("price","time","bug"))$re,
      c("price","time")),
    "The covariate 'bug' in 're' is not part of 'form' and hence ignored.")
  expect_identical(check_form(form = form, re = NULL)$re, NULL)
  expect_true(check_form(form = choice ~ price | 1 | time + comfort + change)$ASC)
})
