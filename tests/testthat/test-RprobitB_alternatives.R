test_that("RprobitB_alternatives can be specified and validated", {
  expect_error(
    RprobitB_alternatives(alternatives = "B")
  )
  expect_error(
    RprobitB_alternatives(alternatives = c("A","B"), base = "C")
  )
  expect_s3_class(
    RprobitB_alternatives(alternatives = c("A","B","C"), base = "C"),
    "RprobitB_alternatives"
  )
  expect_s3_class(
    RprobitB_alternatives(alternatives = c("A","B","C"), base = "C", ordered = TRUE),
    "RprobitB_alternatives"
  )
})
