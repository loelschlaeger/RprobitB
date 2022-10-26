test_that("RprobitB_alternatives can be specified and validated", {
  expect_error(
    RprobitB_alternatives(J = 1)
  )
  expect_error(
    RprobitB_alternatives(J = 2, base = "C")
  )
  expect_s3_class(
    RprobitB_alternatives(J = 3, base = "C"),
    "RprobitB_alternatives"
  )
  expect_s3_class(
    RprobitB_alternatives(J = 3, ordered = TRUE),
    "RprobitB_alternatives"
  )
  expect_error(
    RprobitB_alternatives(J = 2, ordered = TRUE)
  )
  expect_s3_class(
    RprobitB_alternatives(J = 3, alternatives = c("la", "le", "lu")),
    "RprobitB_alternatives"
  )
})
