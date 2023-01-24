test_that("RprobitB_alternatives can be specified and validated", {
  expect_error(
    RprobitB_alternatives(),
    "Please specify the input 'J'."
  )
  expect_error(
    RprobitB_alternatives(J = pi),
    "It should be an `integer`, the number of choice alternatives."
  )
  expect_error(
    RprobitB_alternatives(J = 2, alternatives = diag(2)),
    "Input 'alternatives' should be a `vector`."
  )
  expect_error(
    RprobitB_alternatives(J = 3, alternatives = 1:3),
    "It should be a `character` vector, the names of the choice alternatives."
  )
  expect_error(
    RprobitB_alternatives(J = 1),
    "Input 'alternatives' is misspecified."
  )
  expect_error(
    RprobitB_alternatives(J = 3, alternatives = c("1", "2")),
    "Input 'alternatives' must be of length 'J = 3'."
  )
  expect_error(
    RprobitB_alternatives(J = 2, alternatives = c("same", "same")),
    "Alternatives must be unqiue."
  )
  expect_error(
    RprobitB_alternatives(J = 2, base = "C"),
    "Base alternative must be in alternative set."
  )
  expect_error(
    RprobitB_alternatives(J = 2, alternatives = c("A", "B"), base = c("A", "B")),
    "Input 'base' is misspecified."
  )
  expect_s3_class(
    RprobitB_alternatives(J = 3, base = "C"),
    "RprobitB_alternatives"
  )
  expect_true(
    is.RprobitB_alternatives(RprobitB_alternatives(J = 3, base = "C"))
  )
  expect_error(
    RprobitB_alternatives(J = 3, ordered = "not_a_logical"),
    "Input 'ordered' must be `TRUE` or `FALSE`."
  )
  expect_s3_class(
    RprobitB_alternatives(J = 3, ordered = TRUE),
    "RprobitB_alternatives"
  )
  expect_true(
    is.RprobitB_alternatives(RprobitB_alternatives(J = 3, ordered = TRUE))
  )
  expect_error(
    RprobitB_alternatives(J = 2, ordered = TRUE),
    "At least 3 alternatives are required in the ordered case."
  )
  expect_s3_class(
    RprobitB_alternatives(J = 3, alternatives = c("la", "le", "lu")),
    "RprobitB_alternatives"
  )
})

test_that("RprobitB_alternatives can be printed", {
  expect_error(
    print.RprobitB_alternatives(1),
    "Input 'x' is not of class `RprobitB_alternatives`."
  )
  expect_snapshot(
    RprobitB_alternatives(J = 3, alternatives = c("la", "le", "lu"))
  )
  expect_snapshot(
    RprobitB_alternatives(J = 4, ordered = TRUE)
  )
})
