

test_that("sampling and checking covariance matrix works", {
  expect_true(is_covariance_matrix(diag(10)))
  x <- sample_covariance_matrix(dim = 3)
  expect_true(is_covariance_matrix(x))
})

test_that("Extraction of function body as character works", {
  test_fun <- function(x) {
    stopifnot(is.numeric(x))
    {x + 1}
  }
  expect_equal(
    function_body(test_fun),
    "stopifnot(is.numeric(x)) { x + 1 }")
  expect_equal(
    function_body(test_fun, braces = TRUE),
    "{ stopifnot(is.numeric(x)) { x + 1 } }")
  expect_equal(
    function_body(test_fun, nchar = 20),
    "stopifnot(is.nume..."
  )
})

test_that("computation of permutations works", {
  expect_equal(
    permutations(1:3),
    list(1:3, c(1L, 3L, 2L), c(2L, 1L, 3L), c(2L, 3L, 1L), c(3L, 1L, 2L), 3:1)
  )
  expect_equal(
    permutations(LETTERS[1:3]),
    list(c("A", "B", "C"), c("A", "C", "B"), c("B", "A", "C"), c("B", "C", "A"),
         c("C", "A", "B"), c("C", "B", "A"))
  )
})
