test_that("checking booleans works", {
  expect_false(is_bool("TRUE"))
  expect_true(is_bool(FALSE))
})

test_that("checking single numeric works", {
  expect_true(is_single_numeric(1))
  expect_false(is_single_numeric("1"))
  expect_false(is_single_numeric(NA_real_))
})

test_that("checking positive integer works", {
  expect_false(is_pos_int(-1))
  expect_true(is_pos_int(1))
  expect_false(is_pos_int(NA_real_))
  expect_false(is_pos_int(1.1))
})

test_that("sampling and checking covariance matrix works", {
  expect_true(is_cov_matrix(diag(10)))
  x <- sample_cov_matrix(dim = 3)
  expect_true(is_cov_matrix(x))
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

test_that("print_matrix works", {
  expect_snapshot(
    print_matrix(x = 1)
  )
  expect_snapshot(
    print_matrix(x = 1.5, label = "single numeric")
  )
  expect_snapshot(
    print_matrix(x = 1.5, label = "single numeric", simplify = TRUE)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26])
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], label = "letters")
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], label = "letters", details = FALSE)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], label = "letters", simplify = TRUE)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], rowdots = 1)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], rowdots = 26)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], coldots = 1)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], coldots = 25)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], coldots = 26)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], coldots = 10)
  )
  expect_error(
    print_matrix(x = LETTERS[1:26], coldots = 0)
  )
  expect_snapshot(
    print_matrix(x = matrix(LETTERS[1:24], ncol = 6))
  )
  expect_snapshot(
    print_matrix(x = matrix(LETTERS[1:24], ncol = 6), label = "big matrix")
  )
  expect_snapshot(
    print_matrix(x = matrix(LETTERS[1:24], ncol = 6), rowdots = 2)
  )
  expect_snapshot(
    print_matrix(x = matrix(LETTERS[1:24], ncol = 6), details = FALSE)
  )
  expect_snapshot(
    print_matrix(x = matrix(LETTERS[1:24], ncol = 6), coldots = 1, rowdots = 1)
  )
  expect_snapshot(
    print_matrix(x = matrix(LETTERS[1:24], ncol = 6), simplify = TRUE,
                 coldots = 2, rowdots = 2)
  )
  expect_snapshot(
    print_matrix(x = diag(5), coldots = 2, rowdots = 3, simplify = TRUE,
                 digits = 0)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, nrow = 1))
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, nrow = 1), label = "single row matrix")
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, nrow = 1), details = FALSE)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, nrow = 1), coldots = 1)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, nrow = 1), rowdots = 1)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, nrow = 1), simplify = TRUE, coldots = 5)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, ncol = 1))
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, ncol = 1), label = "single column matrix")
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, ncol = 1), details = FALSE)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, ncol = 1), coldots = 1)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, ncol = 1), rowdots = 1)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, ncol = 1), simplify = TRUE, coldots = 5)
  )
  expect_snapshot(
    print_matrix(x = diag(2), simplify = TRUE)
  )
  expect_snapshot(
    print_matrix(x = diag(2), simplify = FALSE)
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
