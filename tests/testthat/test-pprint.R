test_that("pprint works", {
  expect_snapshot(pprint(x = 1))
  expect_snapshot(pprint(x = 1.5, label = "single numeric"))
  expect_snapshot(pprint(x = 1.5, label = "single numeric", onerow = TRUE))
  expect_snapshot(pprint(x = LETTERS[1:26]))
  expect_snapshot(pprint(x = LETTERS[1:26], label = "letters"))
  expect_snapshot(pprint(x = LETTERS[1:26], label = "letters", details = FALSE))
  expect_snapshot(pprint(x = LETTERS[1:26], label = "letters", onerow = TRUE))
  expect_snapshot(pprint(x = LETTERS[1:26], rowdots = 1))
  expect_snapshot(pprint(x = LETTERS[1:26], rowdots = 26))
  expect_snapshot(pprint(x = LETTERS[1:26], coldots = 1))
  expect_snapshot(pprint(x = LETTERS[1:26], coldots = 26))
  expect_snapshot(pprint(x = LETTERS[1:26], coldots = 10))
  expect_error(pprint(x = LETTERS[1:26], coldots = 0))
  expect_snapshot(pprint(x = matrix(LETTERS[1:24], ncol = 6)))
  expect_snapshot(pprint(x = matrix(LETTERS[1:24], ncol = 6), label = "big matrix"))
  expect_snapshot(pprint(x = matrix(LETTERS[1:24], ncol = 6), rowdots = 2, label = "big matrix"))
  expect_snapshot(pprint(x = matrix(LETTERS[1:24], ncol = 6), details = FALSE))
  expect_snapshot(pprint(x = matrix(LETTERS[1:24], ncol = 6), coldots = 1, rowdots = 1))
  expect_snapshot(pprint(x = matrix(LETTERS[1:24], ncol = 6), onerow = TRUE, coldots = 2, rowdots = 2))
  expect_snapshot(pprint(x = diag(5), coldots = 2, rowdots = 3, onerow = TRUE, digits = 0))



  pprint(x = matrix(1:100, nrow = 1))
  pprint(x = matrix(1:100, nrow = 1), label = "single row matrix")
  pprint(x = matrix(1:100, nrow = 1), details = FALSE)

  pprint(x = matrix(1:100, nrow = 1), coldots = 1) # BUG

  pprint(x = matrix(1:100, nrow = 1), rowdots = 1)
  pprint(x = matrix(1:100, nrow = 1), onerow = TRUE, coldots = 5)




  pprint(x = matrix(1:100, ncol = 1))
  pprint(x = matrix(1:100, ncol = 1), label = "single column matrix")
  pprint(x = matrix(1:100, ncol = 1), details = FALSE)
  pprint(x = matrix(1:100, ncol = 1), coldots = 1)

  pprint(x = matrix(1:100, ncol = 1), rowdots = 1) # BUG

  pprint(x = matrix(1:100, ncol = 1), onerow = TRUE, coldots = 5)

})
