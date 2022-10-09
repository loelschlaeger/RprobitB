test_that("RprobitB_formula can be specified and validated", {
  expect_RprobitB_formula <- function(object, vars, ASC, md_n, md_ln) {
    act <- quasi_label(rlang::enquo(object), arg = "object")
    expect(
      inherits(object, "RprobitB_formula"),
      glue::glue("{act$lab} is not of class 'RprobitB_formula'.")
    )
    expect(
      identical(object$vars, vars),
      glue::glue("{act$lab}: vars is is not as expected.")
    )
    expect(
      identical(object$ASC, ASC),
      glue::glue("{act$lab}: ASC is {object$ASC} but is expected to be {ASC}.")
    )
    expect(
      identical(object$md_n, md_n),
      glue::glue("{act$lab}: md_n is not as expected.")
    )
    expect(
      identical(object$md_ln, md_ln),
      glue::glue("{act$lab}: md_ln is not as expected.")
    )
    invisible(act$val)
  }
  f1 <- choice ~ A
  f2 <- choice ~ A | B
  f3 <- choice ~ A | B | C
  f4 <- choice ~ A | B + 0 | C
  f5 <- choice ~ A | B + 0 | C + D
  f6 <- choice ~ 0 | 0 | C
  f7 <- choice ~ 0 | 1 | C
  f8 <- choice ~ A + B
  f9 <- choice ~ A + B + 1
  re1 <- character()
  re2 <- "A"
  re3 <- "A+"
  re4 <- c("A+", "B")
  re5 <- c("B", "ASC")
  re6 <- c("B", "ASC+")
  re7 <- c("A", "A+")
  expect_error(
    new_RprobitB_formula(formula = "not_a_correct_formula", re = re1, ordered = FALSE)
  )
  expect_error(
    new_RprobitB_formula(formula = f1, re = 1, ordered = FALSE)
  )
  expect_error(
    new_RprobitB_formula(formula = f1, re = re1, ordered = "not_a_boolean")
  )
  expect_error(
    new_RprobitB_formula(formula = f1, re = "this_covariate_does_not_exist", ordered = FALSE)
  )
  expect_RprobitB_formula(
    new_RprobitB_formula(formula = f1, re = re1, ordered = FALSE),
    vars = list("A", character(), character()), ASC = TRUE,
    md_n = character(), md_ln = character()
  )
  expect_RprobitB_formula(
    new_RprobitB_formula(formula = f2, re = re2, ordered = FALSE),
    vars = list("A", "B", character()), ASC = TRUE,
    md_n = "A", md_ln = character()
  )
  expect_RprobitB_formula(
    new_RprobitB_formula(formula = f2, re = re3, ordered = FALSE),
    vars = list("A", "B", character()), ASC = TRUE,
    md_n = character(), md_ln = "A"
  )
  expect_RprobitB_formula(
    new_RprobitB_formula(formula = f2, re = re4, ordered = FALSE),
    vars = list("A", "B", character()), ASC = TRUE,
    md_n = "B", md_ln = "A"
  )
  expect_RprobitB_formula(
    new_RprobitB_formula(formula = f6, re = character(), ordered = FALSE),
    vars = list(character(), character(), "C"), ASC = FALSE,
    md_n = character(), md_ln = character()
  )
  expect_RprobitB_formula(
    new_RprobitB_formula(formula = f7, re = character(), ordered = FALSE),
    vars = list(character(), character(), "C"), ASC = TRUE,
    md_n = character(), md_ln = character()
  )
  expect_RprobitB_formula(
    new_RprobitB_formula(formula = f8, re = character(), ordered = FALSE),
    vars = list(c("A", "B"), character(), character()), ASC = TRUE,
    md_n = character(), md_ln = character()
  )
  expect_RprobitB_formula(
    new_RprobitB_formula(formula = f9, re = re4, ordered = TRUE),
    vars = list(character(), c("A", "B"), character()), ASC = FALSE,
    md_n = "B", md_ln = "A"
  )
  expect_error(
    new_RprobitB_formula(formula = f3, re = character(), ordered = TRUE)
  )
  expect_RprobitB_formula(
    new_RprobitB_formula(formula = f8, re = re5, ordered = FALSE),
    vars = list(c("A", "B"), character(), character()), ASC = TRUE,
    md_n = c("B", "ASC"), md_ln = character()
  )
  expect_error(
    new_RprobitB_formula(formula = f5, re = re5, ordered = FALSE)
  )
  expect_RprobitB_formula(
    new_RprobitB_formula(formula = f8, re = re6, ordered = FALSE),
    vars = list(c("A", "B"), character(), character()), ASC = TRUE,
    md_n = "B", md_ln = "ASC"
  )
  expect_error(
    new_RprobitB_formula(formula = f2, re = re7, ordered = FALSE)
  )
})
