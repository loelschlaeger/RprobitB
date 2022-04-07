test_that("model formula is correctly interpreted", {
  form <- choice ~ price | 0 | time + comfort + change
  re <- c("price", "time")
  out <- check_form(form = form, re = re)
  expect_type(out, "list")
  expect_equal(names(out), c("form", "choice", "re", "vars", "ASC"))
  expect_equal(out$choice, "choice")
  expect_equal(out$re, re)
  expect_equal(
    out$vars,
    list("price", character(0), c("time", "comfort", "change"))
  )
  expect_false(out$ASC)
  expect_warning(
    expect_equal(
      check_form(form = form, re = c("price", "time", "bug"))$re,
      c("price", "time")
    ),
    "The covariate 'bug' in 're' is not part of 'form' and hence ignored."
  )
  expect_identical(check_form(form = form, re = NULL)$re, NULL)
})

test_that("overview effects can be generated", {
  form <- choice ~ cost | income | time
  re <- c("cost", "ASC")
  alternatives <- c("train", "bus", "car")
  out <- overview_effects(form = form, re = re, alternatives = alternatives)
  expect_snapshot(out)
})

test_that("lagged choice covariates can be created", {
  choice_data <- data.frame("A" = rep(1:3, each = 3), "B" = rep(1:3, times = 3))
  expect_equal(
    create_lagged_cov(choice_data = choice_data, column = "B", k = 1, id = "A"),
    cbind(choice_data, "B.1" = rep(c(NA, 1, 2), 3))
  )
})

test_that("alternative-specific covariates can be renamed", {
  choice_data <- as_cov_names(
    choice_data = data.frame(
      "A1" = NA, "A2" = NA, "B1" = NA, "B2" = NA,
      "C" = NA
    ),
    cov = c("A", "B"),
    alternatives = c("1", "2")
  )
  expect_equal(colnames(choice_data), c("A_1", "A_2", "B_1", "B_2", "C"))
})

test_that("data preparation works", {
  data("Train", package = "mlogit")
  data <- prepare_data(
    form = choice ~ price | 1 | time + comfort + change,
    choice_data = Train,
    re = c("price", "time"),
    alternatives = c("A", "B"),
    id = "id",
    idc = "choiceid",
    standardize = c("price", "time")
  )
  expect_s3_class(data, "RprobitB_data")
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("simulating choice data works", {
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 10,
    T = 1:10,
    J = 3,
    re = c("cost", "ASC"),
    alternatives = c("train", "bus", "car"),
    seed = 1,
    C = 2
  )
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})


test_that("splitting data set by N works", {
  x <- simulate_choices(
    form = choice ~ covariate, N = 10, T = 1:10, J = 2,
    re = "covariate", C = 2, seed = 1
  )
  expect_snapshot(train_test(x, test_proportion = 0.3, by = "N"))
  expect_snapshot(train_test(x, test_proportion = 0, by = "N"))
  expect_snapshot(train_test(x, test_proportion = 1, by = "N"))
  expect_snapshot(train_test(x,
    test_proportion = 0.5, by = "N",
    random = TRUE, seed = 1
  ))
  expect_snapshot(train_test(x, test_number = 1, by = "N"))
  expect_snapshot(train_test(x, test_number = 2, by = "N"))
  expect_snapshot(train_test(x,
    test_number = 1, by = "N",
    random = TRUE, seed = 1
  ))
})

test_that("splitting data set by T works", {
  x <- simulate_choices(
    form = choice ~ covariate, N = 10, T = 10, J = 2,
    re = "covariate", C = 2, seed = 1
  )
  expect_snapshot(train_test(x, test_proportion = 0.3, by = "T"))
  expect_snapshot(train_test(x,
    test_proportion = 0.5, by = "T",
    random = TRUE, seed = 1
  ))
  expect_snapshot(train_test(x, test_number = 1, by = "T"))
  expect_snapshot(train_test(x, test_number = 2, by = "T"))
  expect_snapshot(train_test(x,
    test_number = 1, by = "T",
    random = TRUE, seed = 1
  ))
})
