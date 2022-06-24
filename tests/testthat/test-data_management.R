options("RprobitB_progress" = FALSE)

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
  base <- "train"
  out <- overview_effects(
    form = form, re = re, alternatives = alternatives,
    base = base
  )
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
    standardize = c("price", "time"),
  )
  expect_s3_class(data, "RprobitB_data")
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("data preparation with non-standard base alternative works", {
  data("Train", package = "mlogit")
  data <- prepare_data(
    form = choice ~ price | 1 | time + comfort + change,
    choice_data = Train,
    re = c("price", "time"),
    alternatives = c("A", "B"),
    id = "id",
    idc = "choiceid",
    standardize = c("price", "time"),
    base = "A"
  )
  expect_s3_class(data, "RprobitB_data")
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("missing data replacement works", {
  choice_data <- data.frame("A" = c(1, NA, 3), "B" = c(1, 2, Inf))
  expect_equal(
    missing_data(choice_data, "complete_cases"),
    data.frame("A" = c(1), "B" = c(1))
  )
  expect_equal(
    missing_data(choice_data, "zero"),
    data.frame("A" = c(1, 0, 3), "B" = c(1, 2, 0))
  )
  expect_equal(
    missing_data(choice_data, "mean"),
    data.frame("A" = c(1, 2, 3), "B" = c(1, 2, 1.5))
  )
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
    true_parameter = list("C" = 2)
  )
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("simulating ordered choices works", {
  data <- simulate_choices(
    form = opinion ~ age + gender,
    N = 10,
    T = 1:10,
    J = 5,
    alternatives = c("very bad", "bad", "indifferent", "good", "very good"),
    ordered = TRUE,
    covariates = list(
      "gender" = rep(sample(c(0,1), 10, replace = TRUE), times = 1:10)
      ),
    seed = 1
  )
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("simulating ranked choices works", {
  data <- simulate_choices(
    form = product ~ price,
    N = 10,
    T = 1:10,
    J = 3,
    alternatives = c("A", "B", "C"),
    ranked = TRUE,
    seed = 1
  )
  expect_snapshot(print(data))
  expect_snapshot(summary(data))
})

test_that("splitting data set by N works", {
  x <- simulate_choices(
    form = choice ~ covariate, N = 10, T = 1:10, J = 2,
    re = "covariate", true_parameter = list("C" = 2), seed = 1
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
    re = "covariate", true_parameter = list("C" = 2), seed = 1
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

test_that("parameter checks work", {
  P_f <- sample(0:10, 1)
  P_r <- sample(0:10, 1)
  J <- sample(2:10, 1)
  N <- sample(1:1000, 1)
  parm <- RprobitB_parameter(P_f = P_f, P_r = P_r, J = J, N = N)
  if (P_f > 0) {
    expect_length(parm$alpha, P_f)
  } else {
    expect_equal(parm$alpha, NA)
  }
  if (P_r > 0) {
    expect_true(is.numeric(parm$C))
    expect_true(parm$C %% 1 == 0)
    expect_true(parm$C >= 0)
  } else {
    expect_equal(parm$C, NA)
  }
})

test_that("parameter reproducibility works", {
  x <- RprobitB_parameter(P_f = 2, P_r = 2, J = 2, N = 100, seed = 1)
  expect_snapshot(unclass(x))
})

test_that("parameter printing works", {
  x <- RprobitB_parameter(P_f = 1, P_r = 1, J = 2, N = 100, seed = 1)
  expect_snapshot(x)
})

