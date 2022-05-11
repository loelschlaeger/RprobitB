options("RprobitB_progress" = FALSE)

test_that("choice probabilities can be computed", {
  data("model_train", package = "RprobitB")
  expect_snapshot(choice_probabilities(model_train))
})

test_that("creation of labels works", {
  P_f <- sample(0:10, 1)
  P_r <- sample(0:10, 1)
  J <- sample(2:10, 1)
  C <- sample(1:5, 1)
  cov_sym <- sample(c(TRUE, FALSE), 1)
  drop_par <- if (runif(1) < 0.5) {
    NULL
  } else {
    sample(c("alpha", "s", "b", "Omega", "Sigma"), sample(1:5, 1))
  }
  out <- parameter_labels(
    P_f = P_f, P_r = P_r, J = J, C = C, cov_sym = cov_sym,
    drop_par = drop_par
  )
  expect_setequal(
    names(out), setdiff(
      c(
        if (P_f > 0) "alpha",
        if (P_r > 0) c("s", "b", "Omega"), "Sigma"
      ),
      drop_par
    )
  )
})

test_that("choice prediction works", {
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 10,
    T = 1:10,
    J = 2,
    alternatives = c("bus", "car"),
    seed = 1,
    true_parameter = list("alpha" = 1:5, "Sigma" = 1)
  )
  data <- train_test(data, test_proportion = 0.3)
  model <- fit_model(data$train, R = 1000, seed = 1)
  expect_snapshot(predict(model, overview = TRUE))
  expect_snapshot(predict(model, overview = FALSE))
  expect_snapshot(predict(model, data = data$test, overview = TRUE))
})

test_that("preference classification works", {
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 30,
    T = 10,
    J = 3,
    re = c("cost", "ASC"),
    alternatives = c("train", "bus", "car"),
    seed = 1,
    true_parameter = list("C" = 2)
  )
  model <- fit_model(data,
    R = 1000,
    seed = 1,
    latent_classes = list("C" = 2)
  )
  expect_snapshot(preference_classification(model, add_true = TRUE))
})
