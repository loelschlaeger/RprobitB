options("RprobitB_progress" = FALSE)

test_that("choice probabilities can be computed", {
  set.seed(1)
  form <- choice ~ price + time + change + comfort | 0
  data <- prepare_data(
    form = form,
    choice_data = train_choice[1:500, ],
    id = "deciderID",
    idc = "occasionID"
  )
  model_train <- fit_model(
    data = data,
    R = 100,
    scale = "price := -1"
  )
  choice_probs <- choice_probabilities(model_train)
  expect_snapshot(choice_probs)
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
  set.seed(1)
  N <- 100
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = N,
    T = sample.int(5, size = N, replace = TRUE),
    J = 2,
    alternatives = c("bus", "car"),
    true_parameter = list("alpha" = 1:5, "Sigma" = 1)
  )
  data <- train_test(data, test_proportion = 0.3)
  model <- fit_model(data$train, R = 1000)
  expect_snapshot(predict(model, overview = TRUE))
  expect_snapshot(predict(model, overview = FALSE))
  expect_snapshot(predict(model, data = data$test, overview = TRUE))
})

test_that("preference classification works", {
  set.seed(1)
  N <- 100
  data <- simulate_choices(
    form = choice ~ cost,
    N = N,
    T = 10,
    J = 3,
    re = c("cost"),
    alternatives = c("train", "bus", "car"),
    true_parameter = list(
      "C" = 2, "s" = c(0.6, 0.4), "b" = matrix(c(-3, 3), ncol = 2)
    )
  )
  model <- fit_model(data, latent_classes = list("C" = 2))
  classif <- classification(model, add_true = TRUE)
  expect_snapshot(classif)
  # sum(classif$est == classif$true) / N
})
