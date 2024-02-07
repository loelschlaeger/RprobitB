options("RprobitB_progress" = FALSE)

test_that("setting prior parameter works", {
  prior <- check_prior(P_f = 1, P_r = 2, J = 3)
  expect_snapshot(prior)
  expect_s3_class(prior, "RprobitB_prior")
})

test_that("setting of initial Gibbs values works", {
  init <- RprobitB:::set_initial_gibbs_values(
    N = 2, T = 3, J = 3, P_f = 1, P_r = 2, C = 2
  )
  expect_snapshot(init)
})

test_that("RprobitB_latent_class setting works", {
  expect_error(
    RprobitB_latent_classes("not a list")
  )
  expect_snapshot(
    RprobitB_latent_classes(list("C" = 2))
  )
  expect_error(
    RprobitB_latent_classes(list("C" = -1))
  )
  expect_snapshot(
    (out <- RprobitB_latent_classes(list(
      "weight_update" = TRUE,
      "dp_update" = TRUE
    )))
  )
  expect_snapshot(str(out))
})

test_that("building of RprobitB_normalization works", {
  form <- choice ~ price + time + comfort + change | 1
  re <- "time"
  alternatives <- c("A", "B")
  expect_warning(
    RprobitB_normalization(
      level = "A", scale = "Sigma_1,1 := 1", form = form, re = re,
      alternatives = alternatives, base = "B"
    )
  )
  expect_snapshot(
    RprobitB_normalization(
      level = "B", scale = "price := -1", form = form, re = re,
      alternatives = alternatives, base = "B"
    )
  )
  expect_error(
    RprobitB_normalization(
      level = "B", scale = "time := 1", form = form, re = re,
      alternatives = alternatives, base = "B"
    )
  )
  expect_error(
    RprobitB_normalization(
      level = "B", scale = "Sigma_3,3 := 1", form = form, re = re,
      alternatives = alternatives, base = "B"
    )
  )
  expect_error(
    RprobitB_normalization(
      level = "B", scale = "Sigma_1,1 := -1", form = form, re = re,
      alternatives = alternatives, base = "B"
    )
  )
})

test_that("Gibbs sampling works", {
  data <- simulate_choices(
    form = choice ~ a | b | c,
    N = 50, T = 1:50, J = 2,
    seed = 1, base = "B"
  )
  model <- fit_model(data, R = 2000, seed = 1)
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
  expect_snapshot(print(coef(model)))
})

test_that("Ordered probit model estimation works", {
  data <- simulate_choices(
    form = opinion_on_sth ~ age + gender,
    N = 50,
    T = 1:50,
    J = 5,
    alternatives = c("very bad", "bad", "indifferent", "good", "very good"),
    ordered = TRUE,
    covariates = list(
      "gender" = rep(sample(c(0, 1), 50, replace = TRUE), times = 1:50)
    ),
    seed = 1
  )
  model <- fit_model(data)
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
  expect_snapshot(print(coef(model)))
})

test_that("Ranked probit model estimation works", {
  data <- simulate_choices(
    form = product ~ price,
    N = 100,
    T = 10,
    J = 3,
    ranked = TRUE,
    seed = 1
  )
  model <- fit_model(data)
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
  expect_snapshot(print(coef(model)))
})

test_that("setting fixed parameters for the Gibbs sampling works", {
  par <- list("Sigma" = 1, "alpha" = 1:2, b = 1, Omega = 0.1)
  data <- simulate_choices(
    form = choice ~ a | b, N = 10, T = 1:10, J = 2, seed = 1, base = "B",
    re = "b", true_parameter = par
  )
  model <- fit_model(
    data,
    R = 2000, seed = 1, fixed_parameter = par
  )
  true <- do.call(
    what = RprobitB_parameter,
    args = c(
      list(
        "P_f" = data$P_f, "P_r" = data$P_r, "J" = data$J, "N" = data$N,
        "ordered" = data$ordered, sample = FALSE
      ),
      par
    )
  )
  est <- point_estimates(model)
  expect_true(all.equal(true, est))
})

test_that("computation of sufficient statistics works", {
  form <- choice ~ v1 | v2
  re <- "v2"
  alternatives <- c("A", "B", "C")
  data <- simulate_choices(
    form = form, N = 2, T = 1:2, J = 3, re = re, alternatives = alternatives,
    seed = 1
  )
  normalization <- RprobitB:::RprobitB_normalization(
    form = form, re = re, alternatives = alternatives, base = "C"
  )
  ss <- RprobitB:::sufficient_statistics(data = data, normalization = normalization)
  expect_snapshot(ss)
})

test_that("transforming RprobitB_fit works", {
  set.seed(1)
  form <- choice ~ price + time + change + comfort | 0
  data <- prepare_data(form = form, choice_data = train_choice, id = "deciderID", idc = "occasionID")
  model <- fit_model(
    data = data,
    scale = "price := -1",
    R = 100,
    B = 90
  )
  model_new_B <- transform(model, B = 2)
  expect_s3_class(model_new_B, "RprobitB_fit")
  expect_s3_class(model_new_B$gibbs_samples, "RprobitB_gibbs_samples")
  expect_equal(model_new_B$B, 2)
  model_new_Q <- transform(model, Q = 1)
  expect_s3_class(model_new_Q, "RprobitB_fit")
  expect_s3_class(model_new_Q$gibbs_samples, "RprobitB_gibbs_samples")
  expect_equal(model_new_Q$Q, 1)
  model_new_scale <- transform(
    model,
    scale = "Sigma_1,1 := 1", check_preference_flip = FALSE
  )
  expect_s3_class(model_new_scale, "RprobitB_fit")
  expect_s3_class(model_new_scale$gibbs_samples, "RprobitB_gibbs_samples")
})
