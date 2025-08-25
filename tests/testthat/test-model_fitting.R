options("RprobitB_progress" = FALSE)

test_that("setting prior parameter works", {
  prior <- check_prior(P_f = 1, P_r = 2, J = 3)
  expect_snapshot(prior)
  expect_s3_class(prior, "RprobitB_prior")
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
      "wb_update" = TRUE,
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
    form = choice ~ a | b | c, N = 50, T = 1:50, J = 2, base = "B"
  )
  model <- fit_model(data, R = 2000)
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
  expect_snapshot(print(coef(model)))
})

test_that("Ordered probit model estimation works", {
  N <- 100
  T <- 10
  data <- simulate_choices(
    form = opinion_on_sth ~ age + gender,
    N = N,
    T = T,
    J = 5,
    alternatives = c("very bad", "bad", "indifferent", "good", "very good"),
    ordered = TRUE,
    covariates = list(
      "gender" = rep(sample(c(0, 1), N, replace = TRUE), times = T)
    ),
    true_parameter = list(
      "alpha" = c(1, 2), "d" = c(0, 1, 2)
    )
  )
  # lapply(data$data, `[[`, "y") |> unlist() |> table() / (N * T) |> round(2)
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
    ranked = TRUE
  )
  model <- fit_model(data)
  expect_snapshot(print(model))
  expect_snapshot(summary(model))
  expect_snapshot(print(coef(model)))
})

test_that("setting fixed parameters for the Gibbs sampling works", {
  set.seed(1)
  par <- list("Sigma" = 1, "alpha" = 1:2, b = 1, Omega = 0.1)
  data <- simulate_choices(
    form = choice ~ a | b, N = 100, T = 5, J = 2, base = "B",
    re = "b", true_parameter = par
  )
  model <- fit_model(data, R = 1000, fixed_parameter = par)
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

test_that("Gelman-Rubin statistic can be computed", {
  set.seed(1)
  no_chains <- 2
  length_chains <- 1e3
  samples <- matrix(NA_real_, length_chains, no_chains)
  samples[1, ] <- 1
  Gamma <- matrix(c(0.8, 0.1, 0.2, 0.9), 2, 2)
  for (c in 1:no_chains) {
    for (t in 2:length_chains) {
      samples[t, c] <- sample(1:2, 1, prob = Gamma[samples[t - 1, c], ])
    }
  }
  R_hat_val <- R_hat(samples)
  expect_equal(round(R_hat_val, 2), 1.01)
})

