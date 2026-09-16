test_that("logLik.RprobitB_fit evaluates posterior mean parameters", {
  model <- make_test_fit(data.frame(
    deciderID = rep(1:2, each = 2L),
    occasionID = rep(1:2, 2L),
    choice = c("A", "B", "B", "A"),
    x_A = c(0, 1, 1, 0),
    x_B = c(1, 0, 0, 1)
  ))

  result <- logLik(model)
  ordered <- fit(
    choice ~ x | 0,
    alternatives = c("low", "middle", "high"),
    choice_type = "ordered",
    n_deciders = 4L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  uncorrelated <- fit(
    choice ~ x + z | 0,
    random_effects = c(x = "n", z = "n"),
    n_deciders = 4L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )

  expect_s3_class(result, "logLik")
  expect_identical(attr(result, "nobs"), 4L)
  expect_identical(attr(result, "df"), 1L)
  expect_true(is.finite(as.numeric(result)))
  latent <- fit(
    choice ~ x + z | 0,
    latent_class_effects = "x",
    classes = 2L,
    n_deciders = 4L,
    n_occasions = 2L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  expect_identical(attr(logLik(uncorrelated), "df"), 4L)
  expect_identical(attr(logLik(ordered), "df"), 2L)
  expect_identical(attr(logLik(latent), "df"), 4L)
  expect_true(is.finite(AIC(model)))
  expect_true(is.finite(BIC(model)))
  expect_false(any(c("AIC.RprobitB_fit", "BIC.RprobitB_fit") %in%
    methods(class = "RprobitB_fit")))

  sparse <- fit(
    choice ~ x | 0,
    random_effects = "x",
    latent_class_effects = "x",
    classes = 3L,
    class_update = "sparse",
    n_deciders = 5L,
    n_occasions = 2L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  sparse_parameters <- as_choice_parameters(sparse)
  sparse_classes <- if (is.list(sparse_parameters$beta)) {
    length(sparse_parameters$beta)
  } else {
    1L
  }
  expect_true(is.finite(logLik(sparse)))
  expect_identical(attr(logLik(sparse), "df"), 3L * sparse_classes - 1L)
  if (sparse_classes > 1L) expect_equal(sum(sparse_parameters$weights), 1)
  expect_error(logLik(model, ghk_draws = 1L), "ghk_draws")
})

test_that("WAIC uses decider-level posterior log likelihoods", {
  model <- make_test_fit(data.frame(
    deciderID = rep(1:2, each = 2L),
    occasionID = rep(1:2, 2L),
    choice = c("A", "B", "B", "A"),
    x_A = c(0, 1, 1, 0),
    x_B = c(1, 0, 0, 1)
  ))

  result <- suppressWarnings(WAIC(model, progress = FALSE))

  expect_s3_class(result, "waic")
  expect_identical(colnames(result$estimates), c("Estimate", "SE"))
  expect_identical(nrow(result$pointwise), 2L)
  expect_identical(dim(result$pointwise), c(2L, 3L))
  expect_error(WAIC(model, ghk_draws = 1L, progress = FALSE), "ghk_draws")
})

test_that("loo.RprobitB_fit returns Pareto-k diagnostics by decider", {
  model <- make_test_fit(data.frame(
    deciderID = rep(1:2, each = 2L),
    occasionID = rep(1:2, 2L),
    choice = c("A", "B", "B", "A"),
    x_A = c(0, 1, 1, 0),
    x_B = c(1, 0, 0, 1)
  ))

  result <- suppressWarnings(loo::loo(model, progress = FALSE))

  expect_s3_class(result, "psis_loo")
  expect_length(result$diagnostics$pareto_k, 2L)
  expect_identical(nrow(result$pointwise), 2L)
})

test_that("bayes_factor compares models by bridge sampling", {
  model <- make_test_fit()
  set.seed(1)
  mixed_model <- fit(
    choice ~ x + z | 0,
    random_effects = c(x = "n", z = "ln"),
    n_deciders = 5L,
    n_occasions = 2L,
    iterations = 20L,
    warmup = 10L,
    chains = 1L,
    progress = FALSE
  )
  mixture <- fit(
    choice ~ x | 0,
    latent_class_effects = "x",
    classes = 2L,
    n_deciders = 4L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )

  result <- suppressWarnings(bayes_factor(model, model, progress = FALSE))
  mixed_result <- suppressWarnings(
    bayes_factor(mixed_model, mixed_model, progress = FALSE)
  )

  expect_s3_class(result, "bf_bridge")
  expect_true(is.finite(result$bf))
  expect_gt(result$bf, 0)
  expect_true(is.finite(mixed_result$bf))
  expect_error(bayes_factor(model, mixture), "model2")
  expect_error(bayes_factor(model, model, ghk_draws = 1L), "ghk_draws")
})
