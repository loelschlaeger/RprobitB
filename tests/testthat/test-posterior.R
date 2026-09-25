test_that("summary.RprobitB_fit reports posterior diagnostics", {
  model <- make_test_fit()
  dynamic_model <- suppressWarnings(fit(
    choice ~ x | 0,
    random_effects = "x",
    latent_class_effects = "x",
    class_update = "dirichlet_process",
    max_classes = 3L,
    n_deciders = 5L,
    iterations = 20L,
    warmup = 10L,
    chains = 1L,
    progress = FALSE
  ))
  result <- summary(model)
  class_result <- summary(dynamic_model, variables = "n_classes")
  expect_s3_class(result, "summary.RprobitB_fit")
  expect_s3_class(result$posterior, "data.frame")
  expect_named(
    result$posterior,
    c("variable", "mean", "mode", "sd", "rhat", "ess_bulk")
  )
  expect_named(
    class_result$posterior,
    c("variable", "dgp", "mean", "mode", "sd", "rhat", "ess_bulk")
  )
  expect_identical(class_result$posterior$dgp, 1)
  expect_true(is.finite(result$posterior$mode))
  expect_identical(class_result$posterior$variable, "n_classes")
  dynamic_summary <- summary(dynamic_model)$posterior
  dynamic_default <- dynamic_summary$variable
  expect_true(all(c("class_concentration", "n_classes") %in% dynamic_default))
  expect_true(any(startsWith(dynamic_default, "weight[")))
  expect_true(any(startsWith(dynamic_default, "mu[")))
  if ("occupied" %in% names(dynamic_summary)) {
    expect_true(all(dynamic_summary$occupied > 0))
  }
  class_draws <- as.vector(dynamic_model$draws[, , "n_classes"])
  class_values <- sort(unique(class_draws))
  expect_identical(
    class_result$posterior$mode,
    class_values[which.max(tabulate(match(class_draws, class_values)))]
  )
  expect_false("Sigma[B,B]" %in% result$posterior$variable)
  uncorrelated <- fit(
    choice ~ x + z | 0,
    random_effects = c(x = "n", z = "n"),
    n_deciders = 4L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  expect_identical(
    summary(uncorrelated)$posterior$variable,
    c("mu[x]", "mu[z]", "Omega[x,x]", "Omega[z,z]")
  )
  expect_identical(
    summary(model, variables = "Sigma[B,B]")$posterior$variable, "Sigma[B,B]"
  )
  expect_named(
    summary(model, probs = c(0.1, 0.9))$posterior,
    c("variable", "mean", "mode", "sd", "q10", "q90", "rhat", "ess_bulk")
  )
  selected <- summary(
    model, statistics = c("median", "mcse_mean", "ess_tail"), probs = 0.5
  )$posterior
  expect_named(
    selected, c("variable", "median", "q50", "mcse_mean", "ess_tail")
  )
  expect_identical(selected$median, selected$q50)
  expect_true(all(is.finite(selected$mcse_mean) & selected$mcse_mean > 0))
  expect_named(
    summary(model, statistics = "rhat", probs = 0.5)$posterior,
    c("variable", "q50", "rhat")
  )
  expect_error(summary(model, statistics = "mcse"), "statistics")
})

test_that("simulated-data summaries show DGP values", {
  model <- fit(
    choice ~ price + x | 0,
    scale = c(price = -1),
    dgp_parameters = list(beta = c(price = -1, x = 2)),
    n_deciders = 5L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  mixture <- fit(
    choice ~ x | 0,
    random_effects = "x",
    latent_class_effects = "x",
    classes = 2L,
    n_deciders = 6L,
    n_occasions = 2L,
    dgp_parameters = list(
      beta = list(c(x = -1), c(x = 2)),
      Omega = list(matrix(0.2), matrix(0.4)),
      weights = c(0.25, 0.75)
    ),
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  result <- summary(model)$posterior
  mixture_result <- summary(mixture)$posterior
  expect_equal(result$dgp, c(2, 1))
  expect_equal(
    mixture_result$dgp,
    c(0.75, 0.25, 2, -1, 0.4, 0.2)
  )
})

test_that("summaries and parameters handle latent class effects", {
  set.seed(1)
  model <- fit(
    choice ~ price + time | 0,
    latent_class_effects = "price",
    classes = 2L,
    n_deciders = 8L,
    n_occasions = 2L,
    scale = c(time = 1),
    dgp_parameters = list(
      beta = list(c(price = -2, time = 1), c(price = 0.5, time = 1)),
      weights = c(0.75, 0.25),
      Sigma = diag(2)
    ),
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  result <- summary(model)$posterior
  parameters <- as_choice_parameters(model)
  expect_identical(
    result$variable,
    c("weight[1]", "weight[2]", "beta[price,1]", "beta[price,2]", "Sigma[B,B]")
  )
  expect_equal(result$dgp, c(0.75, 0.25, -2, 0.5, 2))
  expect_named(parameters$beta[[1L]], c("price", "time"))
  expect_equal(parameters$beta[[1L]][["time"]], parameters$beta[[2L]][["time"]])
  expect_false(
    parameters$beta[[1L]][["price"]] == parameters$beta[[2L]][["price"]]
  )
  expect_null(parameters$Omega)
  expect_length(parameters$weights, 2L)
})

test_that("latent_class_diagnostics is label invariant", {
  model <- fit(
    choice ~ x | 0,
    random_effects = "x",
    latent_class_effects = "x",
    classes = 2L,
    n_deciders = 8L,
    n_occasions = 3L,
    iterations = 30L,
    warmup = 10L,
    chains = 1L,
    progress = FALSE
  )
  diagnostics <- latent_class_diagnostics(model)
  permuted <- model
  class_variables <- startsWith(
    dimnames(permuted$draws)$variable, "class["
  )
  permuted$draws[, , class_variables] <-
    3L - permuted$draws[, , class_variables]
  permuted_diagnostics <- latent_class_diagnostics(permuted)
  expect_named(diagnostics, c("occupancy", "co_clustering", "membership"))
  expect_equal(sum(diagnostics$occupancy$probability), 1)
  expect_identical(diagnostics$occupancy$n_classes, 1:2)
  expect_identical(diagnostics$occupancy$probability[2], 1)
  expect_equal(unname(diag(diagnostics$co_clustering)), rep(1, 8L))
  expect_equal(unname(rowSums(diagnostics$membership)), rep(1, 8L))
  expect_equal(
    permuted_diagnostics$co_clustering,
    diagnostics$co_clustering
  )
  expect_equal(permuted_diagnostics$occupancy, diagnostics$occupancy)
})

test_that("plot.RprobitB_fit creates standard posterior plots", {
  model <- suppressWarnings(make_test_fit(random_effects = "x", chains = 2L))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  plots <- lapply(
    c("trace", "rank", "acf", "density", "interval", "pairs"),
    function(type) plot(model, type = type)
  )
  expect_true(all(vapply(
    plots,
    function(x) inherits(x, "ggplot") || inherits(x, "bayesplot_grid"),
    logical(1)
  )))
  expect_error(plot(model, variables = "unknown"), "subset")
})

test_that("coef.RprobitB_fit summarizes global posterior variables", {
  model <- make_test_fit()
  mixed_model <- make_test_fit(random_effects = "x")
  result <- coef(model)
  individual <- coef(mixed_model, level = "individual")
  expect_identical(names(result), "beta[x]")
  expect_identical(dim(individual), c(4L, 1L))
  expect_identical(rownames(individual), as.character(1:4))
  expect_identical(colnames(individual), "x")
})

test_that("vcov.RprobitB_fit returns posterior covariance", {
  model <- make_test_fit()
  result <- vcov(model)
  expect_identical(dim(result), c(1L, 1L))
  expect_identical(rownames(result), "beta[x]")
  expect_equal(result, t(result))
})

test_that("confint.RprobitB_fit returns equal-tailed intervals", {
  model <- make_test_fit()
  result <- confint(model, parm = "beta[x]", level = 0.8)
  expect_identical(dim(result), c(1L, 2L))
  expect_identical(dimnames(result), list("beta[x]", c("10%", "90%")))
  expect_true(result[1L, 1L] <= result[1L, 2L])
})

test_that("as_draws.RprobitB_fit returns posterior draws", {
  model <- make_test_fit()
  result <- posterior::as_draws(model)
  expect_identical(result, model$draws)
})
