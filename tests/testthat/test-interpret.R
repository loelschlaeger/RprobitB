test_that("interpret computes compensations from the coefficient draws", {
  model <- fit(
    choice ~ x + z | 0,
    n_deciders = 20L,
    iterations = 20L,
    warmup = 10L,
    chains = 1L,
    progress = FALSE
  )
  draws <- posterior::as_draws_matrix(model$draws)
  expected <- -draws[, "beta[z]"] / draws[, "beta[x]"]
  result <- interpret(model, reference = "x")
  expect_s3_class(result, "RprobitB_interpretation")
  expect_identical(result$effect, "z")
  expect_equal(result$mean, mean(expected))
  expect_named(result, c("effect", "mean", "sd", "lower", "upper"))
  expect_error(interpret(model), "unless `scale` fixed a coefficient")
  expect_error(interpret(model, reference = "unknown"), "reference")
  expect_error(interpret(model, reference = "x", effects = "x"), "effects")
})

test_that("interpret computes marginal effects for numeric covariates", {
  model <- fit(
    choice ~ x | 0,
    n_deciders = 20L,
    iterations = 20L,
    warmup = 10L,
    chains = 1L,
    progress = FALSE
  )
  at_average <- interpret(model, type = "mea", progress = FALSE)
  at_given <- interpret(model, type = "mea", at = c(x = 0.5), progress = FALSE)
  averaged <- interpret(model, type = "ame", progress = FALSE)
  handlers <- progressr::handlers("void")
  on.exit(progressr::handlers(handlers), add = TRUE)
  reported <- interpret(model, type = "ame", progress = TRUE)
  output <- capture.output(print(averaged))
  expect_named(
    at_average,
    c("covariate", "alternative", "at", "mean", "sd", "lower", "upper")
  )
  expect_named(
    averaged, c("covariate", "alternative", "mean", "sd", "lower", "upper")
  )
  expect_identical(at_average$alternative, c("A", "B"))
  expect_identical(at_given$at, rep(0.5, 2L))
  expect_false(any(at_average$at == 0.5))
  expect_identical(reported, averaged)
  expect_true(all(is.finite(averaged$mean)))
  expect_true(any(grepl("Average marginal effects", output, fixed = TRUE)))
})
