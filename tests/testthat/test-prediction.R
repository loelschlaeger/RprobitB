test_that("predict.RprobitB_fit returns posterior choice predictions", {
  model <- make_test_fit()
  newdata <- model.frame(model)
  newdata$choice <- NULL
  result <- predict(model, newdata = newdata, uncertainty = TRUE, level = 0.8)
  expect_s3_class(result, "data.frame", exact = TRUE)
  expect_named(
    result,
    c(
      "deciderID", ".prediction", "probability_A", "probability_B",
      "sd_A", "sd_B", "lower_A", "lower_B", "upper_A", "upper_B"
    )
  )
  expect_equal(result$probability_A + result$probability_B, rep(1, 4))
  expect_true(all(result$.prediction %in% c("A", "B")))
  expect_true(all(result$lower_A <= result$probability_A))
  parameters <- as_choice_parameters(
    model,
    draws = seq_len(prod(dim(model$draws)[1:2]))
  )
  scalar <- lapply(parameters, function(parameter) {
    probability <- choicedata::compute_choice_probabilities(
      choice_parameters = parameter,
      choice_data = model$data,
      choice_effects = model$model$effects,
      choice_only = FALSE,
      aggregate = "occasion"
    )
    as.matrix(probability[, c("A", "B")])
  })
  expect_equal(
    unname(as.matrix(result[, c("probability_A", "probability_B")])),
    unname(Reduce(`+`, scalar) / length(scalar))
  )
  mixed_model <- make_test_fit(random_effects = "x")
  conditional <- predict(mixed_model, type = "conditional")
  population <- predict(mixed_model, type = "population")
  expect_false(isTRUE(all.equal(
    conditional$probability_A, population$probability_A
  )))
  unknown <- model.frame(mixed_model)
  unknown$deciderID[1L] <- 99L
  expect_error(
    predict(mixed_model, newdata = unknown, type = "conditional"),
    "fitted deciders"
  )
  compact_model <- make_test_fit(
    random_effects = "x", save_individual_draws = FALSE
  )
  expect_error(
    predict(compact_model, type = "conditional"),
    "save_individual_draws"
  )
  expect_error(predict(model, level = 2), "between zero and one")
  latent <- fit(
    choice ~ x + z | 0,
    latent_class_effects = "x",
    classes = 2L,
    n_deciders = 6L,
    n_occasions = 2L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  latent_conditional <- predict(latent, type = "conditional")
  latent_population <- predict(latent, type = "population")
  expect_equal(
    latent_conditional$probability_A + latent_conditional$probability_B,
    rep(1, 12L)
  )
  expect_false(isTRUE(all.equal(
    latent_conditional$probability_A, latent_population$probability_A
  )))
})

test_that("residuals.RprobitB_fit returns choice-indicator residuals", {
  model <- make_test_fit(data.frame(
    deciderID = 1:3,
    choice = c("A", "B", NA),
    x_A = c(0, 1, 0),
    x_B = c(1, 0, 1)
  ))
  result <- residuals(model)
  expect_identical(dim(result), c(3L, 2L))
  expect_identical(colnames(result), c("A", "B"))
  expect_equal(unname(rowSums(result[1:2, , drop = FALSE])), c(0, 0))
  expect_true(all(is.na(result[3L, ])))
})

test_that("conditional prediction combines random and fixed class effects", {
  model <- fit(
    choice ~ x + z | 0,
    random_effects = "x",
    latent_class_effects = c("x", "z"),
    classes = 2L,
    n_deciders = 6L,
    n_occasions = 2L,
    save_individual_draws = TRUE,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  result <- predict(model, type = "conditional")
  expect_identical(nrow(result), 12L)
  expect_equal(result$probability_A + result$probability_B, rep(1, 12L))
  expect_true(all(result$probability_A >= 0 & result$probability_A <= 1))
})

test_that("predict numbers the deciders of new data without identifiers", {
  data <- data.frame(
    choice = c("A", "B", "A", "B"),
    x_A = c(0, 1, 0, 1),
    x_B = c(1, 0, 1, 0)
  )
  model <- fit(
    choice ~ x | 0, data = data, column_decider = NULL,
    iterations = 20L, warmup = 10L, chains = 1L, progress = FALSE
  )
  prediction <- predict(model, newdata = data.frame(x_A = 0:1, x_B = 1:0))
  expect_identical(nrow(prediction), 2L)
  expect_true("deciderID" %in% names(prediction))
})

test_that("predict keeps the factor levels of the fitted data", {
  data <- data.frame(
    deciderID = 1:6,
    choice = c("A", "B", "A", "B", "A", "B"),
    x_A = c(0, 1, 2, 0, 1, 2),
    x_B = c(2, 1, 0, 1, 0, 2)
  )
  model <- fit(
    choice ~ factor(x) | 0, data = data,
    iterations = 20L, warmup = 10L, chains = 1L, progress = FALSE
  )
  expect_identical(
    names(coef(model))[1:2], c("beta[factor(x)1]", "beta[factor(x)2]")
  )
  prediction <- predict(
    model, newdata = data.frame(deciderID = 1, x_A = 0, x_B = 1)
  )
  expect_identical(nrow(prediction), 1L)
})
