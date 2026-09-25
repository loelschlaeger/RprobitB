test_that("fit handles empirical and simulated data", {
  empirical_data <- data.frame(
    deciderID = 1:4,
    choice = c("A", "B", "A", "B"),
    x_A = c(0, 1, 0, 1),
    x_B = c(1, 0, 1, 0)
  )
  empirical <- fit(
    choice ~ x | 0,
    data = empirical_data,
    iterations = 20L,
    warmup = 10L,
    progress = FALSE
  )
  simulated <- fit(
    choice ~ x | 0,
    n_deciders = 4L,
    iterations = 20L,
    warmup = 10L,
    progress = FALSE
  )
  mixing <- c(
    normal_correlated = "cn",
    normal_independent = "n",
    log_correlated = "cln",
    log_independent = "ln",
    negative_log_correlated = "cln-",
    negative_log_independent = "ln-"
  )
  random_model <- fit(
    choice ~ normal_correlated + normal_independent + log_correlated +
      log_independent + negative_log_correlated + negative_log_independent | 0,
    random_effects = mixing,
    n_deciders = 4L,
    n_occasions = 2L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    save_individual_draws = TRUE,
    progress = FALSE
  )
  ordered_model <- fit(
    choice ~ x,
    alternatives = c("low", "middle", "high"),
    choice_type = "ordered",
    random_effects = c(ASC = "n"),
    n_deciders = 4L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  expect_identical(ordered_model$model$effects$effect_name, c("x", "ASC"))
  ranked_model <- fit(
    choice ~ x | 0,
    choice_type = "ranked",
    n_deciders = 4L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  long_model <- fit(
    choice ~ x | 0,
    data = data.frame(
      deciderID = rep(1:3, each = 3L),
      alternative = rep(c("A", "B", "C"), 3L),
      choice = rep(c(1L, 0L, 0L), 3L),
      x = seq_len(9L)
    ),
    format = "long",
    column_alternative = "alternative",
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  effect_model <- fit(
    choice ~ price | income | comfort,
    data = data.frame(
      deciderID = 1:3,
      choice = c("A", "B", "C"),
      price_A = c(1, 2, 3), price_B = c(2, 3, 1),
      price_C = c(3, 1, 2), income = 0:2,
      comfort_A = c(1, 0, 1), comfort_B = c(0, 1, 1),
      comfort_C = c(1, 1, 0)
    ),
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  unbalanced_model <- fit(
    choice ~ x | 0,
    data = data.frame(
      deciderID = c(1L, rep(2L, 3L)),
      occasionID = c(1L, 1:3),
      choice = c("A", "B", NA, "A"),
      x_A = c(0, 1, 0, 1),
      x_B = c(1, 0, 1, 0)
    ),
    random_effects = "x",
    column_occasion = "occasionID",
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  finite_mixture <- fit(
    choice ~ x | 0,
    random_effects = "x",
    latent_class_effects = "x",
    classes = 2L,
    n_deciders = 5L,
    n_occasions = 2L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  sparse_mixture <- fit(
    choice ~ x | 0,
    random_effects = "x",
    latent_class_effects = "x",
    classes = 4L,
    class_update = "sparse",
    n_deciders = 8L,
    n_occasions = 2L,
    iterations = 30L,
    warmup = 10L,
    chains = 1L,
    progress = FALSE
  )
  weight_based_mixture <- fit(
    choice ~ x | 0,
    random_effects = "x",
    latent_class_effects = "x",
    class_update = "weight_based",
    weight_based_control = list(buffer = 2L),
    n_deciders = 8L,
    n_occasions = 2L,
    iterations = 8L,
    warmup = 4L,
    chains = 1L,
    progress = FALSE
  )
  set.seed(1)
  dynamic_mixture <- suppressWarnings(fit(
    choice ~ x | 0,
    random_effects = "x",
    latent_class_effects = "x",
    classes = 2L,
    class_update = "dirichlet_process",
    max_classes = 4L,
    prior = list(class_concentration = c(shape = 2, rate = 4)),
    n_deciders = 8L,
    n_occasions = 2L,
    iterations = 40L,
    warmup = 10L,
    chains = 1L,
    progress = FALSE
  ))
  expect_s3_class(empirical, "RprobitB_fit")
  expect_named(
    empirical,
    c("call", "data", "model", "prior", "draws", "sampler", "simulation")
  )
  expect_identical(empirical$model$responses, c("A", "B", "A", "B"))
  expect_identical(unbalanced_model$model$responses, c("A", "B", NA, "A"))
  expect_s3_class(empirical$data, "choice_data")
  expect_s3_class(empirical$model$effects, "choice_effects")
  expect_identical(as.character(empirical$model$alternatives), c("A", "B"))
  expect_s3_class(empirical$draws, "draws_array")
  expect_identical(dim(empirical$draws), c(10L, 4L, 2L))
  expect_identical(
    dimnames(empirical$draws)$variable,
    c("beta[x]", "Sigma[B,B]")
  )
  expect_identical(attr(empirical$draws, "warmup"), 10L)
  expect_identical(attr(empirical$draws, "thin"), 1L)
  expect_s3_class(simulated, "RprobitB_fit")
  expect_s3_class(simulated$simulation$dgp_parameters, "choice_parameters")
  expect_null(empirical$simulation)
  expect_identical(empirical$model$normalization$level$name, "A")
  expect_identical(
    as.character(random_model$model$effects$mixing), unname(mixing)
  )
  expect_true(all(is.finite(random_model$draws)))
  expect_true(any(startsWith(
    dimnames(random_model$draws)$variable, "individual["
  )))
  expect_s3_class(ordered_model, "RprobitB_fit")
  expect_s3_class(ranked_model, "RprobitB_fit")
  expect_s3_class(long_model, "RprobitB_fit")
  expect_s3_class(effect_model, "RprobitB_fit")
  expect_s3_class(unbalanced_model, "RprobitB_fit")
  expect_s3_class(finite_mixture, "RprobitB_fit")
  expect_s3_class(sparse_mixture, "RprobitB_fit")
  expect_s3_class(weight_based_mixture, "RprobitB_fit")
  expect_identical(
    sparse_mixture$model$latent_classes,
    list(
      initial = 4L,
      update = "sparse",
      maximum = 4L
    )
  )
  finite_allocations <- posterior::as_draws_matrix(finite_mixture$draws)[
    , startsWith(dimnames(finite_mixture$draws)$variable, "class["),
    drop = FALSE
  ]
  expect_true(all(apply(
    finite_allocations, 1L, function(value) length(unique(value)) == 2L
  )))
  sparse_draws <- posterior::as_draws_matrix(sparse_mixture$draws)
  expect_true(all(sparse_draws[, "n_classes"] >= 1L))
  expect_true(all(sparse_draws[, "n_classes"] <= 4L))
  expect_true(all(sparse_draws[, "class_concentration"] > 0))
  expect_identical(
    weight_based_mixture$model$latent_classes$update, "weight_based"
  )
  expect_identical(
    weight_based_mixture$model$latent_classes$control,
    list(
      buffer = 2L, epsmin = 0.01, epsmax = 0.7,
      deltamin = 0.1, deltashift = 0.5
    )
  )
  expect_identical(
    dim(weight_based_mixture$model$latent_classes$history),
    c(
      weight_based_mixture$sampler$iterations,
      weight_based_mixture$sampler$chains
    )
  )
  expect_true(all(weight_based_mixture$model$latent_classes$changes %in% 0:3))
  weight_based_draws <- posterior::as_draws_matrix(
    weight_based_mixture$draws
  )[, "n_classes"]
  expect_length(unique(weight_based_draws), 1L)
  expect_true(all(weight_based_draws >= 1L & weight_based_draws <= 10L))
  class_draws <- posterior::as_draws_matrix(dynamic_mixture$draws)[
    , "n_classes"
  ]
  expect_true(length(unique(class_draws)) > 1L)
  expect_true(all(class_draws >= 1L & class_draws <= 4L))
  expect_true(all(posterior::as_draws_matrix(dynamic_mixture$draws)[
    , "class_concentration"
  ] > 0))
  random_parameters <- as_choice_parameters(random_model)
  expect_s3_class(random_parameters, "choice_parameters")
  correlated <- startsWith(mixing, "c")
  covariance_mask <- outer(correlated, correlated, `&`)
  diag(covariance_mask) <- TRUE
  expect_true(all(random_parameters$Omega[!covariance_mask] == 0))
  preferences <- choicedata::generate_choice_preferences(
    choice_effects = random_model$model$effects,
    choice_parameters = random_parameters,
    choice_identifiers = choicedata::generate_choice_identifiers(N = 20L)
  )
  expect_true(all(preferences$log_correlated > 0))
  expect_true(all(preferences$log_independent > 0))
  expect_true(all(preferences$negative_log_correlated < 0))
  expect_true(all(preferences$negative_log_independent < 0))
  choice_sets <- fit(
    choice ~ x | 0,
    data = data.frame(
      deciderID = c(1L, 1L, 2L, 2L, 2L, 3L, 3L),
      alternative = c("A", "B", "A", "B", "C", "B", "C"),
      choice = c(1L, 0L, 0L, 0L, 1L, 1L, 0L),
      x = c(0.5, 1, 0, 1.5, 1, 0.2, 0.8)
    ),
    format = "long",
    column_alternative = "alternative",
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  choice_set_prediction <- predict(choice_sets)
  probability_columns <- paste0("probability_", c("A", "B", "C"))
  expect_s3_class(choice_sets, "RprobitB_fit")
  expect_identical(choice_set_prediction$probability_C[1L], 0)
  expect_identical(choice_set_prediction$probability_A[3L], 0)
  expect_equal(
    rowSums(choice_set_prediction[, probability_columns]), rep(1, 3)
  )
  expect_error(
    fit(
      rank ~ x | 0,
      data = data.frame(
        deciderID = c(1L, 1L, 1L, 2L, 2L),
        alternative = c("A", "B", "C", "A", "B"),
        rank = c(1L, 2L, 3L, 2L, 1L),
        x = 1:5
      ),
      format = "long",
      column_alternative = "alternative",
      choice_type = "ranked",
      iterations = 4L,
      warmup = 2L,
      chains = 1L,
      progress = FALSE
    ),
    "all model alternatives"
  )
  expect_error(fit(choice ~ x, data = 1:3), "data[.]frame")
  expect_error(
    fit(choice ~ x | 0, random_effects = c(x = "unsupported")),
    "subset"
  )
  expect_error(
    fit(choice ~ x + z | 0, random_effects = c(x = "cn", "z")),
    "at least 1 characters"
  )
  expect_error(
    fit(
      choice ~ x + z | 0,
      random_effects = c(x = "n", z = "n"),
      prior = list(random_covariance_scale = matrix(c(1, 0.1, 0.1, 1), 2L))
    ),
    "Must be zero"
  )
  expect_error(
    fit(choice ~ x | 0, data = empirical_data, scale = "Sigma_1,1 := 1"),
    "type 'number'"
  )
  expect_error(
    fit(
      choice ~ x | 0,
      class_update = "weight",
      random_effects = "x"
    ),
    "Must be element of set"
  )
  expect_error(
    fit(
      choice ~ x | 0,
      weight_based_control = list(buffer = 10L)
    ),
    "only used for `class_update = \"weight_based\"`"
  )
  expect_error(
    fit(
      choice ~ x | 0,
      random_effects = "x",
      class_update = "weight_based",
      weight_based_control = list(buffer = 3L),
      iterations = 4L,
      warmup = 2L
    ),
    "weight_based_control[$]buffer"
  )
  expect_error(
    fit(
      choice ~ x | 0,
      random_effects = "x",
      class_update = "sparse"
    ),
    "at least two"
  )
  old_handlers <- progressr::handlers()
  on.exit(progressr::handlers(old_handlers), add = TRUE)
  progressr::handlers("debug")
  progress_output <- capture.output(
    progress_model <- fit(
      choice ~ x | 0,
      n_deciders = 2L,
      iterations = 2L,
      warmup = 1L,
      chains = 2L,
      progress = TRUE
    ),
    type = "message"
  )
  expect_s3_class(progress_model, "RprobitB_fit")
  expect_match(paste(progress_output, collapse = "\n"), "chain 1")
  expect_match(paste(progress_output, collapse = "\n"), "chain 2")
})

test_that("fit estimates latent class effects", {
  set.seed(1)
  pure <- fit(
    choice ~ price + time | 0,
    latent_class_effects = "price",
    classes = 2L,
    n_deciders = 20L,
    n_occasions = 3L,
    dgp_parameters = list(
      beta = list(c(price = -2, time = 1), c(price = 0.5, time = 1)),
      weights = c(0.6, 0.4)
    ),
    iterations = 20L,
    warmup = 10L,
    chains = 1L,
    progress = FALSE
  )
  mixed <- fit(
    choice ~ price + time | 0,
    random_effects = "time",
    latent_class_effects = "price",
    classes = 2L,
    n_deciders = 10L,
    n_occasions = 2L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  sparse <- fit(
    choice ~ price + time | 0,
    latent_class_effects = "price",
    classes = 3L,
    class_update = "sparse",
    n_deciders = 10L,
    n_occasions = 2L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  dynamic <- suppressWarnings(fit(
    choice ~ price + time | 0,
    latent_class_effects = "price",
    classes = 2L,
    class_update = "dirichlet_process",
    max_classes = 3L,
    n_deciders = 10L,
    n_occasions = 2L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  ))
  constant <- fit(
    choice ~ price | 1,
    latent_class_effects = "ASC",
    classes = 2L,
    n_deciders = 10L,
    n_occasions = 2L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  common <- fit(
    choice ~ x + z | 0,
    random_effects = "x",
    latent_class_effects = "z",
    classes = 2L,
    n_deciders = 6L,
    n_occasions = 2L,
    iterations = 10L,
    warmup = 5L,
    chains = 1L,
    progress = FALSE
  )
  variables <- dimnames(pure$draws)$variable
  expect_true(all(
    c("beta[time]", "weight[1]", "beta[price,1]", "beta[price,2]") %in%
      variables
  ))
  expect_false(any(startsWith(variables, "mu[")))
  expect_identical(pure$model$latent_class_effects, "price")
  expect_identical(pure$prior$latent_class_mean, 0)
  expect_identical(pure$prior$latent_class_covariance, matrix(10))
  expect_true("mu[time]" %in% dimnames(mixed$draws)$variable)
  expect_true("beta[price,2]" %in% dimnames(mixed$draws)$variable)
  expect_true("n_classes" %in% dimnames(sparse$draws)$variable)
  expect_true("n_classes" %in% dimnames(dynamic$draws)$variable)
  expect_true(all(
    c("beta[ASC_B,1]", "beta[ASC_B,2]") %in% dimnames(constant$draws)$variable
  ))
  truth <- common$simulation$dgp_parameters
  expect_identical(truth$beta[[1L]][["x"]], truth$beta[[2L]][["x"]])
  expect_identical(truth$Omega[[1L]], truth$Omega[[2L]])
  expect_true(all(
    c("mu[x]", "Omega[x,x]", "beta[z,1]", "beta[z,2]") %in%
      dimnames(common$draws)$variable
  ))
  expect_false("mu[x,1]" %in% dimnames(common$draws)$variable)
  expect_error(
    fit(
      choice ~ price + time | 0,
      latent_class_effects = "price",
      n_deciders = 4L,
      iterations = 4L,
      warmup = 2L,
      chains = 1L,
      progress = FALSE
    ),
    "more than one class"
  )
  expect_error(
    fit(
      choice ~ price + time | 0,
      random_effects = "price",
      classes = 2L,
      n_deciders = 4L,
      iterations = 4L,
      warmup = 2L,
      chains = 1L,
      progress = FALSE
    ),
    "latent_class_effects"
  )
  expect_error(
    fit(
      choice ~ price + time | 0,
      latent_class_effects = "comfort",
      classes = 2L,
      n_deciders = 4L,
      iterations = 4L,
      warmup = 2L,
      chains = 1L,
      progress = FALSE
    ),
    "latent_class_effects"
  )
  expect_error(
    fit(
      choice ~ price + time | 0,
      latent_class_effects = "price",
      class_update = "weight_based",
      weight_based_control = list(buffer = 2L),
      n_deciders = 4L,
      iterations = 4L,
      warmup = 2L,
      chains = 1L,
      progress = FALSE
    ),
    "requires a random effect"
  )
  expect_error(
    fit(
      choice ~ x + z | 0,
      random_effects = "x",
      latent_class_effects = "x",
      classes = 2L,
      dgp_parameters = list(beta = list(c(z = 1, x = 1), c(z = 2, x = 1))),
      n_deciders = 4L,
      iterations = 4L,
      warmup = 2L,
      chains = 1L,
      progress = FALSE
    ),
    "must not differ by class"
  )
})

test_that("fit warns about correlated effects across the class blocks", {
  expect_warning(
    fit(
      choice ~ x + z | 0,
      random_effects = c("x", "z"),
      latent_class_effects = "z",
      classes = 2L,
      n_deciders = 6L,
      n_occasions = 2L,
      iterations = 10L,
      warmup = 5L,
      chains = 1L,
      progress = FALSE
    ),
    "uncorrelated"
  )
})

test_that("fit warns when the class count reaches max_classes", {
  expect_warning(
    fit(
      choice ~ x | 0,
      random_effects = "x",
      latent_class_effects = "x",
      class_update = "dirichlet_process",
      max_classes = 1L,
      n_deciders = 8L,
      n_occasions = 2L,
      iterations = 20L,
      warmup = 10L,
      chains = 1L,
      progress = FALSE
    ),
    "maximum of 1 latent classes"
  )
})
