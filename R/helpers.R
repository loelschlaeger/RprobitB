cross_product_sum <- function(matrices, P, J, ordered) {
  initial <- matrix(0, nrow = P^2, ncol = if (ordered) 1L else (J - 1L)^2)
  Reduce(`+`, lapply(matrices, function(x) {
    if (ordered) {
      x <- as.numeric(x)
      t(kronecker(t(x), t(x)))
    } else {
      kronecker(t(x), t(x))
    }
  }), init = initial)
}

check_fit <- function(x, var_name = "object") {
  oeli::input_check_response(
    checkmate::check_class(x, "RprobitB_fit"), var_name
  )
  invisible(x)
}

check_probability <- function(x, var_name = "level") {
  oeli::input_check_response(
    checkmate::check_number(x, finite = TRUE), var_name
  )
  if (x <= 0 || x >= 1) {
    oeli::input_check_response(
      "Must be strictly between zero and one.", var_name
    )
  }
  invisible(x)
}

check_ghk_draws <- function(x, var_name = "ghk_draws") {
  oeli::input_check_response(checkmate::check_int(x, lower = 2L), var_name)
  invisible(x)
}

check_finite_matrix <- function(x, var_name) {
  oeli::input_check_response(
    checkmate::check_matrix(x, mode = "numeric", any.missing = FALSE),
    var_name
  )
  oeli::input_check_response(
    checkmate::check_numeric(as.numeric(x), finite = TRUE, any.missing = FALSE),
    var_name
  )
  invisible(x)
}

check_finite_numeric <- function(
  x, var_name, len = NULL, min.len = NULL, lower = -Inf, upper = Inf
) {
  oeli::input_check_response(
    oeli::check_numeric_vector(
      x,
      lower = lower, upper = upper, finite = TRUE, any.missing = FALSE,
      len = len, min.len = min.len
    ),
    var_name
  )
  invisible(x)
}

check_matrix_dimensions <- function(x, rows, columns, var_name) {
  oeli::input_check_response(
    checkmate::check_matrix(
      x,
      mode = "numeric", any.missing = FALSE, nrows = rows, ncols = columns
    ),
    var_name
  )
  invisible(x)
}

select_columns <- function(design, columns) {
  if (!length(columns)) {
    return(NA)
  }
  lapply(design, function(x) x[, columns, drop = FALSE])
}

summarize_draws <- function(values, level) {
  alpha <- (1 - level) / 2
  data.frame(
    mean = mean(values, na.rm = TRUE),
    sd = stats::sd(values, na.rm = TRUE),
    lower = unname(stats::quantile(values, alpha, na.rm = TRUE)),
    upper = unname(stats::quantile(values, 1 - alpha, na.rm = TRUE))
  )
}

posterior_mode <- function(x) {
  x <- as.vector(x)
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  tolerance <- sqrt(.Machine$double.eps) * max(1, abs(mean(x)))
  if (diff(range(x)) <= tolerance) return(mean(x))
  rounded <- round(x)
  if (all(abs(x - rounded) <= tolerance)) x <- rounded
  values <- sort(unique(x))
  if (all(x == round(x))) {
    frequencies <- tabulate(match(x, values), nbins = length(values))
    return(values[which.max(frequencies)])
  }
  estimate <- stats::density(x, from = min(x), to = max(x))
  estimate$x[which.max(estimate$y)]
}

individual_variables <- function(object) {
  variables <- dimnames(object$draws)$variable
  variables[startsWith(variables, "individual[")]
}

log_likelihood_draws <- function(object, ghk_draws = 500L, progress = FALSE) {

  # input checks
  check_fit(object)
  check_ghk_draws(ghk_draws)

  # one parameter set per posterior draw and the likelihood of the data
  draws <- seq_len(prod(dim(object$draws)[1:2]))
  parameters <- as_choice_parameters(object, draws = draws)
  iteration <- dimnames(object$draws)$iteration
  chain <- dimnames(object$draws)$chain
  draw_names <- unlist(lapply(chain, function(chain_id) {
    paste0("chain_", chain_id, ".iteration_", iteration)
  }))
  names(parameters) <- draw_names[draws]
  likelihood <- choicedata::choice_likelihood(
    choice_data = object$data,
    choice_effects = object$model$effects
  )

  # every posterior draw contributes one column of decider likelihoods
  columns <- progressr::with_progress(
    {
      progressor <- progressr::progressor(steps = length(parameters))
      future.apply::future_lapply(
        seq_along(parameters),
        function(draw) {
          value <- choicedata::compute_choice_likelihood(
            choice_parameters = parameters[draw],
            choice_likelihood = likelihood,
            logarithm = TRUE,
            aggregate = "decider",
            ghk_draws = ghk_draws
          )
          progressor(
            message = paste0(
              "Log-likelihood of draw ", draw, " of ", length(parameters)
            )
          )
          value
        },
        future.seed = TRUE
      )
    },
    enable = progress
  )

  # the decider likelihoods of all draws as one matrix
  first <- columns[[1L]]
  deciders <- if (is.null(dim(first))) names(first) else rownames(first)
  matrix(
    unlist(columns),
    ncol = length(parameters),
    dimnames = list(deciders, names(parameters))
  )
}

label_mapping <- function(allocation, reference, classes) {
  used <- sort(unique(c(allocation, reference)))
  score <- unclass(table(
    factor(allocation, levels = used),
    factor(reference, levels = used)
  ))

  # solve the assignments
  n <- length(used)
  states <- 2^n
  values <- rep(-Inf, states)
  values[1L] <- 0
  parents <- matrix(NA_integer_, nrow = n, ncol = states)
  for (row in seq_len(n)) {
    updated <- rep(-Inf, states)
    for (state in which(is.finite(values))) {
      mask <- state - 1L
      for (column in seq_len(n)) {
        bit <- bitwShiftL(1L, column - 1L)
        if (bitwAnd(mask, bit) != 0L) next
        new_mask <- bitwOr(mask, bit)
        candidate <- values[state] + score[row, column]
        if (candidate > updated[new_mask + 1L]) {
          updated[new_mask + 1L] <- candidate
          parents[row, new_mask + 1L] <- column
        }
      }
    }
    values <- updated
  }
  assignment <- integer(n)
  mask <- states - 1L
  for (row in rev(seq_len(n))) {
    column <- parents[row, mask + 1L]
    assignment[row] <- column
    mask <- bitwXor(mask, bitwShiftL(1L, column - 1L))
  }

  # labels that are not used map to themselves
  mapping <- seq_len(classes)
  mapping[used] <- used[assignment]
  mapping
}

transform_chain_draws <- function(
  samples, warmup, thin, effects, normalization, decider_ids, class_update
) {
  retained <- seq.int(warmup + 1L, nrow(samples$Sigma), by = thin)
  for (name in names(samples)) {
    value <- samples[[name]]
    if (is.null(value)) next
    samples[[name]] <- if (is.matrix(value)) {
      value[retained, , drop = FALSE]
    } else {
      value[retained]
    }
  }

  # the model dimensions
  alternatives <- attr(effects, "choice_alternatives")
  J <- length(alternatives)
  ordered <- isTRUE(attr(alternatives, "ordered"))
  random <- !is.na(effects$mixing)
  lognormal <- sub("^c", "", as.character(effects$mixing[random])) != "n"
  specific <- effects$latent_class[random]
  lc <- effects$latent_class & !random
  fixed_names <- effects$effect_name[!random & !lc]
  random_names <- effects$effect_name[random]
  P_r <- length(random_names)
  C <- if (is.null(samples$s)) 0L else ncol(samples$s)
  N <- length(decider_ids)

  # the draws are brought to the utility scale of the normalization
  scale <- normalization$scale
  if (identical(scale$parameter, "alpha")) {
    utility_scale <- scale$value / samples$alpha[, scale$index]
  } else {
    position <- (scale$index - 1L) * (J - 1L) + scale$index
    utility_scale <- sqrt(scale$value / samples$Sigma[, position])
  }
  if (any(lognormal) && any(!is.finite(utility_scale) | utility_scale <= 0)) {
    cli::cli_abort(
      paste(
        "Log-normal random effects require a positive utility-scale",
        "normalization."
      ),
      call = NULL
    )
  }
  if (!is.null(samples$alpha)) samples$alpha <- samples$alpha * utility_scale
  if (!is.null(samples$lambda)) samples$lambda <- samples$lambda * utility_scale
  samples$Sigma <- samples$Sigma * utility_scale^2
  if (P_r) {
    # normal effects are scaled, log-normal effects are shifted on the log scale
    shifted <- rep(lognormal, C)
    samples$b[, !shifted] <- samples$b[, !shifted, drop = FALSE] * utility_scale
    samples$b[, shifted] <- samples$b[, shifted, drop = FALSE] +
      log(utility_scale)
    exponent <- rep(as.numeric(outer(!lognormal, !lognormal, `+`)), C)
    samples$Omega <- samples$Omega * outer(utility_scale, exponent, `^`)
    if (!is.null(samples$beta)) {
      beta <- do.call(rbind, lapply(samples$beta, as.numeric))
      shifted <- rep(lognormal, N)
      beta[, !shifted] <- beta[, !shifted, drop = FALSE] * utility_scale
      beta[, shifted] <- beta[, shifted, drop = FALSE] + log(utility_scale)
      samples$beta <- beta
    }
  }

  # weights and parameters of unoccupied classes are marked as missing
  if (C > 1L && !is.null(samples$z)) {
    occupied <- t(apply(samples$z, 1L, tabulate, nbins = C)) > 0L
    samples$s[!occupied] <- NA_real_
    for (name in c("b", "Omega", "lambda")) {
      if (is.null(samples[[name]])) next
      size <- ncol(samples[[name]]) %/% C
      samples[[name]][!occupied[, rep(seq_len(C), each = size)]] <- NA_real_
    }
  }

  # the retained draws as one matrix of named posterior variables
  difference <- if (ordered) {
    character()
  } else {
    as.character(alternatives)[-normalization$level$level]
  }
  cs <- which(specific)
  co <- which(!specific)
  cs_pairs <- expand.grid(row = cs, column = cs)
  co_pairs <- expand.grid(row = co, column = co)
  offsets <- seq_len(C) - 1L
  components <- list(
    alpha = samples$alpha,
    s = samples$s,
    lambda = samples$lambda,
    b_specific = if (P_r) {
      samples$b[, as.vector(outer(cs, offsets * P_r, `+`)), drop = FALSE]
    },
    b_common = if (P_r) samples$b[, co, drop = FALSE],
    Omega_specific = if (P_r) {
      samples$Omega[, as.vector(outer(
        (cs_pairs$column - 1L) * P_r + cs_pairs$row, offsets * P_r^2, `+`
      )), drop = FALSE]
    },
    Omega_common = if (P_r) {
      samples$Omega[, (co_pairs$column - 1L) * P_r + co_pairs$row, drop = FALSE]
    },
    beta = samples$beta,
    delta = if (!is.null(samples$delta)) matrix(samples$delta, ncol = 1L),
    Sigma = samples$Sigma,
    gamma = if (ordered) {
      t(apply(samples$d, 1L, d_to_gamma))[, -c(1L, J + 1L), drop = FALSE] *
        utility_scale
    },
    z = samples$z,
    n_classes = if (C && !identical(class_update, "fixed")) {
      matrix(samples$class_sequence, ncol = 1L)
    }
  )
  names_of <- list(
    alpha = sprintf("beta[%s]", fixed_names),
    s = sprintf("weight[%s]", seq_len(C)),
    lambda = sprintf(
      "beta[%s,%s]", rep(effects$effect_name[lc], C),
      rep(seq_len(C), each = sum(lc))
    ),
    b_specific = sprintf(
      "mu[%s,%s]", rep(random_names[cs], C), rep(seq_len(C), each = length(cs))
    ),
    b_common = sprintf("mu[%s]", random_names[co]),
    Omega_specific = sprintf(
      "Omega[%s,%s,%s]", rep(random_names[cs_pairs$row], C),
      rep(random_names[cs_pairs$column], C),
      rep(seq_len(C), each = nrow(cs_pairs))
    ),
    Omega_common = sprintf(
      "Omega[%s,%s]", random_names[co_pairs$row], random_names[co_pairs$column]
    ),
    beta = sprintf(
      "individual[%s,%s]", rep(random_names, N), rep(decider_ids, each = P_r)
    ),
    delta = "class_concentration",
    Sigma = if (ordered) {
      "Sigma"
    } else {
      sprintf(
        "Sigma[%s,%s]", rep(difference, J - 1L), rep(difference, each = J - 1L)
      )
    },
    gamma = sprintf("gamma[%s]", seq_len(J - 1L)),
    z = sprintf("class[%s]", decider_ids),
    n_classes = "n_classes"
  )
  for (name in names(components)) {
    if (!is.null(components[[name]])) {
      colnames(components[[name]]) <- names_of[[name]]
    }
  }
  draws <- do.call(cbind, components)
  attr(draws, "iteration") <- retained
  draws
}

posterior_variables <- function(object, individual = FALSE) {
  variables <- dimnames(object$draws)$variable
  model <- object$model
  effects <- model$effects
  ordered <- identical(model$choice_type, "ordered")
  scale <- model$normalization$scale
  difference <- if (ordered) {
    character()
  } else {
    as.character(model$alternatives)[-model$normalization$level$level]
  }
  fixed <- c(
    if (identical(scale$parameter, "alpha")) {
      sprintf("beta[%s]", scale$name)
    } else if (ordered) {
      "Sigma"
    } else {
      sprintf("Sigma[%s,%s]", difference[scale$index], difference[scale$index])
    },
    if (ordered) "gamma[1]",
    if (model$latent_classes$maximum == 1L) "weight[1]"
  )

  # covariances are reported once and only between correlated effects
  random <- !is.na(effects$mixing)
  random_names <- effects$effect_name[random]
  correlated <- startsWith(as.character(effects$mixing[random]), "c")
  specific <- effects$latent_class[random]
  C <- model$latent_classes$maximum
  P_r <- length(random_names)
  reported <- outer(correlated, correlated, `&`) | diag(P_r) > 0
  reported <- reported & outer(specific, specific, `==`) &
    upper.tri(reported, diag = TRUE)
  pairs <- which(reported, arr.ind = TRUE)
  common <- !specific[pairs[, 1L]]
  upper <- which(
    upper.tri(diag(length(difference)), diag = TRUE), arr.ind = TRUE
  )
  kept <- c(
    sprintf(
      "Omega[%s,%s]", random_names[pairs[common, 1L]],
      random_names[pairs[common, 2L]]
    ),
    sprintf(
      "Omega[%s,%s,%s]", rep(random_names[pairs[!common, 1L]], C),
      rep(random_names[pairs[!common, 2L]], C),
      rep(seq_len(C), each = sum(!common))
    ),
    if (ordered) {
      "Sigma"
    } else {
      sprintf("Sigma[%s,%s]", difference[upper[, 1L]], difference[upper[, 2L]])
    }
  )
  covariance <- startsWith(variables, "Omega[") | startsWith(variables, "Sigma")
  omitted <- c(fixed, variables[covariance & !variables %in% kept])

  # the remaining variables, without the allocations and unoccupied classes
  keep <- !startsWith(variables, "class[") & !variables %in% omitted &
    apply(!is.na(object$draws), 3L, any)
  if (!individual) keep <- keep & !startsWith(variables, "individual[")
  variables[keep]
}

as_choice_parameters <- function(object, draws = NULL) {

  # input checks
  check_fit(object)
  oeli::input_check_response(
    checkmate::check_integerish(
      draws, lower = 1L, upper = prod(dim(object$draws)[1:2]),
      any.missing = FALSE, null.ok = TRUE
    ),
    "draws"
  )

  # the posterior variables of every parameter block, by class
  effects <- object$model$effects
  alternatives <- as.character(object$model$alternatives)
  J <- length(alternatives)
  ordered <- identical(object$model$choice_type, "ordered")
  base <- if (ordered) NA_integer_ else object$model$normalization$level$level
  random <- !is.na(effects$mixing)
  lc <- effects$latent_class & !random
  fixed_names <- effects$effect_name[!random & !lc]
  lc_names <- effects$effect_name[lc]
  random_names <- effects$effect_name[random]
  specific <- effects$latent_class[random]
  P_r <- length(random_names)
  variables <- dimnames(object$draws)$variable
  C <- sum(startsWith(variables, "weight["))
  classes <- max(C, 1L)
  weight_names <- sprintf("weight[%s]", seq_len(C))
  fixed_variables <- sprintf("beta[%s]", fixed_names)
  mu_names <- matrix(sprintf("mu[%s]", random_names), P_r, classes)
  mu_names[specific, ] <- sprintf(
    "mu[%s,%s]", rep(random_names[specific], classes),
    rep(seq_len(classes), each = sum(specific))
  )
  pair_row <- rep(random_names, P_r)
  pair_column <- rep(random_names, each = P_r)
  both_specific <- as.vector(outer(specific, specific, `&`))
  both_common <- as.vector(outer(!specific, !specific, `&`))
  omega_names <- array(
    sprintf("Omega[%s,%s]", pair_row, pair_column), c(P_r, P_r, classes)
  )
  omega_names[array(both_specific, c(P_r, P_r, classes))] <- sprintf(
    "Omega[%s,%s,%s]", rep(pair_row[both_specific], classes),
    rep(pair_column[both_specific], classes),
    rep(seq_len(classes), each = sum(both_specific))
  )
  omega_names[array(!(both_specific | both_common), c(P_r, P_r, classes))] <- NA
  difference <- if (ordered) character() else alternatives[-base]
  sigma_names <- sprintf(
    "Sigma[%s,%s]", rep(difference, J - 1L), rep(difference, each = J - 1L)
  )
  gamma_names <- sprintf("gamma[%s]", seq_len(J - 1L))

  # the posterior means or the requested draws
  samples <- posterior::as_draws_matrix(object$draws)
  values <- if (is.null(draws)) {
    means <- colMeans(samples, na.rm = TRUE)
    weights <- unclass(samples)[, weight_names, drop = FALSE]
    weights[is.na(weights)] <- 0
    means[weight_names] <- colMeans(weights)
    matrix(means, nrow = 1L, dimnames = list(NULL, colnames(samples)))
  } else {
    samples[draws, , drop = FALSE]
  }

  # one parameter set per row, where only the occupied classes enter
  parameters <- vector("list", nrow(values))
  for (i in seq_len(nrow(values))) {
    row <- stats::setNames(as.numeric(values[i, ]), colnames(values))
    weights <- unname(row[weight_names])
    active <- if (C) which(weights > 0) else 1L
    if (!length(active)) {
      cli::cli_abort(
        "A posterior draw contains no active latent class.", call = NULL
      )
    }
    beta <- vector("list", length(active))
    Omega <- vector("list", length(active))
    for (k in seq_along(active)) {
      class <- active[k]
      coefficients <- numeric(length(fixed_names) + length(lc_names))
      coefficients[!lc[!random]] <- row[fixed_variables]
      coefficients[lc[!random]] <- row[sprintf("beta[%s,%s]", lc_names, class)]
      beta[[k]] <- c(coefficients, unname(row[mu_names[, class]]))
      block <- matrix(0, P_r, P_r)
      block_names <- omega_names[, , class]
      block[!is.na(block_names)] <- row[block_names[!is.na(block_names)]]
      Omega[[k]] <- block
    }
    beta <- if (!length(beta[[1L]])) {
      NULL
    } else if (length(active) == 1L) {
      beta[[1L]]
    } else {
      beta
    }
    Omega <- if (!P_r) {
      NULL
    } else if (length(active) == 1L) {
      Omega[[1L]]
    } else {
      Omega
    }
    if (ordered) {
      Sigma <- unname(row["Sigma"])
      gamma <- unname(row[gamma_names])
    } else {
      Sigma <- oeli::undiff_cov(
        matrix(row[sigma_names], J - 1L, J - 1L), ref = base
      )
      gamma <- NULL
    }
    parameters[[i]] <- choicedata::validate_choice_parameters(
      choice_parameters = choicedata::choice_parameters(
        beta = beta,
        Omega = Omega,
        Sigma = Sigma,
        gamma = gamma,
        weights = if (length(active) > 1L) {
          weights[active] / sum(weights[active])
        }
      ),
      choice_effects = effects,
      allow_missing = FALSE
    )
  }
  if (is.null(draws)) {
    parameters[[1L]]
  } else {
    stats::setNames(parameters, paste0("draw_", draws))
  }
}

probability_draws <- function(
  object, prediction_data, type = "population", ghk_draws = 500L,
  progress = FALSE
) {
  draws <- seq_len(prod(dim(object$draws)[1:2]))
  alternatives <- as.character(object$model$alternatives)
  effects <- object$model$effects
  random <- !is.na(effects$mixing)
  lc <- effects$effect_name %in% object$model$latent_class_effects
  parameters <- as_choice_parameters(object, draws = draws)

  # population probabilities integrate over the random coefficients
  if (!identical(type, "conditional") || !any(random | lc)) {
    block_size <- ceiling(length(draws) / 20)
    blocks <- split(draws, ceiling(seq_along(draws) / block_size))
    probabilities <- progressr::with_progress(
      {
        progressor <- progressr::progressor(steps = length(blocks))
        future.apply::future_lapply(
          blocks,
          function(block) {
            values <- choicedata::compute_choice_probabilities(
              choice_parameters = parameters[block],
              choice_data = prediction_data,
              choice_effects = effects,
              choice_only = FALSE,
              input_checks = FALSE,
              aggregate = "occasion",
              ghk_draws = ghk_draws
            )
            for (j in seq_along(values)) {
              values[[j]] <- as.matrix(
                values[[j]][, alternatives, drop = FALSE]
              )
            }
            progressor(
              message = paste0(
                "Choice probabilities of draws ", block[1L], " to ",
                block[length(block)]
              )
            )
            values
          },
          future.seed = TRUE
        )
      },
      enable = progress
    )
    return(unlist(probabilities, recursive = FALSE, use.names = FALSE))
  }

  # conditional probabilities need the individual draws of fitted deciders
  if (any(random) && !length(individual_variables(object))) {
    cli::cli_abort(
      paste(
        "Conditional prediction requires individual coefficient draws.",
        "Refit with {.code save_individual_draws = TRUE}."
      ),
      call = NULL
    )
  }
  identifiers <- choicedata::extract_choice_identifiers(prediction_data)
  column_decider <- object$model$data_roles$column_decider
  prediction_deciders <- unique(identifiers[[column_decider]])
  decider_index <- match(prediction_deciders, object$model$deciders)
  if (anyNA(decider_index)) {
    unknown <- prediction_deciders[is.na(decider_index)]
    oeli::input_check_response(
      paste0(
        "Conditional prediction is only available for fitted deciders. ",
        "Unknown: ", paste(unknown, collapse = ", "), "."
      ),
      "newdata"
    )
  }

  # the decider's own coefficients and class replace the population values
  samples <- posterior::as_draws_matrix(object$draws)
  random_names <- effects$effect_name[random]
  mixing <- sub("^c", "", as.character(effects$mixing[random]))
  conditional_effects <- effects
  conditional_effects$mixing[random] <- NA
  frame <- as.data.frame(prediction_data)
  by_decider <- progressr::with_progress(
    {
      progressor <- progressr::progressor(steps = length(prediction_deciders))
      future.apply::future_lapply(
        seq_along(prediction_deciders),
        function(i) {
          decider <- prediction_deciders[i]
          fitted <- object$model$deciders[decider_index[i]]
          variable_names <- sprintf("individual[%s,%s]", random_names, fitted)
          classes <- if (any(lc)) {
            as.integer(round(samples[draws, sprintf("class[%s]", fitted)]))
          }
          conditional_parameters <- vector("list", length(draws))
          for (j in seq_along(draws)) {
            shared <- parameters[[j]]
            fixed <- if (is.list(shared$beta)) {
              shared$beta[[1L]]
            } else {
              shared$beta
            }
            fixed <- fixed[!random]
            beta <- samples[draws[j], variable_names]
            beta[mixing == "ln"] <- exp(beta[mixing == "ln"])
            beta[mixing == "ln-"] <- -exp(beta[mixing == "ln-"])
            if (any(lc & !random)) {
              fixed[lc[!random]] <- as.numeric(samples[
                draws[j],
                sprintf(
                  "beta[%s,%s]", effects$effect_name[lc & !random], classes[j]
                )
              ])
            }
            conditional_parameters[[j]] <- choicedata::choice_parameters(
              beta = stats::setNames(c(fixed, beta), effects$effect_name),
              Sigma = shared$Sigma,
              gamma = shared$gamma
            )
          }
          decider_data <- as_prediction_data(
            object,
            frame[frame[[column_decider]] == decider, , drop = FALSE]
          )
          probabilities <- choicedata::compute_choice_probabilities(
            choice_parameters = conditional_parameters,
            choice_data = decider_data,
            choice_effects = conditional_effects,
            choice_only = FALSE,
            input_checks = FALSE,
            aggregate = "occasion",
            ghk_draws = ghk_draws
          )
          progressor(
            message = paste0("Conditional probabilities of decider ", decider)
          )
          probabilities
        },
        future.seed = TRUE
      )
    },
    enable = progress
  )

  # one probability matrix per draw
  template <- matrix(
    NA_real_,
    nrow = nrow(identifiers),
    ncol = length(alternatives),
    dimnames = list(NULL, alternatives)
  )
  probabilities <- rep(list(template), length(draws))
  for (i in seq_along(prediction_deciders)) {
    positions <- which(identifiers[[column_decider]] == prediction_deciders[i])
    for (j in seq_along(draws)) {
      probabilities[[j]][positions, ] <- as.matrix(
        by_decider[[i]][[j]][, alternatives, drop = FALSE]
      )
    }
  }
  probabilities
}

as_prediction_data <- function(object, newdata) {
  check_fit(object)
  oeli::input_check_response(
    checkmate::check_data_frame(newdata, null.ok = TRUE), "newdata"
  )
  if (is.null(newdata)) {
    return(object$data)
  }
  roles <- object$model$data_roles
  response <- all.vars(object$model$formula)[1L]
  ranked_wide <- identical(object$model$choice_type, "ranked") &&
    identical(roles$format, "wide")
  if (ranked_wide) {
    columns <- paste(response, object$model$alternatives, sep = roles$delimiter)
    for (column in setdiff(columns, names(newdata))) {
      newdata[[column]] <- NA_integer_
    }
  } else if (!response %in% names(newdata)) {
    newdata[[response]] <- NA
  }
  choicedata::choice_data(
    data_frame = newdata,
    format = roles$format,
    column_choice = response,
    column_decider = roles$column_decider,
    column_occasion = roles$column_occasion,
    column_alternative = roles$column_alternative,
    delimiter = roles$delimiter,
    choice_type = object$model$choice_type
  )
}
