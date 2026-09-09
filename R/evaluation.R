#' Extract the fitted log-likelihood
#'
#' @description
#' Evaluates the decider-level log-likelihood at the posterior mean parameters.
#'
#' @param object \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @inheritParams WAIC
#'
#' @param ... Currently not used.
#'
#' @return A scalar object of class `logLik`. The `df` attribute counts the free
#' population-level parameters after normalization, and `nobs` is the number of
#' independent likelihood units.
#'
#' @export
#' @keywords models
#'
#' @examples
#' model <- fit(choice ~ x | 0, chains = 1)
#' logLik(model)

logLik.RprobitB_fit <- function(object, ghk_draws = 500L, ...) {

  # input checks
  check_fit(object)
  check_ghk_draws(ghk_draws)

  # the likelihood at the posterior mean and the parameter block sizes
  parameters <- as_choice_parameters(object)
  likelihood <- choicedata::choice_likelihood(
    choice_data = object$data,
    choice_effects = object$model$effects
  )
  contributions <- choicedata::compute_choice_likelihood(
    choice_parameters = parameters,
    choice_likelihood = likelihood,
    logarithm = TRUE,
    aggregate = "decider",
    ghk_draws = ghk_draws
  )
  random <- !is.na(object$model$effects$mixing)
  in_lc <- object$model$effects$effect_name %in%
    object$model$latent_class_effects
  P_f <- sum(!random & !in_lc)
  P_l <- sum(in_lc & !random)
  correlated <- startsWith(
    as.character(object$model$effects$mixing[random]), "c"
  )
  specific <- in_lc[random]
  C <- if (is.list(parameters$beta)) length(parameters$beta) else 1L
  J <- length(object$model$alternatives)

  # the normalization fixes one variance or threshold parameter
  specific_covariances <- sum(specific) + choose(sum(correlated & specific), 2L)
  shared_covariances <- sum(!specific) + choose(sum(correlated & !specific), 2L)
  covariance_parameters <- if (identical(object$model$choice_type, "ordered")) {
    J - 2L
  } else {
    (J - 1L) * J / 2L - 1L
  }
  df <- P_f + C * (P_l + sum(specific)) + sum(!specific) +
    C * specific_covariances + shared_covariances +
    max(0L, C - 1L) + covariance_parameters

  # the log-likelihood with its degrees of freedom
  structure(
    sum(contributions),
    class = "logLik",
    df = as.integer(df),
    nobs = length(contributions)
  )
}

#' Compute the widely applicable information criterion
#'
#' @description
#' Computes WAIC from posterior log-likelihood draws using [loo::waic()].
#'
#' @param object \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param progress \[`logical(1)`\]\cr
#' Show progress?
#'
#' @param ghk_draws \[`integer(1)`\]\cr
#' Number of draws of the GHK simulator for multivariate normal probabilities
#' of more than three dimensions, see [oeli::pmvnorm()].
#'
#' @param ... Further arguments passed to [loo::waic()].
#'
#' @return A `waic` object from [**loo**](https://mc-stan.org/loo/). Its
#' `estimates` matrix contains WAIC, effective parameter counts, and their
#' standard errors.
#'
#' @references
#' \insertRef{Watanabe2010}{RprobitB}
#'
#' \insertRef{Vehtari2017}{RprobitB}
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(choice ~ x | 0, n_occasions = 5, chains = 1)
#' WAIC(model)

WAIC <- function(object, ghk_draws = 500L, progress = interactive(), ...) {
  check_fit(object)
  oeli::input_check_response(checkmate::check_flag(progress), "progress")
  loo::waic(t(log_likelihood_draws(object, ghk_draws, progress)), ...)
}

#' Compute approximate leave-one-out cross-validation
#'
#' @description
#' Computes Pareto-smoothed importance-sampling leave-one-out cross-validation
#' using [loo::loo()].
#'
#' @param x \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @inheritParams WAIC
#'
#' @param ... Further arguments passed to [loo::loo()].
#'
#' @return A `psis_loo` object from [**loo**](https://mc-stan.org/loo/). It
#' contains estimates and standard errors as well as one Pareto-k diagnostic per
#' independent likelihood unit.
#'
#' @references
#' \insertRef{Vehtari2017}{RprobitB}
#'
#' \insertRef{Vehtari2024}{RprobitB}
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(choice ~ x | 0, n_occasions = 5, chains = 1)
#' loo(model)

loo.RprobitB_fit <- function(
  x, ghk_draws = 500L, progress = interactive(), ...
) {
  check_fit(x, "x")
  oeli::input_check_response(checkmate::check_flag(progress), "progress")
  loo::loo(t(log_likelihood_draws(x, ghk_draws, progress)), ...)
}

#' Compare models with a Bayes factor
#'
#' @description
#' Estimates both marginal likelihoods with bridge sampling and compares the
#' first model with the second model using [bridgesampling::bridge_sampler()]
#' and [bridgesampling::bf()].
#'
#' @param model1,model2 \[`RprobitB_fit`\]\cr
#' Fitted models to compare.
#'
#' @param log \[`logical(1)`\]\cr
#' Return the logarithm of the Bayes factor?
#'
#' @param repetitions \[`integer(1)`\]\cr
#' Number of independent bridge-sampling repetitions.
#'
#' @inheritParams WAIC
#'
#' @return A `bf_bridge` object from
#' [**bridgesampling**](https://CRAN.R-project.org/package=bridgesampling).
#' Values greater than one favor `model1`; values below one favor `model2`.
#'
#' @references
#' \insertRef{Gronau2020}{RprobitB}
#'
#' @export
#' @keywords models
#'
#' @examples
#' ### Simulate and fit the correctly specified model
#' set.seed(1)
#' correct_model <- fit(
#'   choice ~ x + z | 0,
#'   dgp_parameters = list(beta = c(x = 1, z = 0.5)),
#'   chains = 1
#' )
#' simulated_data <- as.data.frame(correct_model$data)
#'
#' ### Fit the same data again, but omit the relevant regressor z
#' misspecified_model <- fit(
#'   choice ~ x | 0,
#'   data = simulated_data,
#'   chains = 1
#' )
#'
#' ### A Bayes factor greater than one favors the correct first model
#' bayes_factor(correct_model, misspecified_model)

bayes_factor <- function(
  model1, model2, log = FALSE, repetitions = 1L, ghk_draws = 500L
) {

  # input checks
  models <- list(model1 = model1, model2 = model2)
  for (name in names(models)) {
    check_fit(models[[name]], name)
    model <- models[[name]]$model
    single_class <- identical(model$latent_classes$update, "fixed") &&
      model$latent_classes$initial == 1L
    if (!single_class) {
      oeli::input_check_response(
        "Must be a fixed single-class model for bridge sampling.", name
      )
    }
  }
  oeli::input_check_response(checkmate::check_flag(log), "log")
  oeli::input_check_response(
    checkmate::check_int(repetitions, lower = 1L), "repetitions"
  )
  check_ghk_draws(ghk_draws)

  # the marginal likelihood of each model is estimated by bridge sampling
  bridges <- list()
  for (name in names(models)) {
    object <- models[[name]]
    effects <- object$model$effects
    prior <- object$prior
    ordered <- identical(object$model$choice_type, "ordered")
    base <- if (ordered) NA_integer_ else object$model$normalization$level$level
    J <- length(object$model$alternatives)
    random <- !is.na(effects$mixing)
    random_correlated <- startsWith(as.character(effects$mixing[random]), "c")
    likelihood <- choicedata::choice_likelihood(
      choice_data = object$data,
      choice_effects = effects
    )

    # change normalization to unit variance of first utility difference
    draws <- seq_len(prod(dim(object$draws)[1:2]))
    parameters <- as_choice_parameters(object, draws = draws)
    lognormal <- sub("^c", "", as.character(effects$mixing[random])) != "n"
    shifted <- c(rep(FALSE, sum(!random)), lognormal)
    exponent <- outer(!lognormal, !lognormal, `+`)
    for (i in if (ordered) integer() else seq_along(parameters)) {
      draw <- parameters[[i]]
      factor <- 1 / sqrt(oeli::diff_cov(draw$Sigma, ref = 1L)[1L, 1L])
      betas <- if (is.list(draw$beta)) draw$beta else list(draw$beta)
      for (k in seq_along(betas)) {
        betas[[k]][!shifted] <- betas[[k]][!shifted] * factor
        betas[[k]][shifted] <- betas[[k]][shifted] + log(factor)
      }
      Omega <- draw$Omega
      if (is.list(Omega)) {
        for (k in seq_along(Omega)) Omega[[k]] <- Omega[[k]] * factor^exponent
      } else if (!is.null(Omega)) {
        Omega <- Omega * factor^exponent
      }
      parameters[[i]] <- choicedata::choice_parameters(
        beta = if (is.list(draw$beta)) betas else betas[[1L]],
        Omega = Omega,
        Sigma = draw$Sigma * factor^2,
        weights = draw$weights
      )
    }

    # the posterior draws on the unconstrained parameter space
    transformed <- lapply(parameters, function(parameter) {
      unclass(choicedata::switch_parameter_space(
        choice_parameters = parameter,
        choice_effects = effects
      ))
    })
    template <- transformed[[1L]]
    consistent <- vapply(transformed, function(value) {
      identical(names(value), names(template))
    }, logical(1))
    if (!all(consistent)) {
      cli::cli_abort(
        "Posterior draws have incompatible parameter dimensions.", call = NULL
      )
    }
    samples <- do.call(rbind, transformed)
    colnames(samples) <- names(template)
    varying <- apply(samples, 2L, stats::var) > sqrt(.Machine$double.eps)
    if (!any(varying)) {
      cli::cli_abort(
        "Bridge sampling requires at least one varying parameter.", call = NULL
      )
    }
    free <- names(template)[varying]
    if (!ordered) {
      reference <- oeli::delta(ref = 1L, dim = J) %*% diag(J)[, -base]
      error_scale <- reference %*% prior$error_covariance_scale %*% t(reference)
    }

    # compute unnormalized log posterior density
    log_posterior <- function(theta, ...) {
      optimization <- template
      optimization[free] <- theta
      proposal <- tryCatch(
        choicedata::switch_parameter_space(optimization, effects),
        error = function(error) NULL
      )
      if (is.null(proposal)) {
        return(-Inf)
      }
      log_likelihood <- tryCatch(
        choicedata::compute_choice_likelihood(
          choice_parameters = proposal,
          choice_likelihood = likelihood,
          logarithm = TRUE,
          aggregate = "total",
          ghk_draws = ghk_draws
        ),
        error = function(error) -Inf
      )
      if (!is.finite(log_likelihood)) {
        return(-Inf)
      }
      beta <- proposal$beta
      log_prior <- 0
      covariances <- list()
      if (any(!random)) {
        log_prior <- log_prior + oeli::dmvnorm(
          beta[!random], prior$fixed_mean, prior$fixed_covariance, log = TRUE
        )
      }
      if (any(random)) {
        log_prior <- log_prior + oeli::dmvnorm(
          beta[random], prior$random_mean, prior$random_mean_covariance,
          log = TRUE
        )
        blocks <- c(
          if (any(random_correlated)) list(which(random_correlated)),
          as.list(which(!random_correlated))
        )
        for (block in blocks) {
          covariances[[length(covariances) + 1L]] <- list(
            matrix = proposal$Omega[block, block, drop = FALSE],
            scale = prior$random_covariance_scale[block, block, drop = FALSE],
            df = prior$random_covariance_df,
            fixed = 0L
          )
        }
      }
      if (ordered) {
        increments <- log(diff(proposal$gamma))
        if (length(increments)) {
          log_prior <- log_prior + oeli::dmvnorm(
            increments, prior$threshold_mean, prior$threshold_covariance,
            log = TRUE
          )
        }
      } else {
        covariances[[length(covariances) + 1L]] <- list(
          matrix = oeli::diff_cov(proposal$Sigma, ref = 1L),
          scale = error_scale,
          df = prior$error_covariance_df,
          fixed = 1L
        )
      }
      for (covariance in covariances) {
        log_prior <- log_prior + oeli::dwishart(
          covariance$matrix, df = covariance$df, scale = covariance$scale,
          log = TRUE, inv = TRUE
        )
        dimension <- nrow(covariance$matrix)
        if (dimension > covariance$fixed) {
          indices <- seq.int(covariance$fixed + 1L, dimension)
          diagonal <- diag(chol(covariance$matrix))
          log_prior <- log_prior + (dimension - covariance$fixed) * log(2) +
            sum((dimension + 1L - indices) * log(diagonal[indices]))
        }
      }
      as.numeric(log_likelihood + log_prior)
    }
    bridges[[name]] <- bridgesampling::bridge_sampler(
      samples = samples[, varying, drop = FALSE],
      log_posterior = log_posterior,
      lb = stats::setNames(rep(-Inf, length(free)), free),
      ub = stats::setNames(rep(Inf, length(free)), free),
      repetitions = as.integer(repetitions),
      method = "normal",
      cores = 1L,
      silent = TRUE
    )
  }

  # the Bayes factor from the bridge sampling estimates of both models
  factor <- bridgesampling::bf(bridges$model1, bridges$model2, log = log)
  attr(factor, "model_names") <- names(models)
  factor
}
