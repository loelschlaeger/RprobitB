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
#' independent likelihood units as counted by [nobs()].
#'
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), chains = 1
#' )
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
    nobs = stats::nobs(object)
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
#' model <- fit(
#'   choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), n_occasions = 5,
#'   chains = 1
#' )
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
#' decider.
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
#' model <- fit(
#'   choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), n_occasions = 5,
#'   chains = 1
#' )
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
#' Estimates both marginal likelihoods with bridge sampling (Meng and Wong
#' 1996; Frühwirth-Schnatter 2004) and compares the first model with the second
#' model using [bridgesampling::bf()].
#'
#' @param model1,model2 \[`RprobitB_fit`\]\cr
#' Fitted models to compare.
#'
#' @param log \[`logical(1)`\]\cr
#' Return the logarithm of the Bayes factor?
#'
#' @param repetitions \[`integer(1)`\]\cr
#' Number of bridge-sampling repetitions.
#'
#' @inheritParams WAIC
#'
#' @return A `bf_bridge` object from
#' [**bridgesampling**](https://CRAN.R-project.org/package=bridgesampling).
#' Values greater than one (zero if `log = TRUE`) favor `model1`; smaller
#' values favor `model2`.
#'
#' @references
#' \insertRef{Meng1996}{RprobitB}
#'
#' \insertRef{FruehwirthSchnatter2004}{RprobitB}
#'
#' \insertRef{Gronau2020}{RprobitB}
#'
#' \insertRef{Celeux2019}{RprobitB}
#'
#' \insertRef{FruehwirthSchnatter2019b}{RprobitB}
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
#'   iterations = 500,
#'   chains = 1
#' )
#' simulated_data <- as.data.frame(correct_model$data)
#'
#' ### Fit the same data again, but omit the relevant regressor z
#' misspecified_model <- fit(
#'   choice ~ x | 0,
#'   data = simulated_data,
#'   iterations = 500,
#'   chains = 1
#' )
#'
#' ### A Bayes factor greater than one favors the correct first model
#' bayes_factor(correct_model, misspecified_model)

bayes_factor <- function(
  model1, model2, log = FALSE, repetitions = 1L, ghk_draws = 500L,
  progress = interactive()
) {

  # input checks
  models <- list(model1 = model1, model2 = model2)
  for (name in names(models)) {
    check_fit(models[[name]], name)
    model <- models[[name]]$model
    if (!identical(model$latent_classes$update, "fixed")) {
      oeli::input_check_response(
        "Must be a model with a fixed number of classes for bridge sampling.",
        name
      )
    }
  }
  oeli::input_check_response(checkmate::check_flag(log), "log")
  oeli::input_check_response(
    checkmate::check_int(repetitions, lower = 1L), "repetitions"
  )
  check_ghk_draws(ghk_draws)
  oeli::input_check_response(checkmate::check_flag(progress), "progress")

  # the marginal likelihood of each model is estimated by bridge sampling,
  # which evaluates the log posterior at half of the draws and at as many
  # proposals per repetition
  draws_total <- prod(dim(model1$draws)[1:2]) + prod(dim(model2$draws)[1:2])
  steps <- ceiling(draws_total * (1 + repetitions) / 2)
  evaluations <- 0L
  bridges <- progressr::with_progress(
    {
      progressor <- progressr::progressor(steps = steps)
      bridges <- list()
      for (name in names(models)) {
        object <- models[[name]]
        effects <- object$model$effects
        prior <- object$prior
        ordered <- identical(object$model$choice_type, "ordered")
        base <- if (ordered) {
          NA_integer_
        } else {
          object$model$normalization$level$level
        }
        J <- length(object$model$alternatives)
        if (!ordered && J > 2L && base > 2L) {
          cli::cli_abort(
            "Bridge sampling requires one of the first two alternatives as
            {.var base} of {.var {name}}.",
            call = NULL
          )
        }
        random <- !is.na(effects$mixing)
        random_correlated <- startsWith(
          as.character(effects$mixing[random]), "c"
        )
        latent_class <- as.logical(effects$latent_class)
        nonrandom_index <- which(!random)
        alpha_index <- nonrandom_index[!latent_class[!random]]
        lambda_index <- nonrandom_index[latent_class[!random]]
        random_index <- which(random)
        specific <- latent_class[random]
        classes <- max(
          sum(startsWith(dimnames(object$draws)$variable, "weight[")), 1L
        )
        covariance_blocks <- function(index) {
          correlated <- index[random_correlated[index]]
          c(
            if (length(correlated)) list(correlated),
            as.list(index[!random_correlated[index]])
          )
        }
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
        for (i in seq_along(parameters)) {
          draw <- parameters[[i]]
          variance <- if (ordered) {
            as.numeric(draw$Sigma)[1L]
          } else {
            oeli::diff_cov(draw$Sigma, ref = 1L)[1L, 1L]
          }
          factor <- 1 / sqrt(variance)
          betas <- if (is.list(draw$beta)) draw$beta else list(draw$beta)
          for (k in seq_along(betas)) {
            betas[[k]][!shifted] <- betas[[k]][!shifted] * factor
            betas[[k]][shifted] <- betas[[k]][shifted] + log(factor)
          }
          Omega <- draw$Omega
          if (is.list(Omega)) {
            for (k in seq_along(Omega)) {
              Omega[[k]] <- Omega[[k]] * factor^exponent
            }
          } else if (!is.null(Omega)) {
            Omega <- Omega * factor^exponent
          }
          parameters[[i]] <- choicedata::choice_parameters(
            beta = if (is.list(draw$beta)) betas else betas[[1L]],
            Omega = Omega,
            Sigma = draw$Sigma * factor^2,
            gamma = if (ordered) draw$gamma * factor,
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
            "Posterior draws have incompatible parameter dimensions.",
            call = NULL
          )
        }
        samples <- do.call(rbind, transformed)
        colnames(samples) <- names(template)
        varying <- apply(samples, 2L, stats::var) > sqrt(.Machine$double.eps)
        if (!any(varying)) {
          cli::cli_abort(
            "Bridge sampling requires at least one varying parameter.",
            call = NULL
          )
        }
        free <- names(template)[varying]
        if (!ordered) {
          reference <- oeli::delta(ref = 1L, dim = J) %*% diag(J)[, -base]
          error_scale <- reference %*% prior$error_covariance_scale %*%
            t(reference)
        }

        # compute unnormalized log posterior density
        log_posterior <- function(theta, ...) {
          evaluations <<- evaluations + 1L
          amount <- progress_amount(evaluations, steps)
          if (amount > 0) {
            progressor(
              amount = amount,
              message = paste0("Bridge sampling (", name, ")")
            )
          }
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
          betas <- if (is.list(proposal$beta)) {
            proposal$beta
          } else {
            list(proposal$beta)
          }
          omegas <- if (is.list(proposal$Omega)) {
            proposal$Omega
          } else {
            list(proposal$Omega)
          }
          log_prior <- 0
          covariances <- list()
          if (length(alpha_index)) {
            log_prior <- log_prior + oeli::dmvnorm(
              betas[[1L]][alpha_index], prior$fixed_mean,
              prior$fixed_covariance, log = TRUE
            )
          }
          if (length(lambda_index)) {
            for (beta in betas) {
              log_prior <- log_prior + oeli::dmvnorm(
                beta[lambda_index], prior$latent_class_mean,
                prior$latent_class_covariance, log = TRUE
              )
            }
          }
          add_covariances <- function(Omega, index) {
            for (block in covariance_blocks(index)) {
              covariances[[length(covariances) + 1L]] <<- list(
                matrix = Omega[block, block, drop = FALSE],
                scale = prior$random_covariance_scale[
                  block, block,
                  drop = FALSE
                ],
                df = prior$random_covariance_df,
                fixed = 0L
              )
            }
          }
          common <- which(!specific)
          if (length(common)) {
            log_prior <- log_prior + oeli::dmvnorm(
              betas[[1L]][random_index[common]], prior$random_mean[common],
              prior$random_mean_covariance[common, common, drop = FALSE],
              log = TRUE
            )
            add_covariances(omegas[[1L]], common)
          }
          class_specific <- which(specific)
          if (length(class_specific)) {
            for (k in seq_along(betas)) {
              log_prior <- log_prior + oeli::dmvnorm(
                betas[[k]][random_index[class_specific]],
                prior$random_mean[class_specific],
                prior$random_mean_covariance[
                  class_specific, class_specific,
                  drop = FALSE
                ],
                log = TRUE
              )
              add_covariances(omegas[[k]], class_specific)
            }
          }
          if (length(betas) > 1L) {
            # Dirichlet prior of the weights and the Jacobian of the softmax
            weights <- proposal$weights
            concentration <- prior$class_concentration
            log_prior <- log_prior + lgamma(length(weights) * concentration) -
              length(weights) * lgamma(concentration) +
              (concentration - 1) * sum(log(weights)) + sum(log(weights))
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
            dimension <- nrow(covariance$matrix)
            log_prior <- log_prior + if (covariance$fixed == 0L) {
              oeli::dwishart(
                covariance$matrix, df = covariance$df,
                scale = covariance$scale, log = TRUE, inv = TRUE
              )
            } else {
              df <- covariance$df
              df / 2 * determinant(covariance$scale)$modulus +
                lgamma(dimension * df / 2) -
                dimension * (dimension - 1) / 4 * log(pi) -
                sum(lgamma(df / 2 + (1 - seq_len(dimension)) / 2)) -
                (df + dimension + 1) / 2 *
                  determinant(covariance$matrix)$modulus -
                dimension * df / 2 * log(sum(diag(
                  covariance$scale %*% solve(covariance$matrix)
                )))
            }
            if (dimension > covariance$fixed) {
              indices <- seq.int(covariance$fixed + 1L, dimension)
              diagonal <- diag(chol(covariance$matrix))
              log_prior <- log_prior + (dimension - covariance$fixed) * log(2) +
                sum((dimension + 1L - indices) * log(diagonal[indices]))
            }
          }
          as.numeric(log_likelihood + log_prior)
        }
        if (classes == 1L) {
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
          next
        }

        ### balanced proposal for latent classes
        if (classes > 6L) {
          cli::cli_abort(
            "Bridge sampling with all class permutations is limited to at most
            6 classes.",
            call = NULL
          )
        }
        orders <- matrix(1L, 1L, 1L)
        for (k in seq_len(classes)[-1L]) {
          orders <- do.call(rbind, lapply(seq_len(k), function(position) {
            t(apply(orders, 1L, append, values = k, after = position - 1L))
          }))
        }
        block <- regmatches(free, regexec("^(beta|o)_([0-9]+)_([0-9]+)$", free))
        weight <- regmatches(free, regexec("^w_([0-9]+)$", free))
        mappings <- lapply(seq_len(nrow(orders)), function(row) {
          order <- orders[row, ]
          mapping <- matrix(0, length(free), length(free))
          for (j in seq_along(free)) {
            if (length(block[[j]])) {
              source <- match(sprintf(
                "%s_%d_%s", block[[j]][2L], order[as.integer(block[[j]][3L])],
                block[[j]][4L]
              ), free)
              if (is.na(source)) {
                cli::cli_abort(
                  "The class-specific parameters must vary in every class.",
                  call = NULL
                )
              }
              mapping[j, source] <- 1
            } else if (length(weight[[j]])) {
              # log weight ratios to the first class are shifted linearly
              for (term in list(
                c(order[as.integer(weight[[j]][2L])], 1), c(order[1L], -1)
              )) {
                if (term[1L] > 1) {
                  source <- match(sprintf("w_%d", term[1L]), free)
                  mapping[j, source] <- mapping[j, source] + term[2L]
                }
              }
            } else {
              mapping[j, j] <- 1
            }
          }
          mapping
        })
        fitting <- seq_len(round(nrow(samples) / 2))
        posterior_draws <- samples[-fitting, varying, drop = FALSE]
        center <- colMeans(samples[fitting, varying, drop = FALSE])
        covariance <- stats::cov(samples[fitting, varying, drop = FALSE])
        ridge <- 0
        repeat {
          root <- tryCatch(
            t(chol(covariance + ridge * diag(length(free)))),
            error = function(error) NULL
          )
          if (!is.null(root)) break
          ridge <- max(2 * ridge, 1e-10 * mean(diag(covariance)))
        }
        log_proposal <- function(points) {
          log_densities <- vapply(mappings, function(mapping) {
            centered <- t(points %*% t(mapping)) - center
            standardized <- forwardsolve(root, centered)
            -0.5 * colSums(standardized^2) - sum(log(diag(root))) -
              0.5 * length(free) * log(2 * pi)
          }, numeric(nrow(points)))
          log_densities <- matrix(log_densities, nrow = nrow(points))
          largest <- apply(log_densities, 1L, max)
          largest + log(rowMeans(exp(log_densities - largest)))
        }

        ### iterative scheme of Meng and Wong (1996)
        q11 <- apply(posterior_draws, 1L, log_posterior)
        q12 <- log_proposal(posterior_draws)
        q11[is.na(q11)] <- -Inf
        neff <- stats::median(apply(posterior_draws, 2L, posterior::ess_basic))
        if (!is.finite(neff)) neff <- nrow(posterior_draws)
        logml <- niter <- numeric(repetitions)
        for (repetition in seq_len(repetitions)) {
          proposals <- t(center + root %*% matrix(
            stats::rnorm(length(posterior_draws)), nrow = length(free)
          ))
          q21 <- apply(proposals, 1L, log_posterior)
          q21[is.na(q21)] <- -Inf
          q22 <- log_proposal(proposals)
          l1 <- q11 - q12
          l2 <- q21 - q22
          lstar <- stats::median(l1[is.finite(l1)])
          s1 <- neff / (neff + length(l2))
          s2 <- length(l2) / (neff + length(l2))
          r <- 0.5
          for (iteration in seq_len(1000L)) {
            numerator <- 1 / (s1 + s2 * r * exp(lstar - l2))
            denominator <- 1 / (s1 * exp(l1 - lstar) + s2 * r)
            update <- mean(numerator) / mean(denominator)
            converged <- abs((update - r) / update) < 1e-10
            r <- update
            if (converged) break
          }
          logml[repetition] <- if (converged) log(r) + lstar else NA_real_
          niter[repetition] <- iteration
        }
        if (anyNA(logml)) {
          cli::cli_warn(
            "The iterative scheme of bridge sampling did not converge.",
            call = NULL
          )
        }
        bridges[[name]] <- structure(
          list(
            logml = logml, niter = niter, method = "normal",
            repetitions = repetitions
          ),
          class = if (repetitions == 1L) "bridge" else "bridge_list"
        )
      }
      bridges
    },
    enable = progress
  )

  # the Bayes factor from the bridge sampling estimates of both models
  factor <- bridgesampling::bf(bridges$model1, bridges$model2, log = log)
  attr(factor, "model_names") <- names(models)
  factor
}
