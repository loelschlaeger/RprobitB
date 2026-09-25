#' Summarize a fitted choice model
#'
#' @description
#' Summarizes the marginal posterior distributions of the fitted model
#' parameters with selectable statistics.
#'
#' @param object \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param variables \[`character()` | `NULL`\]\cr
#' Posterior variables to summarize.
#'
#' @param statistics \[`character()`\]\cr
#' Posterior statistics to report, in this order. Available are `"mean"`,
#' `"median"`, `"mode"`, `"sd"`, `"mcse_mean"`, `"mcse_median"`, `"mcse_sd"`,
#' `"rhat"`, `"ess_bulk"`, and `"ess_tail"`, see the details.
#'
#' @param probs \[`numeric()` | `NULL`\]\cr
#' Optional unique probabilities for posterior quantiles.
#'
#' @param ... Currently not used.
#'
#' @return A `summary.RprobitB_fit` object.
#'
#' @details
#' Every statistic describes the marginal posterior of one variable and is
#' computed from the retained draws of all chains:
#'
#' - `mean`: the average of the draws, the usual point estimate.
#' - `median`: the median value of the draws. If it differs clearly from the
#'   mean, the posterior is skewed.
#' - `mode`: the most probable value. For continuous draws, it is the peak
#'   of a kernel density estimate; for integer-valued draws, such as the
#'   active class count, it is the most frequent value.
#' - `sd`: the standard deviation of the draws, the posterior uncertainty of
#'   the parameter.
#' - `q<100 * p>`: the quantile of probability `p`. With
#'   `probs = c(0.025, 0.975)`, the two columns are the limits of the 95%
#'   credible interval.
#' - `mcse_mean`, `mcse_median`, `mcse_sd`: the Monte Carlo standard errors
#'   of the mean, median, and standard deviation, the sampling error of
#'   these estimates that more iterations would reduce. Good values are
#'   below a tenth of `sd`; larger values mean that the reported digits are
#'   not yet reliable and the sampler should run longer.
#' - `rhat`: the rank-normalized, folded split-R-hat of Vehtari et al.
#'   (2021). It compares the variance between the halves of all chains with
#'   the variance within them. With a single chain, it compares the two
#'   halves of that chain. Good values are at most 1.01; larger values mean
#'   that the chains have not mixed and the sampler should run longer, see
#'   `plot(type = "trace")`.
#' - `ess_bulk`: the effective sample size for the center of the posterior,
#'   the number of independent draws that carry the same information as the
#'   correlated draws. Good values are at least 100 per chain; smaller values
#'   mean that the sampler should run longer.
#' - `ess_tail`: the smaller of the effective sample sizes of the 5% and 95%
#'   quantiles. It governs the precision of quantiles, credible intervals,
#'   and the standard deviation. The same rule applies: at least 100 per
#'   chain is good.
#'
#' @references
#' \insertRef{Vehtari2021}{RprobitB}
#'
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), chains = 1
#' )
#' summary(model)

summary.RprobitB_fit <- function(
  object, variables = NULL,
  statistics = c("mean", "mode", "sd", "rhat", "ess_bulk"), probs = NULL, ...
) {

  # input checks
  check_fit(object)
  available <- dimnames(object$draws)$variable
  if (is.null(variables)) variables <- posterior_variables(object)
  oeli::input_check_response(
    checkmate::check_character(
      variables,
      any.missing = FALSE, unique = TRUE
    ),
    "variables"
  )
  oeli::input_check_response(
    checkmate::check_subset(variables, choices = available), "variables"
  )
  oeli::input_check_response(
    checkmate::check_character(
      statistics,
      min.len = 1L, any.missing = FALSE, unique = TRUE
    ),
    "statistics"
  )
  oeli::input_check_response(
    checkmate::check_subset(
      statistics,
      choices = c(
        "mean", "median", "mode", "sd", "mcse_mean", "mcse_median",
        "mcse_sd", "rhat", "ess_bulk", "ess_tail"
      )
    ),
    "statistics"
  )
  if (!is.null(probs)) {
    oeli::input_check_response(
      checkmate::check_numeric(
        probs,
        lower = 0, upper = 1, finite = TRUE,
        any.missing = FALSE, min.len = 1L, unique = TRUE
      ),
      "probs"
    )
    if (any(probs == 0 | probs == 1)) {
      oeli::input_check_response(
        "Must be strictly between zero and one.", "probs"
      )
    }
  }
  draws <- object$draws[, , variables, drop = FALSE]

  # one column per statistic
  quantiles <- NULL
  if (!is.null(probs)) {
    quantiles <- matrix(
      apply(
        draws, 3L, stats::quantile, probs = probs, na.rm = TRUE, names = FALSE
      ),
      ncol = length(probs),
      byrow = TRUE,
      dimnames = list(NULL, paste0("q", 100 * probs))
    )
  }
  location <- c("mean", "median", "mode", "sd")
  after <- max(0L, which(statistics %in% location))
  posterior <- data.frame(variable = variables)
  if (after == 0L && !is.null(quantiles)) {
    posterior <- cbind(posterior, quantiles)
  }
  for (i in seq_along(statistics)) {
    posterior[[statistics[i]]] <- unname(switch(
      statistics[i],
      mean = apply(draws, 3L, mean, na.rm = TRUE),
      median = apply(draws, 3L, stats::median, na.rm = TRUE),
      mode = apply(draws, 3L, posterior_mode),
      sd = apply(draws, 3L, stats::sd, na.rm = TRUE),
      mcse_mean = apply(draws, 3L, posterior::mcse_mean),
      mcse_median = apply(draws, 3L, posterior::mcse_median),
      mcse_sd = apply(draws, 3L, posterior::mcse_sd),
      rhat = apply(draws, 3L, posterior::rhat),
      ess_bulk = apply(draws, 3L, posterior::ess_bulk),
      ess_tail = apply(draws, 3L, posterior::ess_tail)
    ))
    if (i == after && !is.null(quantiles)) {
      posterior <- cbind(posterior, quantiles)
    }
  }
  occupied <- unname(apply(!is.na(draws), 3L, mean))
  if (any(occupied < 1)) {
    posterior <- cbind(posterior[1L], occupied = occupied, posterior[-1L])
  }
  if (!is.null(object$simulation)) {
    dgp <- unname(object$simulation$dgp[variables])
    posterior <- cbind(posterior[1L], dgp = dgp, posterior[-1L])
  }

  # the summary object
  structure(
    list(
      call = object$call,
      formula = object$model$formula,
      observations = stats::nobs(object),
      chains = object$sampler$chains,
      retained_per_chain = object$sampler$retained_per_chain,
      posterior = posterior
    ),
    class = "summary.RprobitB_fit"
  )
}

#' Print a fitted model summary
#'
#' @description
#' Prints sampling information followed by the posterior summary table.
#'
#' @param x \[`summary.RprobitB_fit`\]\cr
#' Model summary returned by `summary()`.
#'
#' @param digits \[`integer(1)`\]\cr
#' Number of significant digits to print.
#'
#' @param ... Further arguments passed to `print.data.frame()`.
#'
#' @return `x`, invisibly.
#'
#' @export
#' @keywords models

print.summary.RprobitB_fit <- function(x, digits = 3L, ...) {
  oeli::input_check_response(
    checkmate::check_class(x, "summary.RprobitB_fit"), "x"
  )
  oeli::input_check_response(checkmate::check_int(digits, lower = 0L), "digits")
  cat("Bayesian probit choice model\n")
  cat("Formula:", deparse1(x$formula), "\n")
  cat(
    "Samples:", x$retained_per_chain, "retained per chain,",
    x$chains, if (x$chains == 1L) "chain\n" else "chains\n\n"
  )
  print.data.frame(
    as.data.frame(x$posterior),
    row.names = FALSE,
    digits = digits,
    ...
  )
  invisible(x)
}

#' Diagnose latent-class occupancy and membership
#'
#' @description
#' Computes label-invariant posterior summaries of a fitted mixture model: the
#' distribution of the occupied class count and the posterior probability that
#' every pair of deciders belongs to the same class. It also returns the
#' probability of every decider belonging to each relabeled class.
#'
#' @param object \[`RprobitB_fit`\]\cr
#' Fitted model with at least two finite classes, a sparse finite mixture,
#' a Dirichlet-process mixture, or a weight-based heuristic fit.
#'
#' @return A list with three elements:
#'
#' - `occupancy`: a `data.frame` with the columns `n_classes` and
#'   `probability`, with one row for every number of classes from one to the
#'   maximum number of classes of the model. It gives the share of draws in
#'   which exactly `n_classes` classes contain at least one decider, so it
#'   answers how many classes the data support. For a fixed mixture, the
#'   number of classes is fixed, and the table shows how often one of them
#'   stays empty.
#' - `co_clustering`: a square matrix with one row and one column per
#'   decider. Entry `[i, j]` is the share of draws in which deciders `i` and
#'   `j` are in the same class, so it answers whether two deciders behave
#'   alike. It does not depend on how the classes are labeled.
#' - `membership`: a matrix with one row per decider and one column per
#'   class, `class_1` to `class_<maximum>`. Entry `[i, k]` is the share of
#'   draws in which decider `i` is in class `k` after relabeling, so it
#'   answers which class a decider most likely belongs to.
#'
#' @references
#' \insertRef{Dahl2006}{RprobitB}
#'
#' \insertRef{Papastamoulis2010}{RprobitB}
#'
#' \insertRef{Stephens2000}{RprobitB}
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x | 0, random_effects = "x", latent_class_effects = "x",
#'   classes = 2, class_update = "dirichlet_process",
#'   dgp_parameters = list(
#'     beta = list(c(x = -1), c(x = 2)),
#'     Omega = list(matrix(0.2), matrix(0.2)),
#'     weights = c(0.6, 0.4)
#'   ),
#'   n_occasions = 5,
#'   chains = 1
#' )
#' diagnostics <- latent_class_diagnostics(model)
#' diagnostics$occupancy
#' diagnostics$co_clustering[1:5, 1:5]
#' head(diagnostics$membership)

latent_class_diagnostics <- function(object) {

  # input checks
  check_fit(object)
  variables <- dimnames(object$draws)$variable
  class_variables <- variables[startsWith(variables, "class[")]
  classes <- object$model$latent_classes$maximum
  if (!length(class_variables) || classes < 2L) {
    oeli::input_check_response(
      "Requires a mixture model with at least two components.", "object"
    )
  }

  # the class memberships and co-clusterings are averaged over the draws
  allocation <- round(unclass(posterior::as_draws_matrix(object$draws)))
  allocation <- allocation[, class_variables, drop = FALSE]
  deciders <- as.character(object$model$deciders)
  membership <- matrix(0, ncol(allocation), classes)
  co_clustering <- matrix(0, ncol(allocation), ncol(allocation))
  for (class in seq_len(classes)) {
    in_class <- allocation == class
    membership[, class] <- colMeans(in_class)
    co_clustering <- co_clustering + crossprod(in_class)
  }
  dimnames(membership) <- list(deciders, paste0("class_", seq_len(classes)))
  co_clustering <- co_clustering / nrow(allocation)
  dimnames(co_clustering) <- list(deciders, deciders)

  # the posterior distribution of the number of occupied classes
  occupied <- colSums(apply(allocation, 1L, tabulate, nbins = classes) > 0L)
  counts <- tabulate(occupied, nbins = classes)
  occupancy <- data.frame(
    n_classes = seq_len(classes),
    probability = counts / nrow(allocation)
  )
  list(
    occupancy = occupancy,
    co_clustering = co_clustering,
    membership = membership
  )
}

#' Extract posterior coefficient summaries
#'
#' @description
#' Returns posterior means or medians for population parameters or individual
#' random coefficients.
#'
#' @param object \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param type \[`character(1)`\]\cr
#' The posterior summary to return:
#'
#' - `"mean"` averages the draws.
#' - `"median"` is more robust for skewed posteriors.
#'
#' @param level \[`character(1)`\]\cr
#' Which parameters to return:
#'
#' - `"population"` returns the parameters shared by all deciders.
#' - `"individual"` returns the random coefficients of every decider, which
#'   requires a mixed model fitted with `save_individual_draws = TRUE`.
#'
#' @param ... Currently not used.
#'
#' @return For `level = "population"`, a named numeric vector with one value per
#' global posterior variable that is not fixed by the normalization or the model
#' structure. For `level = "individual"`, a numeric matrix with deciders in rows
#' and random effects in columns. Log-normal coefficients remain on their latent
#' normal scale.
#'
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x | 0,
#'   random_effects = "x",
#'   dgp_parameters = list(beta = c(x = 1), Omega = matrix(0.5)),
#'   chains = 1,
#'   save_individual_draws = TRUE
#' )
#' coef(model)
#' head(coef(model, level = "individual"))

coef.RprobitB_fit <- function(
  object, type = c("mean", "median"),
  level = c("population", "individual"), ...
) {
  check_fit(object)
  if (missing(type)) type <- "mean"
  if (missing(level)) level <- "population"
  oeli::input_check_response(
    checkmate::check_choice(type, choices = c("mean", "median")), "type"
  )
  oeli::input_check_response(
    checkmate::check_choice(
      level, choices = c("population", "individual")
    ),
    "level"
  )
  draws <- posterior::as_draws_matrix(object$draws)
  statistic <- if (identical(type, "mean")) mean else stats::median
  if (identical(level, "individual")) {
    variables <- individual_variables(object)
    if (!length(variables)) {
      cli::cli_abort(
        paste(
          "The fit contains no individual random-coefficient draws.",
          "Refit a mixed model with {.code save_individual_draws = TRUE}."
        ),
        call = NULL
      )
    }
    values <- apply(
      draws[, variables, drop = FALSE], 2L, statistic, na.rm = TRUE
    )
    random_names <- object$model$effects$effect_name[
      !is.na(object$model$effects$mixing)
    ]
    return(matrix(
      values,
      nrow = length(object$model$deciders),
      ncol = length(random_names),
      byrow = TRUE,
      dimnames = list(as.character(object$model$deciders), random_names)
    ))
  }
  draws <- draws[, posterior_variables(object), drop = FALSE]
  apply(draws, 2L, statistic, na.rm = TRUE)
}

#' Extract the posterior covariance matrix
#'
#' @description
#' Computes covariance across all retained draws of global model variables.
#'
#' @details
#' The returned matrix is the covariance of the posterior distribution, not
#' the sampling covariance of an estimator. It is reported through
#' [stats::vcov()] because it is the standard way to ask a fitted model for
#' the covariance of its parameters.
#'
#' @param object \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param ... Currently not used.
#'
#' @return A symmetric numeric matrix whose rows and columns are the global
#' posterior variables returned by `coef()`.
#'
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(choice ~ x + y | 0, chains = 1)
#' vcov(model)

vcov.RprobitB_fit <- function(object, ...) {
  check_fit(object)
  draws <- posterior::as_draws_matrix(object$draws)
  draws <- draws[, posterior_variables(object), drop = FALSE]
  stats::cov(draws, use = "pairwise.complete.obs")
}

#' Compute posterior credible intervals
#'
#' @description
#' Computes equal-tailed posterior intervals for selected model variables.
#'
#' @details
#' The returned intervals are credible intervals, not confidence intervals.
#' They are reported through [stats::confint()] because it is the standard
#' way to ask a fitted model for interval estimates.
#'
#' @param object \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param parm \[`character()` | `NULL`\]\cr
#' Variables to include. `NULL` includes every population-level posterior
#' variable that is not fixed by the normalization or the model structure.
#' Individual coefficients can be selected by their
#' `individual[effect,decider]` names.
#'
#' @param level \[`numeric(1)`\]\cr
#' Probability of the equal-tailed credible intervals.
#'
#' @param ... Currently not used.
#'
#' @return A numeric matrix with one row per selected variable and columns for
#' the lower and upper credible limits.
#'
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), chains = 1
#' )
#' confint(model)

confint.RprobitB_fit <- function(object, parm = NULL, level = 0.95, ...) {
  check_fit(object)
  available <- posterior_variables(object, individual = TRUE)
  if (is.null(parm)) parm <- posterior_variables(object)
  oeli::input_check_response(
    checkmate::check_character(parm, any.missing = FALSE, unique = TRUE),
    "parm"
  )
  oeli::input_check_response(
    checkmate::check_subset(parm, choices = available), "parm"
  )
  check_probability(level)
  alpha <- (1 - level) / 2
  draws <- posterior::as_draws_matrix(object$draws)[, parm, drop = FALSE]
  result <- t(apply(
    draws, 2L, stats::quantile,
    probs = c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE
  ))
  dimnames(result) <- list(
    parm, paste0(format(100 * c(alpha, 1 - alpha)), "%")
  )
  result
}

#' Convert a fitted model to posterior draws
#'
#' @description
#' Returns the canonical retained posterior draws.
#'
#' @param x \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param ... Currently not used.
#'
#' @return A `draws_array` with dimensions iteration, chain, and variable.
#'
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), chains = 1
#' )
#' posterior::as_draws(model)

as_draws.RprobitB_fit <- function(x, ...) {
  check_fit(x, "x")
  x$draws
}

#' Plot posterior draws
#'
#' @description
#' Creates a standard posterior diagnostic or uncertainty plot with
#' [**bayesplot**](https://mc-stan.org/bayesplot/).
#'
#' @param x \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param y \[`NULL`\]\cr
#' Currently not used.
#'
#' @param type \[`character(1)`\]\cr
#' The plot to create:
#'
#' - `"trace"` draws the sampled values of each chain over the iterations.
#' - `"rank"` compares the chains through the ranks of their draws.
#' - `"acf"` draws the autocorrelation within each chain.
#' - `"density"` overlays the marginal posterior density of each chain.
#' - `"interval"` draws posterior point estimates with credible intervals.
#' - `"pairs"` draws bivariate scatter plots of the variables.
#'
#' @param variables \[`character()` | `NULL`\]\cr
#' Posterior variables to include. `NULL` includes all varying model
#' parameters and excludes individual coefficients and latent allocations.
#'
#' @param ... Further arguments passed to the selected
#' [**bayesplot**](https://mc-stan.org/bayesplot/) function.
#'
#' @return A `ggplot` object or, for a pairs plot, a `bayesplot_grid` object.
#'
#' @export
#' @keywords hplot
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x + z | 0, dgp_parameters = list(beta = c(x = 1, z = -0.5)),
#'   chains = 2
#' )
#'
#' ### convergence and mixing of the chains
#' plot(model, type = "trace")
#' plot(model, type = "rank")
#' plot(model, type = "acf")
#'
#' ### marginal and joint posterior distributions
#' plot(model, type = "density")
#' plot(model, type = "interval")
#' plot(model, type = "pairs")

plot.RprobitB_fit <- function(
  x, y = NULL,
  type = c("trace", "rank", "acf", "density", "interval", "pairs"),
  variables = NULL, ...
) {
  check_fit(x, "x")
  oeli::input_check_response(checkmate::check_null(y), "y")
  if (missing(type)) type <- "trace"
  oeli::input_check_response(
    checkmate::check_choice(
      type,
      choices = c("trace", "rank", "acf", "density", "interval", "pairs")
    ),
    "type"
  )
  available <- dimnames(x$draws)$variable
  if (is.null(variables)) {
    variables <- posterior_variables(x)
    spread <- apply(x$draws[, , variables, drop = FALSE], 3L, stats::sd)
    variables <- variables[!is.na(spread) & spread > sqrt(.Machine$double.eps)]
    if (!length(variables)) {
      cli::cli_abort(
        "The fit contains no varying posterior parameters.",
        call = NULL
      )
    }
  } else {
    oeli::input_check_response(
      checkmate::check_character(
        variables,
        any.missing = FALSE, unique = TRUE
      ),
      "variables"
    )
    oeli::input_check_response(
      checkmate::check_subset(variables, choices = available), "variables"
    )
  }
  draws <- as.array(x$draws[, , variables, drop = FALSE])
  if (identical(type, "pairs") && length(variables) < 2L) {
    oeli::input_check_response(
      "Must contain at least two variables for a pairs plot.", "variables"
    )
  }
  dots <- list(...)
  switch(type,
    trace = bayesplot::mcmc_trace(draws, ...),
    rank = bayesplot::mcmc_rank_overlay(draws, ...),
    acf = if ("lags" %in% names(dots)) {
      bayesplot::mcmc_acf(draws, ...)
    } else {
      bayesplot::mcmc_acf(
        draws,
        lags = min(20L, max(0L, dim(draws)[1L] - 2L)),
        ...
      )
    },
    density = bayesplot::mcmc_dens_overlay(draws, ...),
    interval = bayesplot::mcmc_intervals(draws, ...),
    pairs = bayesplot::mcmc_pairs(draws, ...)
  )
}
