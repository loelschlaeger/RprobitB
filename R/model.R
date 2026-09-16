#' Print a fitted choice model
#'
#' @description
#' Prints the model formula, data size, and retained posterior sample size.
#'
#' @param x \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param ... Currently not used.
#'
#' @return `x`, invisibly.
#'
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x, dgp_parameters = list(beta = c(x = 1, ASC_B = -0.5)),
#'   chains = 1
#' )
#' print(model)

print.RprobitB_fit <- function(x, ...) {
  check_fit(x, "x")
  identifiers <- choicedata::extract_choice_identifiers(x$data)
  cat("Bayesian probit choice model\n")
  cat("Formula:", deparse1(x$model$formula), "\n")
  cat(
    "Data:", length(unique(identifiers[[x$model$data_roles$column_decider]])),
    "deciders,", nrow(identifiers), "choice occasions\n"
  )
  cat(
    "Samples:", x$sampler$retained_per_chain, "retained per chain,",
    x$sampler$chains, if (x$sampler$chains == 1L) "chain\n" else "chains\n"
  )
  invisible(x)
}

#' Extract the fitted formula
#'
#' @description
#' Returns the normalized three-part model formula stored in a fitted model.
#'
#' @param x \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param ... Currently not used.
#'
#' @return A `formula` object.
#'
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x, dgp_parameters = list(beta = c(x = 1, ASC_B = -0.5)),
#'   chains = 1
#' )
#' formula(model)

formula.RprobitB_fit <- function(x, ...) {
  check_fit(x, "x")
  x$model$formula
}

#' Extract the fitted data
#'
#' @description
#' Returns the choice data stored in a fitted model as a `data.frame`.
#'
#' @param formula \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param ... Currently not used.
#'
#' @return A `data.frame` containing the fitted choice data.
#'
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x, dgp_parameters = list(beta = c(x = 1, ASC_B = -0.5)),
#'   chains = 1
#' )
#' head(model.frame(model))

model.frame.RprobitB_fit <- function(formula, ...) {
  check_fit(formula, "formula")
  as.data.frame(formula$data)
}

#' Count independent likelihood units
#'
#' @description
#' Counts the observed choice occasions if the model has neither random
#' effects nor latent classes, because the likelihood then factorizes over
#' the occasions. Otherwise the occasions of a decider are dependent through
#' the random coefficients or the class membership, and the deciders with at
#' least one observed response are counted.
#'
#' @param object \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param ... Currently not used.
#'
#' @return An `integer(1)` count.
#'
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x, dgp_parameters = list(beta = c(x = 1, ASC_B = -0.5)),
#'   chains = 1
#' )
#' nobs(model)

nobs.RprobitB_fit <- function(object, ...) {
  check_fit(object)
  observed <- !is.na(object$model$responses)
  heterogeneous <- any(!is.na(object$model$effects$mixing)) ||
    length(object$model$latent_class_effects) > 0L
  if (is.null(object$model$data_roles$column_occasion) || !heterogeneous) {
    return(sum(observed))
  }
  identifiers <- choicedata::extract_choice_identifiers(object$data)
  decider <- object$model$data_roles$column_decider
  length(unique(identifiers[[decider]][observed]))
}

#' Update and refit a choice model
#'
#' @description
#' Refits a choice model with a modified specification.
#'
#' @details
#' Arguments that are not specified are taken from `object`.
#'
#' The model formula is updated part by part, so `. ~ . + income` extends the
#' covariates that are constant across alternatives and leaves the other two
#' formula parts unchanged.
#'
#' The choice data of `object` are reused, also if they were simulated, which
#' makes the updated model comparable to `object`. Supply `data` to fit the
#' updated model to other choice data.
#'
#' @param object \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param formula. \[`formula`\]\cr
#' Changes to the model formula, see the details.
#'
#' @param ...
#' Arguments of [fit()] that replace the ones of `object`.
#'
#' @param evaluate \[`logical(1)`\]\cr
#' Refit the model? If `FALSE`, the updated call is returned, where `data`
#' stands for the choice data of `object`.
#'
#' @return
#' An object of class `RprobitB_fit`, or the updated `call` if `evaluate` is
#' `FALSE`.
#'
#' @export
#' @keywords models
#'
#' @examples
#' ### simulate choice data and fit a model with two covariates
#' set.seed(1)
#' model <- fit(
#'   choice ~ x + y | 0, dgp_parameters = list(beta = c(x = 1, y = -0.5)),
#'   chains = 1
#' )
#' summary(model)
#'
#' ### drop `y` from the formula, the other formula parts stay as they are
#' model_2 <- update(model, . ~ . - y)
#' summary(model_2)
#'
#' ### let the coefficient of `x` vary across deciders instead
#' model_3 <- update(model, random_effects = "x")
#' summary(model_3)

update.RprobitB_fit <- function(object, formula., ..., evaluate = TRUE) {

  # input checks
  check_fit(object)
  oeli::input_check_response(checkmate::check_flag(evaluate), "evaluate")
  model_call <- object$call
  extras <- as.list(match.call(expand.dots = FALSE)[["..."]])
  model <- object$model
  sampler <- object$sampler
  roles <- model$data_roles

  # the specification of `object` replaces the arguments of its call, where
  # every formula part is updated on its own
  model_call$formula <- if (missing(formula.)) {
    model$formula
  } else {
    oeli::input_check_response(checkmate::check_formula(formula.), "formula.")
    stats::formula(
      stats::update(Formula::as.Formula(model$formula), formula.)
    )
  }
  covariates <- c(all.vars(model_call$formula[[3L]]), "ASC")
  random <- model$random_effects
  model_call$random_effects <- random[names(random) %in% covariates]
  latent <- model$latent_class_effects
  suffix <- regexpr(roles$delimiter, latent, fixed = TRUE)
  latent_covariates <- substr(latent, 1L, ifelse(suffix > 0L, suffix - 1L, 1e6))
  model_call$latent_class_effects <- latent[
    latent %in% covariates | latent_covariates %in% covariates
  ]
  model_call$choice_type <- model$choice_type
  model_call$alternatives <- as.character(model$alternatives)
  model_call$base <- attr(model$alternatives, "base")
  scale <- model$normalization$scale
  model_call$scale <- if (is.na(scale$name)) {
    NULL
  } else {
    stats::setNames(scale$value, scale$name)
  }
  model_call$classes <- model$latent_classes$initial
  model_call$class_update <- model$latent_classes$update
  changing_classes <- model$latent_classes$update %in%
    c("dirichlet_process", "weight_based")
  if (changing_classes) model_call$max_classes <- model$latent_classes$maximum
  model_call$weight_based_control <- model$latent_classes$control
  model_call$iterations <- sampler$iterations
  if (!is.null(model_call$warmup)) model_call$warmup <- sampler$warmup
  model_call$thin <- sampler$thin
  model_call$chains <- sampler$chains
  model_call$save_individual_draws <- sampler$save_individual_draws

  # the choice data of `object` replace the data and the simulation arguments
  model_call$format <- roles$format
  model_call$column_decider <- roles$column_decider
  model_call$column_occasion <- roles$column_occasion
  model_call$column_alternative <- roles$column_alternative
  model_call$delimiter <- roles$delimiter
  model_call[c(
    "n_deciders", "n_occasions", "n_alternatives", "covariates",
    "dgp_parameters"
  )] <- NULL
  supplied_data <- "data" %in% names(extras)
  if (!supplied_data) model_call$data <- as.name("data")

  # the arguments in `...` replace the ones of `object`
  replaced <- !is.na(match(names(extras), names(model_call)))
  for (name in names(extras)[replaced]) model_call[[name]] <- extras[[name]]
  if (any(!replaced)) {
    model_call <- as.call(c(as.list(model_call), extras[!replaced]))
  }
  if (!evaluate) {
    return(model_call)
  }
  refit <- new.env(parent = parent.frame())
  if (!supplied_data) refit$data <- object$data
  refitted <- eval(model_call, refit)

  # the refitted model keeps the true parameters of simulated data if only
  # the normalization, the prior, or the sampler settings changed
  harmless <- c(
    "scale", "prior", "iterations", "warmup", "thin", "chains",
    "save_individual_draws", "progress"
  )
  keep_simulation <- !supplied_data && missing(formula.) &&
    !is.null(object$simulation) && all(names(extras) %in% harmless)
  if (keep_simulation) {
    refitted$simulation <- object$simulation
    refitted$simulation$dgp <- dgp_values(
      object$simulation$dgp_parameters, refitted$model$effects,
      refitted$model$normalization, refitted$model$latent_classes$update
    )
  }
  refitted
}
