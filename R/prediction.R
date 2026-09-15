#' Predict choices
#'
#' @description
#' Computes occasion-level choice probabilities over the retained posterior
#' draws and predicts the alternative with the largest mean probability.
#'
#' @details
#' Conditional prediction is based on the individual-level parameters (Train,
#' 2009, Chapters 11 and 12): the posterior distribution of a decider's
#' coefficients given their observed choices, which the Gibbs sampler
#' provides as draws when the model is fitted with
#' `save_individual_draws = TRUE`.
#'
#' @param object \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param newdata \[`data.frame` | `NULL`\]\cr
#' Data for prediction. `NULL` uses the fitted data. A response column is
#' optional. In wide format, missing decider and occasion identifiers make
#' every row a choice occasion of its own decider.
#'
#' @param type \[`character(1)`\]\cr
#' Which coefficients to predict with:
#'
#' - `"population"` integrates over the estimated population distribution
#'   of the random coefficients and applies to any decider.
#' - `"conditional"` uses the posterior random coefficients and class
#'   allocations of the deciders that were observed when fitting the model,
#'   which `newdata` must then name.
#'
#' @param uncertainty \[`logical(1)`\]\cr
#' Add posterior standard deviations and credible intervals?
#'
#' @param level \[`numeric(1)`\]\cr
#' Probability of the credible intervals.
#'
#' @inheritParams WAIC
#'
#' @param ... Currently not used.
#'
#' @return A `data.frame` with one row per choice occasion, identifier columns,
#' `.prediction`, and one `probability_*` column per alternative. If
#' `uncertainty = TRUE`, `sd_*`, `lower_*`, and `upper_*` columns are added.
#'
#' @references
#' \insertRef{Train2009}{RprobitB}
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x | 0,
#'   random_effects = "x",
#'   dgp_parameters = list(beta = c(x = 1), Omega = matrix(0.5)),
#'   n_deciders = 20,
#'   iterations = 300,
#'   warmup = 150,
#'   chains = 1,
#'   save_individual_draws = TRUE
#' )
#' head(predict(model))
#' head(predict(model, type = "conditional"))
#' head(predict(model, uncertainty = TRUE))
#'
#' ### new choice occasions
#' new_data <- data.frame(deciderID = 21:22, x_A = c(1, -1), x_B = c(0, 0))
#' predict(model, newdata = new_data)

predict.RprobitB_fit <- function(
  object, newdata = NULL, type = c("population", "conditional"),
  uncertainty = FALSE, level = 0.95, ghk_draws = 500L,
  progress = interactive(), ...
) {
  check_fit(object)
  if (missing(type)) type <- "population"
  oeli::input_check_response(
    checkmate::check_choice(
      type, choices = c("population", "conditional")
    ),
    "type"
  )
  oeli::input_check_response(checkmate::check_flag(uncertainty), "uncertainty")
  check_probability(level)
  oeli::input_check_response(
    checkmate::check_data_frame(newdata, null.ok = TRUE), "newdata"
  )
  decider <- object$model$data_roles$column_decider
  if (identical(type, "conditional") && !is.null(newdata) &&
      !decider %in% names(newdata)) {
    oeli::input_check_response(
      paste0(
        "Must name the deciders in column `", decider,
        "` if `type = \"conditional\"`."
      ),
      "newdata"
    )
  }
  check_ghk_draws(ghk_draws)
  oeli::input_check_response(checkmate::check_flag(progress), "progress")
  prediction_data <- as_prediction_data(object, newdata)
  alternatives <- as.character(object$model$alternatives)
  probabilities <- probability_draws(
    object, prediction_data, type = type, ghk_draws = ghk_draws,
    progress = progress
  )
  dimensions <- dim(probabilities[[1L]])
  if (isTRUE(uncertainty)) {
    probability_array <- array(
      unlist(probabilities, use.names = FALSE),
      dim = c(dimensions, length(probabilities))
    )
    probability_mean <- apply(probability_array, 1:2, mean)
  } else {
    probability_mean <- Reduce(`+`, probabilities) / length(probabilities)
  }
  if (is.null(dim(probability_mean))) {
    probability_mean <- matrix(
      probability_mean,
      nrow = dimensions[1L], ncol = dimensions[2L]
    )
  }
  identifiers <- as.data.frame(
    choicedata::extract_choice_identifiers(prediction_data)
  )
  prediction <- alternatives[max.col(probability_mean, ties.method = "first")]
  result <- identifiers
  result$.prediction <- prediction
  result[paste0("probability_", alternatives)] <- probability_mean
  if (isTRUE(uncertainty)) {
    alpha <- (1 - level) / 2
    probability_sd <- apply(probability_array, 1:2, stats::sd)
    probability_lower <- apply(
      probability_array, 1:2, stats::quantile,
      probs = alpha, names = FALSE
    )
    probability_upper <- apply(
      probability_array, 1:2, stats::quantile,
      probs = 1 - alpha, names = FALSE
    )
    result[paste0("sd_", alternatives)] <- probability_sd
    result[paste0("lower_", alternatives)] <- probability_lower
    result[paste0("upper_", alternatives)] <- probability_upper
  }
  result
}

#' Extract choice residuals
#'
#' @description
#' Computes observed choice indicators minus posterior mean occasion-level
#' choice probabilities.
#'
#' @param object \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param ... Further arguments passed to `predict()`.
#'
#' @return A numeric matrix with one row per choice occasion and one column per
#' alternative. Rows with a missing response contain `NA`. For ranked data, the
#' indicator represents the first-ranked alternative.
#'
#' @export
#' @keywords models
#'
#' @examples
#' set.seed(1)
#' model <- fit(
#'   choice ~ x | 0, dgp_parameters = list(beta = c(x = 1)), chains = 1
#' )
#' head(residuals(model))

residuals.RprobitB_fit <- function(object, ...) {
  check_fit(object)
  prediction <- predict(object, ...)
  alternatives <- as.character(object$model$alternatives)
  probabilities <- as.matrix(
    prediction[, paste0("probability_", alternatives), drop = FALSE]
  )
  observed <- object$model$responses
  if (identical(object$model$choice_type, "ranked")) {
    observed <- sub(",.*$", "", observed)
  }
  indicator <- matrix(
    0,
    nrow = length(observed),
    ncol = length(alternatives),
    dimnames = list(NULL, alternatives)
  )
  missing <- is.na(observed)
  positions <- cbind(
    which(!missing), match(observed[!missing], alternatives)
  )
  indicator[positions] <- 1
  indicator[missing, ] <- NA_real_
  identifiers <- prediction[, names(
    as.data.frame(choicedata::extract_choice_identifiers(object$data))
  ), drop = FALSE]
  rownames(indicator) <- do.call(paste, c(identifiers, sep = ":"))
  indicator - probabilities
}
