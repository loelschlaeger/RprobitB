#' Interpret the estimates of a fitted choice model
#'
#' @description
#' This function translates the posterior draws to scales that are easier to
#' interpret:
#'
#' - A compensation says how many units of a reference covariate,
#'   for example the price, are worth one unit of another covariate, leaving
#'   the utility and hence the choice probabilities unchanged.
#' - A marginal effect says by how much the choice probability of an alternative
#'   changes per unit of a covariate, computed by finite differences of the
#'   predicted probabilities.
#'
#' Both are reported with their posterior uncertainty.
#'
#' @param object \[`RprobitB_fit`\]\cr
#' Fitted choice model.
#'
#' @param type \[`character(1)`\]\cr
#' The quantity to compute:
#'
#' - `"compensation"`: how many units of `reference` compensate one
#'   additional unit of every other effect, so that the utility stays the
#'   same. With the price as `reference`, this is the willingness to pay.
#' - `"ame"`: the average marginal effect, that is, the derivative of the
#'   choice probability of an alternative with respect to a covariate,
#'   computed for every observed choice occasion and averaged.
#' - `"mea"`: the marginal effect at the average, that is, the same
#'   derivative for a single occasion whose covariates equal the averages of
#'   the observed ones.
#'
#' @param reference \[`character(1)` | `NULL`\]\cr
#' The effect in whose units compensations are measured. `NULL` uses the
#' coefficient that `scale` fixed when fitting, and requires a choice
#' otherwise. Only used if `type = "compensation"`.
#'
#' @param effects \[`character()` | `NULL`\]\cr
#' The effects to express in units of `reference`. `NULL` uses every effect
#' other than `reference`. Only used if `type = "compensation"`.
#'
#' @param at \[`named numeric()` | `NULL`\]\cr
#' Covariate values at which the marginal effects of `type = "mea"` are
#' evaluated, see the details. `NULL` uses the average of every covariate.
#'
#' @param progress \[`logical(1)`\]\cr
#' Show progress?
#'
#' @param level \[`numeric(1)`\]\cr
#' Probability of the equal-tailed posterior credible interval.
#'
#' @return A `data.frame` of class `RprobitB_interpretation` with one row per
#' quantity and the columns `mean`, `sd`, `lower`, and `upper` of its
#' posterior distribution. Compensations have a column `effect` and, for a
#' mixture model, a column `class`; marginal effects have the columns
#' `covariate` and `alternative`.
#'
#' @section Compensations:
#' The utility of an alternative is linear in the covariates, so an increase
#' of one unit in an effect with coefficient \eqn{\beta_j} is compensated by
#' a change of \eqn{-\beta_j / \beta_k} units in the reference effect with
#' coefficient \eqn{\beta_k}. For a random effect, the coefficient of the median
#' decider enters the ratio. Mixture models report one compensation per class.
#' The ratio is computed for every posterior draw, so the reported uncertainty
#' is the posterior uncertainty of the ratio. Compensations are free of the
#' utility scale normalization, which makes them comparable across models.
#'
#' @section Marginal effects:
#' Marginal effects are derivatives of choice probabilities and are computed
#' by finite differences of the predicted probabilities. Marginal
#' effects of a mixed model refer to the population distribution of the
#' random coefficients.
#'
#' - `type = "ame"` averages the marginal effects over all observed choices.
#' - `type = "mea"` builds one artificial choice occasion whose covariates are
#' the averages of the observed ones: the mean for numeric covariates and the
#' most frequent value for the others. The argument `at` can be used to replace
#' the average of the covariates.
#'
#' @export
#' @keywords models
#'
#' @examplesIf requireNamespace("AER", quietly = TRUE)
#' ### travel mode choice where travel time has an alternative-specific effect
#' data("TravelMode", package = "AER")
#' TravelMode$choice <- TravelMode$choice == "yes"
#' TravelMode$vcost <- TravelMode$vcost / 1.6196 # cost in Euro
#' set.seed(1)
#' model <- fit(
#'   choice ~ vcost | 1 | travel,
#'   data = TravelMode,
#'   format = "long",
#'   column_decider = "individual",
#'   column_alternative = "mode",
#'   scale = c(vcost = -1),
#'   iterations = 100,
#'   chains = 1
#' )
#'
#' ### travel time must be compensated far more in the plane than in the bus
#' interpret(
#'   model, type = "compensation", effects = c("travel_bus", "travel_air")
#' )
#'
#' ### the marginal effects at the average covariates, and for a short flight
#' interpret(model, type = "mea", at = c(travel_air = 40))

interpret <- function(
  object, type = c("compensation", "ame", "mea"), reference = NULL,
  effects = NULL, at = NULL, level = 0.95, progress = interactive()
) {

  # input checks
  check_fit(object)
  if (missing(type)) type <- "compensation"
  oeli::input_check_response(
    checkmate::check_choice(type, choices = c("compensation", "ame", "mea")),
    "type"
  )
  check_probability(level)
  oeli::input_check_response(checkmate::check_flag(progress), "progress")
  overview <- object$model$effects
  names <- overview$effect_name

  # calculate compensation
  if (identical(type, "compensation")) {
    if (is.null(reference)) {
      scale <- object$model$normalization$scale
      if (!identical(scale$parameter, "alpha")) {
        oeli::input_check_response(
          paste(
            "Must name the effect in whose units to measure, for example the",
            "price, unless `scale` fixed a coefficient when fitting."
          ),
          "reference"
        )
      }
      reference <- scale$name
    }
    oeli::input_check_response(
      checkmate::check_choice(reference, choices = names), "reference"
    )
    if (is.null(effects)) effects <- setdiff(names, reference)
    oeli::input_check_response(
      checkmate::check_character(
        effects, min.len = 1L, any.missing = FALSE, unique = TRUE
      ),
      "effects"
    )
    oeli::input_check_response(
      checkmate::check_subset(effects, setdiff(names, reference)), "effects"
    )

    # utility coefficients per draw and class, random effects at the median
    samples <- posterior::as_draws_matrix(object$draws)
    classes <- object$model$latent_classes$maximum
    lc <- names %in% object$model$latent_class_effects
    coefficients <- lapply(seq_len(classes), function(class) {
      values <- lapply(seq_along(names), function(i) {
        mixing <- as.character(overview$mixing[i])
        if (is.na(mixing)) {
          variable <- if (lc[i]) {
            sprintf("beta[%s,%s]", names[i], class)
          } else {
            sprintf("beta[%s]", names[i])
          }
          return(as.numeric(samples[, variable]))
        }
        variable <- if (lc[i]) {
          sprintf("mu[%s,%s]", names[i], class)
        } else {
          sprintf("mu[%s]", names[i])
        }
        latent <- as.numeric(samples[, variable])
        switch(sub("^c", "", mixing),
          n = latent,
          ln = exp(latent),
          "ln-" = -exp(latent)
        )
      })
      values <- do.call(cbind, values)
      colnames(values) <- names
      values
    })

    # the compensations with their posterior summaries
    result <- do.call(rbind, lapply(seq_len(classes), function(class) {
      values <- coefficients[[class]]
      do.call(rbind, lapply(effects, function(effect) {
        ratio <- -values[, effect] / values[, reference]
        cbind(effect = effect, class = class, summarize_draws(ratio, level))
      }))
    }))
    if (classes == 1L) result$class <- NULL
    rownames(result) <- NULL
    constants <- is.na(overview$covariate)
    return(structure(
      result,
      type = "compensation",
      reference = reference,
      constants = stats::setNames(
        as.character(overview$alternative[constants]), names[constants]
      ),
      level = level,
      class = c("RprobitB_interpretation", "data.frame")
    ))
  }

  # marginal effects differentiate the predicted probabilities numerically
  frame <- model.frame(object)
  roles <- object$model$data_roles
  alternatives <- as.character(object$model$alternatives)
  long <- identical(roles$format, "long")
  ordered <- identical(object$model$choice_type, "ordered")
  ranked <- identical(object$model$choice_type, "ranked")
  covariates <- unique(overview$covariate[!is.na(overview$covariate)])
  columns_of <- lapply(covariates, function(covariate) {
    varying <- !ordered && isTRUE(
      any(overview$as_covariate[overview$covariate %in% covariate])
    )
    first <- paste(covariate, alternatives[1L], sep = roles$delimiter)
    columns <- if (varying && !long) {
      paste(covariate, alternatives, sep = roles$delimiter)
    } else if (ordered && first %in% names(frame)) {
      rep(first, length(alternatives))
    } else {
      rep(covariate, length(alternatives))
    }
    stats::setNames(list(varying, columns), c("varying", "columns"))
  })
  names(columns_of) <- covariates
  oeli::input_check_response(
    oeli::check_numeric_vector(
      at, any.missing = FALSE, finite = TRUE, names = "unique",
      min.len = 1L, null.ok = TRUE
    ),
    "at"
  )
  targets <- lapply(names(at), function(name) {
    if (name %in% covariates) {
      return(list(columns = unique(columns_of[[name]]$columns)))
    }
    parts <- strsplit(name, roles$delimiter, fixed = TRUE)[[1L]]
    covariate <- paste(
      utils::head(parts, -1L), collapse = roles$delimiter
    )
    alternative <- utils::tail(parts, 1L)
    known <- covariate %in% covariates && alternative %in% alternatives &&
      columns_of[[covariate]]$varying
    if (!known) {
      oeli::input_check_response(
        paste0(
          "Must name a covariate, or a covariate and an alternative that ",
          "varies across alternatives. Unknown: ", name, "."
        ),
        "at"
      )
    }
    columns <- columns_of[[covariate]]$columns
    names(columns) <- alternatives
    list(columns = columns[[alternative]], alternative = alternative)
  })
  names(targets) <- names(at)
  if (identical(type, "mea")) {
    identifiers <- c(roles$column_decider, roles$column_occasion)
    response <- all.vars(object$model$formula)[1L]
    if (ranked && !long) {
      response <- paste(response, alternatives, sep = roles$delimiter)
    }
    groups <- if (long) {
      lapply(alternatives, function(alternative) {
        which(frame[[roles$column_alternative]] == alternative)
      })
    } else {
      list(seq_len(nrow(frame)))
    }
    xlevels <- attr(object$model$effects, "choice_formula")$xlevels
    categorical <- as.character(unlist(lapply(
      names(unlist(xlevels, recursive = FALSE)),
      function(term) all.vars(str2lang(term))
    )))
    categorical <- c(
      categorical,
      outer(categorical, alternatives, paste, sep = roles$delimiter)
    )
    occasion <- do.call(rbind, lapply(groups, function(rows) {
      values <- lapply(names(frame), function(name) {
        column <- frame[[name]][rows]
        if (name %in% c(identifiers, response)) {
          column[1L]
        } else if (is.numeric(column) && !name %in% categorical) {
          mean(column, na.rm = TRUE)
        } else {
          column[match(names(which.max(table(column))), as.character(column))]
        }
      })
      names(values) <- names(frame)
      as.data.frame(values, stringsAsFactors = FALSE)
    }))
    if (long) occasion[[roles$column_alternative]] <- alternatives
    occasion[[roles$column_decider]] <- frame[[roles$column_decider]][1L]
    if (!is.null(roles$column_occasion)) occasion[[roles$column_occasion]] <- 1L
    occasion[response] <- NA
    for (name in names(at)) {
      target <- targets[[name]]
      rows <- if (long && !is.null(target$alternative)) {
        match(target$alternative, alternatives)
      } else {
        seq_len(nrow(occasion))
      }
      occasion[rows, target$columns] <- at[[name]]
    }
    frame <- occasion
  }
  marginals <- list()
  for (covariate in covariates) {
    columns <- columns_of[[covariate]]$columns
    names(columns) <- alternatives
    numeric <- all(columns %in% names(frame)) &&
      all(vapply(frame[unique(columns)], is.numeric, logical(1)))
    if (!numeric) next
    for (alternative in alternatives) {
      marginals[[length(marginals) + 1L]] <- list(
        covariate = covariate, alternative = alternative,
        column = columns[[alternative]],
        varying = columns_of[[covariate]]$varying
      )
    }
  }
  if (!length(marginals)) {
    oeli::input_check_response(
      "Contains no numeric covariate to differentiate.", "object"
    )
  }

  # every covariate and alternative is differentiated independently
  rows <- progressr::with_progress(
    {
      progressor <- progressr::progressor(steps = length(marginals))
      future.apply::future_lapply(
        marginals,
        function(marginal) {
          selected <- if (marginal$varying && long) {
            which(frame[[roles$column_alternative]] == marginal$alternative)
          } else {
            seq_len(nrow(frame))
          }
          values <- frame[[marginal$column]][selected]
          step <- 1e-3 * stats::sd(frame[[marginal$column]])
          if (!is.finite(step) || step == 0) step <- 1e-3
          shifted <- lapply(c(1, -1), function(sign) {
            data <- frame
            data[[marginal$column]][selected] <- values + sign * step
            probability_draws(object, as_prediction_data(object, data))
          })
          derivative <- vapply(seq_along(shifted[[1L]]), function(draw) {
            difference <- shifted[[1L]][[draw]][, marginal$alternative] -
              shifted[[2L]][[draw]][, marginal$alternative]
            mean(difference / (2 * step))
          }, numeric(1))
          progressor(
            message = paste0(
              "Marginal effect of ", marginal$covariate, " on ",
              marginal$alternative
            )
          )
          summary <- summarize_draws(derivative, level)
          if (identical(type, "mea")) {
            summary <- cbind(at = mean(values), summary)
          }
          cbind(
            covariate = marginal$covariate, alternative = marginal$alternative,
            summary
          )
        },
        future.seed = TRUE
      )
    },
    enable = progress
  )

  # the marginal effects with their posterior summaries
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  structure(
    result,
    type = type,
    at = at,
    level = level,
    class = c("RprobitB_interpretation", "data.frame")
  )
}

#' @rdname interpret
#' @param x \[`RprobitB_interpretation`\]\cr
#' Output of `interpret()`.
#' @param digits \[`integer(1)`\]\cr
#' Number of significant digits to print.
#' @param ... Currently not used.
#' @export

print.RprobitB_interpretation <- function(x, digits = 3L, ...) {
  oeli::input_check_response(checkmate::check_int(digits, lower = 1L), "digits")
  type <- attr(x, "type")
  level <- attr(x, "level")
  interval <- paste0(format(100 * level), "% interval")
  if (identical(type, "compensation")) {
    reference <- attr(x, "reference")
    constants <- attr(x, "constants")
    for (i in seq_len(nrow(x))) {
      class <- if ("class" %in% names(x)) {
        paste0("Class ", x$class[i], ": ")
      } else {
        ""
      }
      subject <- if (x$effect[i] %in% names(constants)) {
        paste0("the constant of `", constants[[x$effect[i]]], "` is worth ")
      } else {
        paste0("one unit of `", x$effect[i], "` compensates ")
      }
      sentence <- paste0(
        class, subject, format(x$mean[i], digits = digits), " units of `",
        reference, "` (", interval, " ",
        format(x$lower[i], digits = digits), " to ",
        format(x$upper[i], digits = digits), ")"
      )
      cat(strwrap(sentence, exdent = 2L), sep = "\n")
    }
  } else {
    heading <- if (identical(type, "ame")) {
      "Average marginal effects on the choice probabilities"
    } else if (is.null(attr(x, "at"))) {
      "Marginal effects at the average covariate values"
    } else {
      "Marginal effects at the given covariate values"
    }
    cat(heading, "\n", sep = "")
    cat(strwrap(paste(
      "Change in the probability of the alternative per unit of the",
      "covariate, with", interval
    )), sep = "\n")
    table <- as.data.frame(x)
    numeric <- vapply(table, is.numeric, logical(1))
    table[numeric] <- lapply(table[numeric], format, digits = digits)
    print(table, row.names = FALSE)
  }
  invisible(x)
}

