#' Overview model effects
#'
#' @description
#' This function provides an overview of the model effects.
#'
#' @param RprobitB_formula
#' An \code{\link{RprobitB_formula}} object.
#' @param RprobitB_alternatives
#' An \code{\link{RprobitB_alternatives}} object.
#' @param delimiter
#' A \code{character}, the delimiter between covariate and alternative name
#' to build the effect name.
#' By default, \code{delimiter = "_"}.
#'
#' @inheritSection RprobitB_formula Model formula
#' @inheritSection RprobitB_formula Random effects
#' @inheritSection RprobitB_alternatives Base alternative
#'
#' @return
#' A \code{data.frame}, each row is an effect, columns are
#' 1. \code{"name"}, the effect name (composed of covariate and alternative
#'    name),
#' 2. \code{"covariate"}, the covariate name,
#' 3. \code{"alternative"}, the alternative name (only if the effect is
#'    alternative-specific, i.e., varies across alternatives),
#' 4. \code{"as_covariate"}, indicator whether the covariate is
#'    alternative-specific,
#' 5. \code{"as_effect"}, indicator whether the effect is alternative-specific,
#' 6. \code{"random"}, indicator whether the effect is a random effect,
#' 7. and \code{"log_normal"}, indicator whether the random effect is
#'    log-normal.
#'
#' The effects are ordered as follows: Fixed effects come before random effects,
#' and log-normal random effects are last random effects. Otherwise, the order
#' is determined by occurrence in \code{formula}.
#'
#' @examples
#' overview_effects(
#'   RprobitB_formula = RprobitB_formula(
#'     formula = choice ~ price | income | comfort,
#'     re = c("price+", "income")
#'   ),
#'   RprobitB_alternatives = RprobitB_alternatives(J = 3)
#' )
#'
#' @export

overview_effects <- function(
    RprobitB_formula, RprobitB_alternatives, delimiter = "_") {
  ### input checks
  if (missing(RprobitB_formula)) {
    RprobitB_stop(
      "Please specify the input 'RprobitB_formula'.",
      "It should be an `RprobitB_formula` object.",
      "See the function documentation for details."
    )
  }
  if (!is.RprobitB_formula(RprobitB_formula)) {
    RprobitB_stop(
      "Input 'RprobitB_formula' is misspecified.",
      "It should be an `RprobitB_formula` object.",
      "See the function documentation for details."
    )
  }
  if (missing(RprobitB_alternatives)) {
    RprobitB_stop(
      "Please specify the input 'RprobitB_alternatives'.",
      "See the function documentation for details."
    )
  }
  if (!is.RprobitB_alternatives(RprobitB_alternatives)) {
    RprobitB_stop(
      "Input 'RprobitB_alternatives' is misspecified.",
      "It should be an `RprobitB_alternatives` object.",
      "See the function documentation for details."
    )
  }
  if (!(is.character(delimiter) && length(delimiter) == 1)) {
    RprobitB_stop(
      "Input 'delimiter' is misspecified.",
      "It should be a single `character`."
    )
  }

  ### build effect overview
  J <- RprobitB_alternatives$J
  alternatives <- RprobitB_alternatives$alternatives
  base <- RprobitB_alternatives$base
  ordered <- RprobitB_alternatives$ordered
  vars <- RprobitB_formula$vars
  md_n <- RprobitB_formula$md_n
  md_ln <- RprobitB_formula$md_ln
  re <- c(md_n, md_ln)
  overview <- data.frame(matrix(ncol = 5, nrow = 0))
  if (ordered) {
    for (var in vars[[2]]) {
      overview <- rbind(
        overview,
        c(var, var, NA_character_, FALSE, FALSE, var %in% re, var %in% md_ln)
      )
    }
  } else {
    for (var in vars[[1]]) {
      overview <- rbind(
        overview,
        c(var, var, NA_character_, TRUE, FALSE, var %in% re, var %in% md_ln)
      )
    }
    for (var in c(vars[[2]], if (RprobitB_formula$ASC) "ASC")) {
      for (j in (1:J)[-which(alternatives == base)]) {
        overview <- rbind(
          overview,
          c(
            paste0(var, delimiter, alternatives[j]),
            if (var == "ASC") NA_character_ else var,
            alternatives[j], FALSE, TRUE, var %in% re, var %in% md_ln
          )
        )
      }
    }
    for (var in vars[[3]]) {
      for (j in 1:J) {
        overview <- rbind(
          overview,
          c(
            paste0(var, delimiter, alternatives[j]), var, alternatives[j],
            TRUE, TRUE, var %in% re, var %in% md_ln
          )
        )
      }
    }
  }
  colnames(overview) <- c(
    "name", "covariate", "alternative", "as_covariate",
    "as_effect", "random", "log_normal"
  )
  overview$as_covariate <- as.logical(overview$as_covariate)
  overview$as_effect <- as.logical(overview$as_effect)
  overview$random <- as.logical(overview$random)
  overview$log_normal <- as.logical(overview$log_normal)

  ### sort effects
  effect_order <- order(
    as.numeric(overview$random), ### put random effects last
    as.numeric(overview$log_normal), ### log-normal effects are last random
    as.numeric(rownames(overview)), ### otherwise sort by occurrence in formula
    decreasing = FALSE
  )
  overview <- overview[effect_order, ]
  rownames(overview) <- NULL

  ### return effects
  return(overview)
}

#' Compute number of (fixed and random) model effects
#'
#' @description
#' These functions compute the number of fixed and random model effects.
#'
#' \code{coompute_P()} computes the total number \code{P} of model effects.
#' \code{compute_P_f()} computes the number \code{P_f} of fixed model effects.
#' \code{compute_P_r()} computes the number \code{P_r} of random model effects.
#'
#' @inheritParams RprobitB_formula
#' @inheritParams RprobitB_parameter
#' @inheritParams overview_effects
#'
#' @inheritSection RprobitB_formula Model formula
#' @inheritSection RprobitB_formula Random effects
#'
#' @return
#' An \code{integer}, the number of model effects.
#'
#' @examples
#' formula <- choice ~ A | B | C + D
#' re <- c("A", "D+")
#' J <- 3
#' compute_P(formula, re, J)
#' compute_P_f(formula, re, J)
#' compute_P_r(formula, re, J)
#'
#' @export

compute_P <- function(formula, re, J, ordered = FALSE) {
  compute_P_f(formula = formula, re = re, J = J, ordered = ordered) +
    compute_P_r(formula = formula, re = re, J = J, ordered = ordered)
}

#' @rdname compute_P
#' @export

compute_P_f <- function(formula, re, J, ordered = FALSE) {
  effects <- overview_effects(
    RprobitB_formula = RprobitB_formula(
      formula = formula, re = re,
      ordered = ordered
    ),
    RprobitB_alternatives = RprobitB_alternatives(J = J, ordered = ordered)
  )
  as.integer(sum(!effects$random))
}

#' @rdname compute_P
#' @export

compute_P_r <- function(formula, re, J, ordered = FALSE) {
  effects <- overview_effects(
    RprobitB_formula = RprobitB_formula(
      formula = formula, re = re,
      ordered = ordered
    ),
    RprobitB_alternatives = RprobitB_alternatives(J = J, ordered = ordered)
  )
  as.integer(sum(effects$random))
}
