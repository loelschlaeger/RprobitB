#' Overview of model effects
#'
#' @description
#' This function provides an overview of the model effects.
#'
#' @inheritParams new_RprobitB_formula
#' @inheritParams new_RprobitB_alternatives
#'
#' @inheritSection new_RprobitB_formula Details of model specification
#' @inheritSection new_RprobitB_formula Details of random effects
#' @inheritSection new_RprobitB_alternatives Details of the base alternative
#'
#' @return
#' A \code{data frame}, each row is an effect, columns are
#' * the effect names \code{"name"},
#' * booleans whether the covariate is alternative-specific \code{"as_cov"},
#' * booleans whether the coefficient is alternative-specific \code{"as_coef"},
#' * booleans whether the effect is a random effect \code{"random"},
#' * and booleans whether the random effect is log-normal \code{"ln"}.
#'
#' @examples
#' RprobitB_effects(
#'   formula = choice ~ price | income | comfort,
#'   re = c("price+", "income"),
#'   alternatives = c("A", "B"),
#'   base = "A"
#' )
#'
#' @export

RprobitB_effects <- function(
    formula, re = NULL, alternatives, base = tail(alternatives, 1),
    ordered = FALSE
  ) {
  if (missing(formula)) {
    RprobitB_stop("Please specify the input 'formula'.")
  }
  if (missing(alternatives)) {
    RprobitB_stop("Please specify the input 'alternatives'.")
  }
  RprobitB_alternatives <- new_RprobitB_alternatives(
    alternatives = alternatives, base = base, ordered = ordered
  )
  alternatives <- RprobitB_alternatives$alternatives
  base <- RprobitB_alternatives$base
  RprobitB_formula <- new_RprobitB_formula(
    formula = formula, re = re, ordered = ordered
  )
  vars <- RprobitB_formula$vars
  md_n <- RprobitB_formula$md_n
  md_ln <- RprobitB_formula$md_ln
  re <- c(md_n, md_ln)
  overview <- data.frame(matrix(ncol = 5, nrow = 0))
  if(ordered){
    for (var in vars[[2]]) {
      overview <- rbind(
        overview,
        c(var, FALSE, FALSE, var %in% re, var %in% md_ln)
      )
    }
  } else {
    J <- length(alternatives)
    for (var in vars[[1]]) {
      overview <- rbind(
        overview,
        c(var, TRUE, FALSE, var %in% re, var %in% md_ln)
      )
    }
    for (var in c(vars[[2]], if (RprobitB_formula$ASC) "ASC")) {
      for (j in (1:J)[-which(alternatives == base)]) {
        overview <- rbind(
          overview,
          c(paste0(var, "_", alternatives[j]), FALSE, TRUE, var %in% re, var %in% md_ln)
        )
      }
    }
    for (var in vars[[3]]) {
      for (j in 1:J) {
        overview <- rbind(
          overview,
          c(paste0(var, "_", alternatives[j]), TRUE, TRUE, var %in% re, var %in% md_ln)
        )
      }
    }
  }
  colnames(overview) <- c("name", "as_cov", "as_coef", "random", "log_norm")
  overview$as_cov <- as.logical(overview$as_cov)
  overview$as_coef <- as.logical(overview$as_coef)
  overview$random <- as.logical(overview$random)
  overview$log_norm <- as.logical(overview$log_norm)
  effect_order <- order(overview$random, overview$log_norm, as.numeric(rownames(overview)))
  overview <- overview[effect_order, ]
  rownames(overview) <- NULL
  return(overview)
}

#' Compute number of (fixed and random) model effects
#'
#' @description
#' \code{P()} computes the total number \code{P} of model effects.
#'
#' \code{P_f()} computes the number \code{P_f} of fixed model effects.
#'
#' \code{P_r()} computes the number \code{P_r} of random model effects.
#'
#' @inheritParams new_RprobitB_formula
#' @param J
#' An integer, the number of choice alternatives.
#'
#' @inheritSection new_RprobitB_formula Details of model specification
#' @inheritSection new_RprobitB_formula Details of random effects
#'
#' @return
#' An integer
#'
#' @examples
#' formula <- choice ~ A | B + 0 | C + D
#' re <- c("A", "D+")
#' J <- 3
#' P(formula, re, J)
#' P_f(formula, re, J)
#' P_r(formula, re, J)
#'
#' @export

P <- function(formula, re, J, ordered = FALSE) {
  P_f(formula = formula, re = re, J = J, ordered = ordered) +
    P_r(formula = formula, re = re, J = J, ordered = ordered)
}

#' @rdname P
#' @export

P_f <- function(formula, re, J, ordered = FALSE) {
  RprobitB_effects <- RprobitB_effects(
    formula, re = re, alternatives = LETTERS[1:J], ordered = ordered
  )
  as.integer(sum(!RprobitB_effects$random))
}

#' @rdname P
#' @export

P_r <- function(formula, re, J, ordered = FALSE) {
  RprobitB_effects <- RprobitB_effects(
    formula, re = re, alternatives = LETTERS[1:J], ordered = ordered
  )
  as.integer(sum(RprobitB_effects$random))
}
