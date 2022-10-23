#' Overview model effects
#'
#' This function provides an overview of the model effects.
#'
#' @inheritParams RprobitB_formula
#' @inheritParams RprobitB_alternatives
#'
#' @inheritSection RprobitB_formula Model formula
#' @inheritSection RprobitB_formula Random effects
#' @inheritSection RprobitB_alternatives Base alternative
#'
#' @return
#' A \code{data.frame}, each row is an effect, columns are
#' 1. the effect names \code{"name"},
#' 2. booleans whether the covariate is alternative-specific \code{"as_cov"},
#' 3. booleans whether the coefficient is alternative-specific \code{"as_coef"},
#' 4. booleans whether the effect is a random effect \code{"random"},
#' 5. and booleans whether the random effect is log-normal \code{"ln"}.
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
    formula, re = NULL, alternatives, base = alternatives[1], ordered = FALSE
  ) {
  if (missing(formula)) {
    RprobitB_stop("Please specify the input 'formula'.")
  }
  if (missing(alternatives)) {
    RprobitB_stop("Please specify the input 'alternatives'.")
  }
  RprobitB_alternatives <- RprobitB_alternatives(
    alternatives = alternatives, base = base, ordered = ordered
  )
  alternatives <- RprobitB_alternatives$alternatives
  base <- RprobitB_alternatives$base
  RprobitB_formula <- RprobitB_formula(
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
          c(paste0(var, "_", alternatives[j]), FALSE, TRUE, var %in% re,
            var %in% md_ln)
        )
      }
    }
    for (var in vars[[3]]) {
      for (j in 1:J) {
        overview <- rbind(
          overview,
          c(paste0(var, "_", alternatives[j]), TRUE, TRUE, var %in% re,
            var %in% md_ln)
        )
      }
    }
  }
  colnames(overview) <- c("name", "as_cov", "as_coef", "random", "log_norm")
  overview$as_cov <- as.logical(overview$as_cov)
  overview$as_coef <- as.logical(overview$as_coef)
  overview$random <- as.logical(overview$random)
  overview$log_norm <- as.logical(overview$log_norm)
  effect_order <- order(overview$random, overview$log_norm,
                        as.numeric(rownames(overview)))
  overview <- overview[effect_order, ]
  rownames(overview) <- NULL
  return(overview)
}

#' Compute number of (fixed and random) model effects
#'
#' \code{coompute_P()} computes the total number \code{P} of model effects.
#' \code{compute_P_f()} computes the number \code{P_f} of fixed model effects.
#' \code{compute_P_r()} computes the number \code{P_r} of random model effects.
#'
#' @inheritParams RprobitB_formula
#' @inheritParams RprobitB_parameter
#'
#' @inheritSection RprobitB_formula Model formula
#' @inheritSection RprobitB_formula Random effects
#'
#' @return
#' An \code{integer}.
#'
#' @examples
#' formula <- choice ~ A | B + 0 | C + D
#' re <- c("A", "D+")
#' J <- 3
#' compute_P(formula, re, J)
#' cmopute_P_f(formula, re, J)
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
  RprobitB_effects <- RprobitB_effects(
    formula, re = re, alternatives = LETTERS[1:J], ordered = ordered
  )
  as.integer(sum(!RprobitB_effects$random))
}

#' @rdname compute_P
#' @export

compute_P_r <- function(formula, re, J, ordered = FALSE) {
  RprobitB_effects <- RprobitB_effects(
    formula, re = re, alternatives = LETTERS[1:J], ordered = ordered
  )
  as.integer(sum(RprobitB_effects$random))
}
