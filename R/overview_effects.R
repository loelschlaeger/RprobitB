#' Print effect overview
#'
#' @description
#' This function gives an overview of the effect names, whether the covariate
#' is alternative-specific, whether the coefficient is alternative-specific,
#' and whether it is a random effect.
#'
#' @inheritParams RprobitB_data
#'
#' @return
#' A `data.frame`, each row is a effect, columns are the effect name
#' \code{"effect"}, and booleans whether the covariate is alternative-specific
#' \code{"as_value"}, whether the coefficient is alternative-specific
#' \code{"as_coef"}, and whether it is a random effect \code{"random"}.
#'
#' @examples
#' overview_effects(
#'   form = choice ~ price + time + comfort + change | 1,
#'   re = c("price", "time"),
#'   alternatives = c("A", "B"),
#'   base = "A"
#' )
#'
#' @export
#'
#' @seealso
#' [check_form()] for checking the model formula specification.

overview_effects <- function(
    form, re = NULL, alternatives, base = tail(alternatives, 1), ordered = FALSE
  ) {

  ### check input
  oeli::input_check_response(
    check = oeli::check_missing(form),
    var_name = "form"
  )
  oeli::input_check_response(
    check = checkmate::check_formula(form),
    var_name = "form"
  )
  oeli::input_check_response(
    check = checkmate::check_character(re, null.ok = TRUE),
    var_name = "re"
  )
  oeli::input_check_response(
    check = oeli::check_missing(alternatives),
    var_name = "alternatives"
  )
  oeli::input_check_response(
    check = checkmate::check_character(alternatives, min.len = 2),
    var_name = "alternatives"
  )
  oeli::input_check_response(
    check = checkmate::check_choice(base, alternatives, null.ok = TRUE),
    var_name = "base"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(ordered),
    var_name = "ordered"
  )

  ### check 'form'
  check_form_out <- check_form(form = form, re = re, ordered = ordered)
  re <- check_form_out$re
  vars <- check_form_out$vars
  ASC <- check_form_out$ASC

  ### build overview
  if (ordered) {
    overview <- data.frame()
    for (var in vars[[2]]) {
      overview <- rbind(overview, c(var, FALSE, FALSE, var %in% re))
    }
  } else {
    ### sort and count 'alternatives'
    if (!ordered) alternatives <- sort(alternatives)
    J <- length(alternatives)

    ### determine index of base alternative
    if (is.null(base)) {
      base_index <- J
    } else if (any(alternatives == base)) {
      base_index <- which(alternatives == base)
    } else {
      base <- alternatives[J]
      warning(
        paste0(
          "'base' not contained in 'alternatives'. ",
          "Set 'base = ", alternatives[J], "' instead."
        ),
        immediate. = TRUE, call. = FALSE
      )
      base_index <- J
    }

    ### determine names of linear coefficients
    overview <- data.frame()
    for (var in vars[[1]]) {
      overview <- rbind(overview, c(var, TRUE, FALSE, var %in% re))
    }
    for (var in c(vars[[2]], if (ASC) "ASC")) {
      for (j in (1:J)[-base_index]) {
        overview <- rbind(
          overview,
          c(
            paste0(var, "_", alternatives[j]), FALSE, TRUE,
            var %in% re
          )
        )
      }
    }
    for (var in vars[[3]]) {
      for (j in 1:J) {
        overview <- rbind(
          overview,
          c(
            paste0(var, "_", alternatives[j]), TRUE, TRUE,
            var %in% re
          )
        )
      }
    }
  }
  colnames(overview) <- c("effect", "as_value", "as_coef", "random")
  overview$random <- as.logical(overview$random)

  ### sort 'overview', first by 'random' and second by appearance in the formula
  overview <- overview[order(overview$random, as.numeric(rownames(overview))), ]
  rownames(overview) <- NULL

  ### return 'overview'
  return(overview)
}
