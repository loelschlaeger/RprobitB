#' Re-label alternative specific covariates
#'
#' @description
#' In \code{{RprobitB}}, alternative specific covariates must be named in the
#' format \code{"<covariate>_<alternative>"}. This helper function generates the
#' format for a given \code{choice_data} set.
#'
#' @param cov \[`character()`\]\cr
#' Names of alternative specific covariates in \code{choice_data}.
#'
#' @param alternatives \[`atomic()`\]\cr
#' The alternative names.
#'
#' @inheritParams prepare_data
#'
#' @return
#' The \code{choice_data} input with updated column names.
#'
#' @examples
#' data("Electricity", package = "mlogit")
#' cov <- c("pf", "cl", "loc", "wk", "tod", "seas")
#' alternatives <- 1:4
#' colnames(Electricity)
#' Electricity <- as_cov_names(Electricity, cov, alternatives)
#' colnames(Electricity)
#'
#' @export

as_cov_names <- function(choice_data, cov, alternatives) {

  ### input checks
  oeli::input_check_response(
    check = oeli::check_missing(choice_data),
    var_name = "choice_data"
  )
  oeli::input_check_response(
    check = checkmate::check_data_frame(choice_data),
    var_name = "choice_data"
  )
  oeli::input_check_response(
    check = oeli::check_missing(cov),
    var_name = "cov"
  )
  oeli::input_check_response(
    check = checkmate::check_character(cov),
    var_name = "cov"
  )
  oeli::input_check_response(
    check = oeli::check_missing(alternatives),
    var_name = "alternatives"
  )
  oeli::input_check_response(
    check = checkmate::check_atomic(alternatives),
    var_name = "alternatives"
  )

  ### change format
  x <- colnames(choice_data)
  for (i in seq_len(length(x))) {
    lab <- x[i]
    match_cov <- sapply(cov, function(x) grepl(x, lab))
    match_alt <- sapply(alternatives, function(x) grepl(x, lab))
    if (sum(match_cov) > 1 || sum(match_alt) > 1) {
      stop("Failed due to ambiguity.", call. = FALSE)
    } else if (sum(match_cov) == 1 && sum(match_alt) == 1) {
      x[i] <- paste0(cov[which(match_cov)], "_", alternatives[which(match_alt)])
    }
  }
  colnames(choice_data) <- x
  return(choice_data)
}
