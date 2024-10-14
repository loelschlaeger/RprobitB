#' Re-label alternative specific covariates
#'
#' @description
#' In \code{{RprobitB}}, alternative specific covariates must be named in the format
#' \code{"<covariate>_<alternative>"}. This convenience function generates
#' the format for a given \code{choice_data} set.
#'
#' @inheritParams prepare_data
#' @param cov
#' A character vector of the names of alternative specific covariates in
#' \code{choice_data}.
#' @param alternatives
#' A (character or numeric) vector of the alternative names.
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
