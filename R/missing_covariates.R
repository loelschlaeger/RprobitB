#' Handle missing covariates
#'
#' @description
#' This function checks for and replaces missing covariate entries in
#' \code{choice_data}.
#'
#' @inheritParams prepare_data
#' @param impute
#' A character that specifies how to handle missing covariate entries in
#' \code{choice_data}, one of:
#' \itemize{
#'   \item \code{"complete_cases"}, removes all rows containing missing
#'   covariate entries (the default),
#'   \item \code{"zero"}, replaces missing covariate entries by zero
#'   (only for numeric columns),
#'   \item \code{"mean"}, imputes missing covariate entries by the mean
#'   (only for numeric columns).
#' }
#' @param col_ignore
#' A character vector of columns that are ignored (none per default).
#'
#' @return
#' The input \code{choice_data}, in which missing covariates are addressed.
#'
#' @keywords
#' internal

missing_covariates <- function(
    choice_data, impute = "complete_cases", col_ignore = character()) {
  ### check input
  if (!is.data.frame(choice_data)) {
    stop("'choice_data' must be a data frame.",
         call. = FALSE
    )
  }
  if (!(is.character(impute) && length(impute) == 1 &&
        impute %in% c("complete_cases", "zero", "mean"))) {
    stop(
      "'impute' must be either 'complete_cases', 'zero' or 'mean'.",
      call. = FALSE
    )
  }
  if (!is.character(col_ignore)) {
    stop(
      "'col_ignore' must be a character vector.",
      call. = FALSE
    )
  }

  ### index vector of columns
  ci <- which(!colnames(choice_data) %in% col_ignore)

  ### imputation
  RprobitB_pp("Checking for missing covariates")
  if (impute == "complete_cases") {
    choice_data <- choice_data[stats::complete.cases(choice_data[, ci]), ]
  }
  if (impute == "zero") {
    for (i in ci) {
      if (!is.numeric(choice_data[, i])) {
        warning(
          paste0(
            "Cannot impute column '", colnames(choice_data)[i],
            "' in 'choice_data' with zeros because it is not numeric."
          ),
          immediate. = TRUE, call. = FALSE
        )
      } else {
        choice_data[is.na(choice_data[, i]), i] <- 0
      }
    }
  }
  if (impute == "mean") {
    for (i in ci) {
      if (!is.numeric(choice_data[, i])) {
        warning(
          paste0(
            "Cannot impute column '", colnames(choice_data)[i],
            "' in 'choice_data' with mean because it is not numeric."
          ),
          immediate. = TRUE, call. = FALSE
        )
      } else {
        choice_data[is.na(choice_data[, i]), i] <- mean(choice_data[, i], na.rm = TRUE)
      }
    }
  }

  ### return updated 'choice_data' object
  return(choice_data)
}
