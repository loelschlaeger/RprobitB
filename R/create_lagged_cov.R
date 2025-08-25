#' Create lagged choice covariates
#'
#' @description
#' This function creates lagged choice covariates from the \code{data.frame}
#' \code{choice_data}, which is assumed to be sorted by the choice occasions.
#'
#' @details
#' Say that \code{choice_data} contains the column \code{column}. Then, the
#' function call
#' \preformatted{
#' create_lagged_cov(choice_data, column, k, id)
#' }
#' returns the input \code{choice_data} which includes a new column named
#' \code{column.k}. This column contains for each decider (based on \code{id})
#' and each choice occasion the covariate faced before \code{k} choice
#' occasions. If this data point is not available, it is set to
#' \code{NA}. In particular, the first \code{k} values of \code{column.k} will
#' be \code{NA} (initial condition problem).
#'
#' @param column \[`character()`\]\cr
#' Covariate names in \code{choice_data}.
#'
#' @param k \[`integer()`\]\cr
#' The number of lags (in units of observations), see the details.
#'
#' @inheritParams prepare_data
#'
#' @return
#' The input \code{choice_data} with the additional columns named
#' \code{column.k} for each element \code{column} and each number \code{k}
#' containing the lagged covariates.
#'
#' @examples
#' choice_data <- data.frame(id = rep(1:2, each = 3), cov = LETTERS[1:6])
#' create_lagged_cov(choice_data, column = "cov", k = 1:2)
#'
#' @export

create_lagged_cov <- function(
    choice_data, column = character(), k = 1, id = "id"
  ) {

  ### check inputs
  oeli::input_check_response(
    check = oeli::check_missing(choice_data),
    var_name = "choice_data"
  )
  oeli::input_check_response(
    check = checkmate::check_data_frame(choice_data),
    var_name = "choice_data"
  )
  oeli::input_check_response(
    check = checkmate::check_character(column),
    var_name = "column"
  )
  oeli::input_check_response(
    check = checkmate::check_integerish(k, lower = 1),
    var_name = "k"
  )
  oeli::input_check_response(
    check = checkmate::check_string(id),
    var_name = "id"
  )
  oeli::input_check_response(
    check = checkmate::check_subset(c(column, id), colnames(choice_data)),
    var_name = "colnames(choice_data)"
  )

  ### loop over 'column' and 'k'
  for (col in column) {
    for (k_val in k) {
      ### check if new columns already exist in 'choice_data'
      col_new <- paste(col, k_val, sep = ".")
      if (col_new %in% colnames(choice_data)) {
        warning(
          paste0("Column '", col_new, "' already exists in 'choice_data'."),
          call. = FALSE, immediate. = TRUE
        )
        next()
      }

      ### add column 'col.k'
      cols_old <- colnames(choice_data)
      choice_data <- cbind(choice_data, NA_real_)
      colnames(choice_data) <- c(cols_old, col_new)

      ### preserve factors
      if (is.factor(choice_data[[col]])) {
        choice_data[[col_new]] <- factor(
          choice_data[[col_new]], levels = levels(choice_data[[col]])
        )
      }

      ### build progress bar
      pb <- RprobitB_pb(
        title = paste("create", col_new),
        total = length(unique(choice_data[[id]])),
        tail = "deciders"
      )

      ### create lagged covariate 'col.k'
      for (id_val in unique(choice_data[[id]])) {
        RprobitB_pb_tick(pb)
        id_rows <- which(choice_data[[id]] == id_val)
        i <- seq_along(id_rows)[-(1:k_val)]
        choice_data[id_rows[i], col_new] <- choice_data[id_rows[i - k_val], col]
      }
    }
  }

  ### return updated 'choice_data'
  return(choice_data)
}
