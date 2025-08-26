#' Extract covariates of choice occasion
#'
#' @description
#' This helper function returns the covariates and the choices of specific
#' choice occasions.
#'
#' @param x
#' Either an object of class \code{RprobitB_data} or \code{RprobitB_fit}.
#'
#' @param id,idc \[`integer()` | `NULL`\]\cr
#' Identifiers for deciders and choice occasions.
#'
#' If `NULL`, everything is returned.
#'
#' @param id_label,idc_label \[`character(1)` | `NULL`\]\cr
#' The columns that contain the decider and choice occasion identifier.
#'
#' If `NULL`, this information is extracted from `x`.
#'
#' @return
#' A subset of the `choice_data` data frame specified in `prepare_data()`.
#'
#' @export
#'
#' @examples
#' data <- simulate_choices(
#'   form = product ~ price,
#'   N = 10,
#'   T = 1:10,
#'   J = 3,
#'   ranked = TRUE
#' )
#' get_cov(data, id = 2)

get_cov <- function(
    x, id = NULL, idc = NULL, id_label = NULL, idc_label = NULL
  ) {
  oeli::input_check_response(
    check = list(
      checkmate::check_class(x, "RprobitB_data"),
      checkmate::check_class(x, "RprobitB_fit")
    ),
    var_name = "x"
  )
  if (inherits(x, "RprobitB_fit")) x <- x$data
  id_label <- if (is.null(id_label)) x$res_var_names$id else id_label
  idc_label <- if (is.null(idc_label)) x$res_var_names$idc else idc_label
  oeli::input_check_response(
    check = checkmate::check_string(id_label),
    var_name = "id_label"
  )
  oeli::input_check_response(
    check = checkmate::check_string(idc_label),
    var_name = "idc_label"
  )
  if (is.null(id)) id <- x$choice_data[[id_label]]
  if (is.null(idc)) idc <- x$choice_data[[idc_label]]
  oeli::input_check_response(
    check = checkmate::check_integerish(id, lower = 1, any.missing = FALSE),
    var_name = "id"
  )
  oeli::input_check_response(
    check = checkmate::check_integerish(idc, lower = 1, any.missing = FALSE),
    var_name = "idc"
  )
  ind <- x$choice_data[[id_label]] %in% id & x$choice_data[[idc_label]] %in% idc
  x$choice_data[ind, ]
}
