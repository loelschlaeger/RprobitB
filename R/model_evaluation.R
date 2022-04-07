#' Get covariates of choice situation
#'
#' @description
#' This convenience function returns the covariates and the choices of specific
#' choice occasions.
#'
#' @param x
#' Either an object of class \code{RprobitB_data} or \code{RprobitB_fit}.
#' @param id
#' A numeric (vector), that specifies the decider(s).
#' @param idc
#' A numeric (vector), that specifies the choice occasion(s).
#' @param idc_label
#' The name of the column that contains the choice occasion identifier.
#' @return
#' A subset of the `choice_data` data frame specified in `prepare_data()`.
#'
#' @examples
#' data("model_train", package = "RprobitB")
#' get_cov(model_train, id = 1:2, idc = 1:2, idc_label = "choiceid")
#'
#' @export

get_cov <- function(x, id, idc, idc_label){
  if(inherits(x, "RprobitB_fit")){
    x <- x$data
  }
  if(inherits(x, "RprobitB_data")){
    id_label <- x$res_var_names$id
    idc_label <- if(missing(idc_label)) x$res_var_names$idc else idc_label
    if(missing(id)) id <- x$choice_data[[id_label]]
    if(missing(idc)) idc <- x$choice_data[[idc_label]]
    ind <- x$choice_data[[id_label]] %in% id & x$choice_data[[idc_label]] %in% idc
    out <- x$choice_data[ind,]
    if(nrow(out) == 0){
      stop("Requested choice occasion not found.", call. = FALSE)
    }
    return(out)
  } else {
    stop("'x' must be either an 'RprobitB_fit' or 'RprobitB_data' object.",
         call. = FALSE)
  }
}
