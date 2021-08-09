#' Check \code{scale}
#' @description
#' Function that checks the input \code{scale}.
#' @details
#' Per default, the first error-term variance is fixed to \code{1}, i.e.
#' \code{scale = list("parameter" = "s", "index" = 1, "value" = 1)}. Note that
#' you can set \code{"parameter" = "a"} only if the model has parameters with a
#' fixed coefficient (i.e. \code{P_f}>0).
#' @inheritParams RprobitB_data
#' @param scale
#' A list of three elements, determining the parameter normalization with
#' respect to the scale of utility:
#' \itemize{
#'   \item \code{parameter}:
#'   either \code{"a"} (for a linear coefficient of \code{"alpha"}) or
#'   \code{"s"} (for a variance of the error-term covariance matrix
#'   \code{"Sigma"})
#'   \item \code{index}:
#'   the index of the parameter that gets fixed
#'   \item \code{value}:
#'   the value for the fixed parameter
#' }
#' @examples
#' check_scale(scale = NULL, P_f = 2, J = 3)
#' @return
#' The checked input \code{scale}

check_scale = function(scale, P_f, J) {

  ### check 'scale' based on 'P_f' and 'J' and set default values
  if(!is.null(scale))
    if(!is.list(scale))
      stop("'scale' must be a list")
  if(is.null(scale[["parameter"]]))
    scale[["parameter"]] = "s"
  if(!scale[["parameter"]] %in% c("s","a"))
    stop("'scale$parameter' must be one of 'a' or 's'.")
  if(is.null(scale[["index"]]))
    scale[["index"]] = 1
  if(scale[["parameter"]] == "a" &&
     !scale[["index"]] %in% seq_len(P_f))
    stop("'scale$index' is out of bound.")
  if(scale[["parameter"]] == "s" &&
     !scale[["index"]] %in% seq_len(J-1))
    stop("'scale$index' is out of bound.")
  if(is.null(scale[["value"]]))
    scale[["value"]] = 1
  if(!is.numeric(scale[["value"]]) || length(scale[["value"]])!=1 ||
     scale[["value"]] == 0)
    stop("'scale$value' must be a single numeric value not equal to zero.")
  if(scale[["parameter"]] == "s" && scale[["value"]] < 0)
    stop("'scale$value' must be non-negative.")

  ### add class to 'scale'
  class(scale) = "RprobitB_scale"

  ### return checked 'scale'
  return(scale)

}
