#' Create object of class \code{RprobitB_normalization}.
#' @description
#' This function creates an object of class \code{RprobitB_normalization}.
#' @details
#' Any choice model has to be normalized with respect to level and scale.
#' \itemize{
#'   \item For level normalization, we takes utility differences with respect to
#'         one alternative.
#'   \item For scale normalization, we fix a model parameter. Per default, the
#'         first error-term variance is fixed to \code{1}, i.e.
#'         \code{scale = list("parameter" = "s", "index" = 1, "value" = 1)}.
#'         Alternatively, any error-term variance or any linear coefficient can
#'         be fixed.
#' }
#' @inheritParams RprobitB_data
#' @param level
#' The number of the alternative with respect which utility differences are
#' computed. Currently, only \code{level = J} (i.e. utility differences with
#' respect to the last alternative) is implemented.
#' @param scale
#' A named list of three elements, determining the parameter normalization with
#' respect to the utility scale:
#' \itemize{
#'   \item \code{parameter}:
#'   Either \code{"a"} (for a linear coefficient of \code{"alpha"}) or
#'   \code{"s"} (for a variance of the error-term covariance matrix
#'   \code{"Sigma"}).
#'   \item \code{index}:
#'   The index of the parameter that gets fixed.
#'   \item \code{value}:
#'   The value for the fixed parameter.
#' }
#' @return
#' An object of class \code{RprobitB_normalization}, which is a list of the
#' elements \code{level} and \code{scale}.

RprobitB_normalization = function(J, P_f, level = J,
                                  scale = list("parameter" = "s", "index" = 1,
                                               "value" = 1)) {

  ### check 'level' and 'scale' based on 'J' and 'P_f' and set default values
  if(level != J)
    stop("'level' must be equal to 'J'.")
  if(is.null(scale))
    scale = list()
  if(!is.list(scale))
    stop("'scale' must be a list")
  if(is.null(scale[["parameter"]]))
    scale[["parameter"]] = "s"
  if(!scale[["parameter"]] %in% c("s","a"))
    stop("'scale$parameter' must be one of 'a' or 's'.")
  if(is.null(scale[["index"]]))
    scale[["index"]] = 1
  if(scale[["parameter"]] == "a"){
    if(P_f == 0)
      stop("Cannot use 'alpha' for normalization because the model has no fixed coefficients.")
    if(!scale[["index"]] %in% seq_len(P_f))
      stop("'scale$index' is out of bound.")
  }
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

  ### create and return object of class 'RprobitB_normalization'
  out = list(level = level,
             scale = scale)
  class(out) = "RprobitB_normalization"
  return(out)

}
