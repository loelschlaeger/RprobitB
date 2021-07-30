#' Check \code{lcus}
#' @description
#' Function that checks the input \code{lcus} and sets missing values to
#' default values.
#' @param lcus
#' A list of latent class updating scheme parameters:
#' \itemize{
#'   \item \code{do_lcus}:
#'   A boolean determining whether to update the number of latent classes
#'   \item \code{C0}:
#'   The initial number of latent classes
#'   \item \code{Cmax}:
#'   The maximum number of latent classes
#'   \item \code{buffer}:
#'   The updating buffer (number of iterations to wait before the next update)
#'   \item \code{epsmin}:
#'   The threshold weight for removing latent classes (between 0 and 1)
#'   \item \code{epsmax}:
#'   The threshold weight for splitting latent classes (between 0 and 1)
#'   \item \code{distmin}:
#'   The threshold difference in means for joining latent classes (non-negative)
#' }
#' @examples
#' check_lcus(lcus = NULL)
#' check_lcus(lcus = list("do_lcus" = TRUE, "C0" = 5, "Cmax" = 10,
#'                        "buffer" = 100, "epsmin" = 0.01, "epsmax" = 0.99,
#'                        "distmin" = 0.1))
#' @return
#' The checked input \code{lcus}

check_lcus = function(lcus){

  ### check if lcus is a list
  if(!is.null(lcus)){
    if(!is.list(lcus))
      stop("'lcus' must be either 'NULL' or a list.")
  } else {
    lcus = list()
  }

  ### determine whether latent classes should be updated
  lcus$do_lcus = ifelse(is.na(lcus$do_lcus)||!is.logical(lcus$do_lcus),
                        FALSE,lcus$do_lcus)

  ### set missing parameters to default values
  if(lcus$do_lcus){
    lcus = list("do_lcus" = lcus$do_lcus,
                "C0"      = ifelse(is.null(lcus$C0),      5    ,lcus$C0),
                "Cmax"    = ifelse(is.null(lcus$Cmax),    10   ,lcus$Cmax),
                "buffer"  = ifelse(is.null(lcus$buffer),  100  ,lcus$buffer),
                "epsmin"  = ifelse(is.null(lcus$epsmin),  0.01 ,lcus$epsmin),
                "epsmax"  = ifelse(is.null(lcus$epsmax),  0.99 ,lcus$epsmax),
                "distmin" = ifelse(is.null(lcus$distmin), 0.1  ,lcus$distmin))
  }

  ### check lcus
  if(!is.logical(lcus$do_lcus))
    stop("'lcus$do_lcus' must be a boolean.")
  if(lcus$do_lcus){
    if(!is.natural.number(lcus$C0) || !lcus$C0>0)
      stop("'lcus$C0' must be a positive integer.")
    if(!is.natural.number(lcus$Cmax) || !lcus$Cmax>0 || !lcus$C0 <= lcus$Cmax)
      stop("'lcus$Cmax' must be a positive integer greater than 'lcus$0'.")
    if(!is.natural.number(lcus$buffer) || !lcus$buffer>0)
      stop("'lcus$buffer' must be a positive integer.")
    if(!is.numeric(lcus$epsmin) || !lcus$epsmin <= 1 || !lcus$epsmin >= 0)
      stop("'lcus$epsmin' must be a numeric between 0 and 1.")
    if(!is.numeric(lcus$epsmax) || !lcus$epsmax <= 1 || !lcus$epsmax >= 0 ||
       !lcus$epsmin < lcus$epsmax)
      stop("'lcus$epsmax' must be a numeric between 0 and 1 and greater than
           'lcus$epsmin'.")
    if(!is.numeric(lcus$distmin) || !0<=lcus$distmin)
      stop("'lcus$distmin' must be a non-negative numeric value.")
  }

  ### add class to 'lcus'
  class(lcus) = "RprobitB_lcus"

  ### return lcus
  return(lcus)
}
