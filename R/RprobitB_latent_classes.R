#' Create object of class \code{RprobitB_latent_classes}.
#' @description
#' This function creates an object of class \code{RprobitB_latent_classes}.
#' @param latent_classes
#' Either \code{NULL} or a list of parameters specifying the number and the
#' latent classes:
#' \itemize{
#'   \item \code{C}:
#'   The number (greater or equal 1) of latent classes, which is set to 1 per
#'   default and is ignored if \code{P_r = 0}.
#'   If \code{update = TRUE}, \code{C} equals the initial number of latent
#'   classes and is set to 5 per default.
#'   \item \code{update}:
#'   A boolean, determining whether to update \code{C}. Ignored if
#'   \code{P_r = 0}. If \code{update = FALSE}, all of the following elements are
#'   ignored.
#'   \item \code{Cmax}:
#'   The maximum number of latent classes.
#'   \item \code{buffer}:
#'   The updating buffer (number of iterations to wait before the next update).
#'   \item \code{epsmin}:
#'   The threshold weight for removing latent classes (between 0 and 1).
#'   \item \code{epsmax}:
#'   The threshold weight for splitting latent classes (between 0 and 1).
#'   \item \code{distmin}:
#'   The threshold difference in means for joining latent classes
#'   (non-negative).
#' }
#' @return
#' An object of class \code{RprobitB_latent_classes}.
#' @keywords
#' s3

RprobitB_latent_classes = function(latent_classes = NULL){

  ### check if 'latent_classes' is a list
  if(!is.null(latent_classes)){
    if(!is.list(latent_classes))
      stop("'latent_classes' must be either 'NULL' or a list.")
  } else {
    latent_classes = list()
  }

  ### set default number of latent classes
  if(is.null(latent_classes[["C"]])) latent_classes[["C"]] = 1

  ### determine whether latent classes should be updated
  latent_classes$update =
    ifelse(is.na(latent_classes$update) || !is.logical(latent_classes$update),
           FALSE, latent_classes$update)

  if(!latent_classes$update){
    ### remove other parameters if 'latent_classes$update = FALSE'
    latent_classes = list("C" = latent_classes[["C"]], "update" = FALSE)

  } else {
    ### if 'latent_classes$update = TRUE', 'latent_classes$C' is initial number
    latent_classes[["C"]] = 5

    ### set missing parameters to default values
    if(is.null(latent_classes[["Cmax"]])) latent_classes[["Cmax"]] = 10
    if(is.null(latent_classes[["buffer"]])) latent_classes[["buffer"]] = 100
    if(is.null(latent_classes[["epsmin"]])) latent_classes[["epsmin"]] = 0.01
    if(is.null(latent_classes[["epsmax"]])) latent_classes[["epsmax"]] = 0.99
    if(is.null(latent_classes[["distmin"]])) latent_classes[["distmin"]] = 0.1

    ### remove redundant parameters
    names = c("C","update","Cmax","buffer","epsmin","epsmax","distmin")
    latent_classes[!names(latent_classes) %in% names] = NULL
  }

  ### check 'latent_classes'
  if(latent_classes$update){
    if(!is.numeric(latent_classes$C) || !latent_classes$C%%1 == 0 ||
       !latent_classes$C>0 || !latent_classes$C <= latent_classes$Cmax)
      stop("'latent_classes$C' must be a positive integer less or equal than",
           "'latent_classes$Cmax'.")
    if(!is.numeric(latent_classes$Cmax) || !latent_classes$Cmax%%1 == 0 ||
       !latent_classes$Cmax>0 || !latent_classes$C <= latent_classes$Cmax)
      stop("'latent_classes$Cmax' must be a positive integer greater or equal",
           "than 'latent_classes$C'.")
    if(!is.numeric(latent_classes$buffer) || !latent_classes$buffer%%1 == 0 ||
       !latent_classes$buffer>0)
      stop("'latent_classes$buffer' must be a positive integer.")
    if(!is.numeric(latent_classes$epsmin) || !latent_classes$epsmin <= 1 ||
       !latent_classes$epsmin >= 0)
      stop("'latent_classes$epsmin' must be a numeric between 0 and 1.")
    if(!is.numeric(latent_classes$epsmax) || !latent_classes$epsmax <= 1 ||
       !latent_classes$epsmax >= 0 ||
       !latent_classes$epsmin < latent_classes$epsmax)
      stop("'latent_classes$epsmax' must be a numeric between 0 and 1 and",
           "greater than 'latent_classes$epsmin'.")
    if(!is.numeric(latent_classes$distmin) || !0<=latent_classes$distmin)
      stop("'latent_classes$distmin' must be a non-negative numeric value.")
  }

  ### add class to 'latent_classes'
  class(latent_classes) = "RprobitB_latent_classes"

  ### return latent_classes
  return(latent_classes)
}
