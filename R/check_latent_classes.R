#' Check \code{latent_classes}
#' @description
#' Function that checks the input \code{latent_classes} and sets missing values
#' to default values.
#' @param latent_classes
#' A list of parameters specifying the number and the updating scheme of latent
#' classes:
#' \itemize{
#'   \item \code{C}:
#'   The number (greater or equal 1) of latent classes. Set to 1 per default
#'   and is ignored if \code{P_r = 0}.
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
#' @examples
#' check_latent_classes(latent_classes = NULL)
#' check_latent_classes(latent_classes = list("C" = 2, "update" = TRUE,
#'                                            "Cmax" = 10, "buffer" = 100,
#'                                            "epsmin" = 0.01, "epsmax" = 0.99,
#'                                            "distmin" = 0.1))
#' @return
#' The checked input \code{latent_classes}

check_latent_classes = function(latent_classes){

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
           FALSE,latent_classes$update)

  if(!latent_classes$update){
    ### remove other parameters if 'latent_classes$update = FALSE'
    latent_classes = list("C" = latent_classes[["C"]], "update" = FALSE)

  } else {
    ### set missing parameters to default values
    if(is.null(latent_classes[["Cmax"]])) latent_classes[["Cmax"]] = 10
    if(is.null(latent_classes[["buffer"]])) latent_classes[["buffer"]] = 100
    if(is.null(latent_classes[["epsmin"]])) latent_classes[["epsmin"]] = 0.01
    if(is.null(latent_classes[["epsmax"]])) latent_classes[["epsmax"]] = 0.09
    if(is.null(latent_classes[["distmin"]])) latent_classes[["distmin"]] = 0.1

    ### remove redundant parameters
    names = c("C","update","Cmax","buffer","epsmin","epsmax","distmin")
    latent_classes[!names(latent_classes) %in% names] = NULL
  }

  ### check 'latent_classes'
  if(latent_classes$update){
    if(!is.numeric(latent_classes$C) || !latent_classes$C%%1 == 0 ||
       !latent_classes$C>0)
      stop("'latent_classes$C' must be a positive integer.")
    if(!is.numeric(latent_classes$Cmax) || !latent_classes$Cmax%%1 == 0 ||
       !latent_classes$Cmax>0 || !latent_classes$C <= latent_classes$Cmax)
      stop("'latent_classes$Cmax' must be a positive integer greater than
           'latent_classes$0'.")
    if(!is.numeric(latent_classes$buffer) || !latent_classes$buffer%%1 == 0 ||
       !latent_classes$buffer>0)
      stop("'latent_classes$buffer' must be a positive integer.")
    if(!is.numeric(latent_classes$epsmin) || !latent_classes$epsmin <= 1 ||
       !latent_classes$epsmin >= 0)
      stop("'latent_classes$epsmin' must be a numeric between 0 and 1.")
    if(!is.numeric(latent_classes$epsmax) || !latent_classes$epsmax <= 1 ||
       !latent_classes$epsmax >= 0 ||
       !latent_classes$epsmin < latent_classes$epsmax)
      stop("'latent_classes$epsmax' must be a numeric between 0 and 1 and
           greater than 'latent_classes$epsmin'.")
    if(!is.numeric(latent_classes$distmin) || !0<=latent_classes$distmin)
      stop("'latent_classes$distmin' must be a non-negative numeric value.")
  }

  ### add class to 'latent_classes'
  class(latent_classes) = "RprobitB_latent_classes"

  ### return latent_classes
  return(latent_classes)
}
