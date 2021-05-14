#' Check \code{lcus}
#' @description Function that checks the input \code{lcus} and sets default values. 
#' @param lcus A list of latent class updating scheme parameters.
#' @param model A list of model information.
#' @return \code{lcus}

check_lcus = function(lcus,model){
  
  ### check if lcus is a list
  if(!is.null(lcus)){
    stopifnot(is.list(lcus))
  } else {
    lcus = list()
  }
  
  ### determine whether latent classes should be updated
  if(model$P_r==0){
    lcus$do_lcus = FALSE
  } else {
    lcus$do_lcus = ifelse(is.na(lcus$do_lcus)||!is.logical(lcus$do_lcus),FALSE,lcus$do_lcus)
  }
  
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
  stopifnot(lcus$C0 <= lcus$Cmax)
  stopifnot(lcus$epsmin <= 1)
  stopifnot(lcus$epsmin >= 0)
  stopifnot(lcus$epsmax <= 1)
  stopifnot(lcus$epsmax >= 0)
  stopifnot(lcus$epsmin <= lcus$epsmax)
  stopifnot(lcus$distmin > 0)
  
  ### return lcus
  return(lcus)
}