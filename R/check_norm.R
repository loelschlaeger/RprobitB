#' Check \code{norm}
#' @description Function that checks the input \code{norm} and sets default values. 
#' @param norm A list of normalization information.
#' @param model A list of model information.
#' @return \code{norm}

check_norm = function(norm,model){
  
  ### set default values
  if(is.null(norm$parameter)) norm$parameter = "s"
  if(is.null(norm$index)) norm$index = 1
  if(is.null(norm$value)) norm$value = 1
  
  ### check norm
  norm$parameter = tolower(gsub('\\d','',norm$parameter))
  stopifnot(norm$parameter %in% c("a","s"))
  if(model$P_f==0) stopifnot(norm$parameter=="s")
  norm$index = as.numeric(norm$index)
  if(norm$parameter == "a") stopifnot(norm$index %in% seq_len(model$P_f))
  if(norm$parameter == "s") stopifnot(norm$index %in% seq_len(model$J-1))
  norm$value = as.numeric(norm$value)
  if(norm$parameter == "s") stopifnot(norm$value>0)
  
  ### return norm
  return(norm)
}