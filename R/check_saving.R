#' Saving checks
#' @description Function that checks overwriting and saves model objects.
#' @details Model results with \code{model[["id"]]="test"} will be overwritting.
#' @param out A list of output settings.
#' @param objects A list of R objects.
#' @return No return value. Saves rds-files of elements in \code{objects} in folder "\code{out[["rdir"]]/out[["id"]]}".

check_saving = function(out,objects){
  path = paste0(out$rdir,"/",out$id)
  for(o in seq_len(length(objects))){
    object = objects[[o]]
    name = names(objects)[o]
    filename = paste0(path,"/",name)
    if(file.exists(filename) & path != paste0(out$rdir,"/test")){
      warning(paste0("Cannot save '",name,"' because '",filename,"' already exists."),call.=FALSE) 
    } else {
      saveRDS(object,file=paste0(filename,".rds"))
    }
  }
}