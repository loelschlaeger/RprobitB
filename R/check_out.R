#' Check \code{out}
#' @description Function that checks the input \code{out} and sets default values.
#' @param out A list of output settings.
#' @return \code{out}

check_out = function(out){
  
  ### check if out is a list
  if(!is.null(out)){
    stopifnot(is.list(out))
  } else {
    out = list()
  }
  
  ### set default values
  if(is.null(out$id)) out$id = "test"
  if(is.null(out$rdir)) out$rdir = tempdir()
  if(is.null(out$pp)) out$pp = FALSE
  if(is.null(out$return)) out$return = FALSE
  if(is.null(out$waic)) out$waic = FALSE
  
  ### check inputs
  stopifnot(is.character(c(out$id,out$rdir)))
  stopifnot(is.logical(c(out$pp,out$return,out$waic)))
  
  ### create output folders
  if(!dir.exists(out$rdir)) dir.create(out$rdir)
  if(out$id=="test"){
    if(!dir.exists(paste0(out$rdir,"/test"))) dir.create(paste0(out$rdir,"/test"))
  } else {
    if(dir.exists(paste0(out$rdir,"/",out$id))){
      stop(paste0("Folder ",out$rdir,"/",out$id,"' already exists."),call.=FALSE)
    } else {
      dir.create(paste0(out$rdir,"/",out$id))
    }
  }
  
  ### return out
  return(out)
}