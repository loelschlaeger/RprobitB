#' Print method for \code{RprobitB_latent_classes}.
#' @description
#' This function is the print method for an object of class
#' \code{RprobitB_latent_classes}.
#' @param x
#' An object of class \code{RprobitB_latent_classes}.
#' @param ...
#' Ignored.
#' @export

print.RprobitB_latent_classes = function(x, ...) {
  cat("Latent classes:",x$C,"\n")
  cat(paste("- Update:",x$update,"\n"))
  if(x$update){
    cat(paste("- Maximum number:",x$Cmax,"\n"))
    cat(paste("- Buffer:",x$buffer,"\n"))
    cat(paste("- Minimum class weight:",x$epsmin,"\n"))
    cat(paste("- Maximum class weight:",x$epsmax,"\n"))
    cat(paste("- Mimumum class distance:",x$distmin,"\n"))
  }
  return(invisible(x))
}
