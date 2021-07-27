#' Provide separator line
#' @description
#' Function that returns a separator line for text outputs.
#' @param length
#' @return
#' A separator line.

separator = function(length = 42){
  line = paste0(paste0(rep("-",length),collapse=""),"\n")
  return(line)
}
