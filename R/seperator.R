#' Provide separator line
#' @description
#' Function that provides a separator line for text outputs.
#' @param length
#' @return
#' A separator line.

separator = function(length = 42) return(paste0(rep("-",length),collapse=""))
