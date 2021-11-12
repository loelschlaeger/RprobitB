#' Summary method for \code{RprobitB_data}.
#' @description
#' This function is the summary method for an object of class
#' \code{RprobitB_data}.
#' @param object
#' An object of class \code{RprobitB_data}.
#' @param ...
#' Ignored.
#' @export

summary.RprobitB_data = function(object, ...){

  ### check class of 'object'
  if(!inherits(object, "RprobitB_data"))
    stop("Not of class 'RprobitB_data'.")

  ### summary of alternatives
  alt = data.frame(matrix(NA, nrow = 0, ncol = 1))
  colnames(alt) = "frequency"
  for(i in 1:object$J){
    alt[nrow(alt)+1,] =
      sum(unlist(lapply(object$data, function(x) x[["y"]]))==i)
    rownames(alt)[nrow(alt)] = object$alternatives[i]
  }

  ### build 'summary.RprobitB_data' object
  out = list("simulated" = object$simulated,
             "N" = object$N,
             "T" = object$T,
             "linear_coeffs" = object$linear_coeffs,
             "alternatives" = alt)
  class(out) = "summary.RprobitB_data"

  ### return 'summary.RprobitB_data' object
  return(out)
}
