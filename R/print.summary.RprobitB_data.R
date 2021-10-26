#' Print method for the summary of \code{RprobitB_data}.
#' @description
#' This function is the print method for an object of class
#' \code{summary.RprobitB_data}.
#' @param x
#' An object of class \code{summary.RprobitB_data}.
#' @param ...
#' Ignored.
#' @export

print.summary.RprobitB_data = function(x, ...) {

  cat("Summary of",ifelse(x$simulated,"simulated","empirical"),
      "choice data\n\n")

  ### summary of decision makers
  cat(x$N, paste0("decision maker",ifelse(x$N==1,"","s")),"\n")
  if(length(unique(x$T))==1)
    cat(x$T[1], paste0("choice occasion",ifelse(unique(x$T)==1,"","s")),
        ifelse(x$N==1,"","each"),"\n")
  if(length(unique(x$T))>1)
    cat(min(x$T),"to",max(x$T),"choice occasions",
        ifelse(x$N==1,"","each"),"\n")
  cat(sum(x$T),"choices in total\n")
  cat("\n")

  ### summary of alternatives
  cat("Alternatives\n")
  print(x$alternatives)
  cat("\n")

  ### summary of covariates
  cat("Covariates\n")
  print(x$covariates)
  if(!is.null(x$asc_alt)){
    cat("\n")
    message = ifelse(length(x$asc_alt)==1,
                     "ASC added for alternative",
                     "ASCs added for alternatives")
    cat(message,paste(x$asc_alt,collapse=", "),"\n")
  }
  return(invisible(x))
}
