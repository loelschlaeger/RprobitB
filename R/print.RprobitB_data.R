#' @export

print.RprobitB_data = function(x, ...){

  if(!is.RprobitB_data(x))
    stop("Not of class 'RprobitB_data'.")

  ### data
  cat(ifelse(x$simulated,"Simulated","Empirical"),"choice data\n")
  cat("*", x$N, paste0("decision maker",ifelse(x$N==1,"","s")), "with ")
  if(length(unique(x$T))==1)
    cat(x$T[1], paste0("choice occasion",ifelse(unique(x$T)==1,"","s")),
        ifelse(x$N==1,"","each"),"\n")
  if(length(unique(x$T))>1)
    cat(min(x$T),"to",max(x$T),"choice occasions",
        ifelse(x$N==1,"","each"),"\n")

  ### covariates
  cat("* Covariates:", paste(unlist(x$vars),collapse=", "), "\n")

  ### alternatives
  cat("* Alternatives:", paste(unlist(x$alternatives),collapse=", "))

  return(invisible(x))

}
