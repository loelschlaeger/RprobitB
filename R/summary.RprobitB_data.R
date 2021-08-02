#' For more details see the vignette "Data management":
#' \code{vignette("data_management", package = "RprobitB")}
#' @eobjectport

summary.RprobitB_data = function(object, ...){

  if(!is.RprobitB_data(object))
    stop("Not of class 'RprobitB_data'.")

  cat(ifelse(object$simulated,"Simulated","Empirical"),"choice data\n")
  cat("\n")

  cat("observations:\n")
  cat("-",object$N,ifelse(object$N==1,"decision maker","decision makers"),"\n")
  if(length(unique(object$T))==1)
    cat("-",object$T[1],
        ifelse(unique(object$T)==1,"choice occasion","choice occasions"),
        ifelse(object$N==1,"","each"),"\n")
  if(length(unique(object$T))>1)
    cat("-",min(object$T),"to",max(object$T),"choice occasions",
        ifelse(object$N==1,"","each"),"\n")
  cat("\n")

  cat("covariates:\n")
  for(type in 1:3){
    for(var in object$vars[[type]]){
      cat("-",var)
      cat(" (")
      cat(paste(c(paste0("type ",type),
                  if(var %in% unique(gsub("_.*$","",object$cov_random))){"re"},
                  if(var %in% object$standardize){"scaled"}), collapse=", "))
      cat(")\n")
    }
  }
  if(object$ASC)
    cat("- ASC",if("ASC" %in% object$re){"(re)"},"\n")
  cat("\n")

  cat("alternatives (occurence):","\n")
  for(i in 1:object$J)
    cat(paste0(i,"."),object$alternatives[i],
        paste0("(",sum(unlist(lapply(object$data,function(object)object$y))==i),")"),"\n")
}
