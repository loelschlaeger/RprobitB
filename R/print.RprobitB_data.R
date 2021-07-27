#' @export

print.RprobitB_data = function(x, ...){

  if(!is.RprobitB_data(x)) stop("wrong class")

  cat(ifelse(x$simulated,"Simulated","Empirical"),"choice data\n")
  cat("\n")

  cat("observations:\n")
  cat("-",x$N,ifelse(x$N==1,"decision maker","decision makers"),"\n")
  if(length(unique(x$T))==1)
    cat("-",x$T[1],
        ifelse(unique(x$T)==1,"choice occasion","choice occasions"),
        ifelse(x$N==1,"","each"),"\n")
  if(length(unique(x$T))>1)
    cat("-",min(x$T),"to",max(x$T),"choice occasions",
        ifelse(x$N==1,"","each"),"\n")
  cat("\n")

  cat("covariates:\n")
  for(type in 1:3){
    for(var in x$vars[[type]]){
      cat("-",var)
      cat(" (")
      cat(paste(c(paste0("type ",type),
                  if(var %in% unique(gsub("_.*$","",x$cov_random))){"re"},
                  if(var %in% standardize){"scaled"}), collapse=", "))
      cat(")\n")
    }
  }
  if(ASC) cat("- ASC",if("ASC" %in% re){"(re)\n"})
  cat("\n")

  cat("alternatives (occurence):","\n")
  for(i in 1:x$J)
    cat(paste0(i,"."),x$alternatives[i],
        paste0("(",sum(unlist(lapply(x$data,function(x)x$y))==i),")"),"\n")

}
