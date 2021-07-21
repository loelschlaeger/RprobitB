print.RprobitB_data = function(RprobitB_data){

  if(!is.RprobitB_data(RprobitB_data)) stop()

  cat(separator(),"\n")
  cat("N=",RprobitB_data$N)
  cat(separator(),"\n")

  # cat(separator(),"\n")
  #
  # cat("observations:","\n-",N,ifelse(N==1,"decision maker","decision makers"),"\n")
  # if(length(unique(T))==1) cat("-",T[1],ifelse(unique(T)==1,"choice occasion","choice occasions"),ifelse(N==1,"","each"),"\n")
  # if(length(unique(T))>1) cat("-",min(T),"to",max(T),"choice occasions",ifelse(N==1,"","each"),"\n")
  #
  # cat("covariates:\n")
  # for(type in 1:3){
  #   for(var in vars[[type]]){
  #     cat("-",var)
  #     cat(" (")
  #     cat(paste(c(paste0("t",type),if(var %in% re){"re"},if(var %in% standardize){"z"}),collapse=", "))
  #     cat(")\n")
  #   }
  # }
  # if(ASC){
  #   cat("- ASC",if("ASC" %in% re){"(re)\n"})
  # }
  #
  # cat("alternatives:","\n")
  # for(i in 1:length(alternatives)){
  #   cat("-",alternatives[i],paste(c("(",i,")"),collapse=""),"\n")
  #   choice_data[["choice"]][choice_data[["choice"]]==alternatives[i]] = i
  # }
  # choice_data[["choice"]] = as.numeric(choice_data[["choice"]])
  # cat(seperator,"\n")

}
