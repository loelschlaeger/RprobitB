{
  ### load dependencies
  installed_packages = installed.packages()[,"Package"]
  required_packages = c("Rcpp",        ### required to include C++ code
                        "viridis",     ### color palette for visualizations
                        "mvtnorm")     ### required for computation of WAIC
  for(package in required_packages){
    message(sprintf("Loading packages: %.0f%%",(which(required_packages==package)/length(required_packages)*100)),"\r",appendLF=FALSE)
    if(!package %in% installed_packages) suppressMessages(install.packages(package))
  }

  ### load .R files
  r_files = list.files(path="R/",pattern=".R")
  for(file in r_files){
    message(sprintf("Loading .R files: %.0f%%",((which(r_files==file)-1)/length(r_files)*100)),"\r",appendLF=FALSE)
    source(paste0("R/",file))
  }

  ### load .cpp files
  cpp_files = list.files(path="src/",pattern=".cpp")
  cpp_files = cpp_files[-which(cpp_files %in% c("RcppExports.cpp","RcppExports.o"))]
  for(file in cpp_files){
    message(sprintf("Loading .cpp files: %.0f%%",((which(cpp_files==file)-1)/length(cpp_files)*100)),"\r",appendLF=FALSE)
    Rcpp::sourceCpp(paste0("src/",file))
  }

  message(sprintf("Code loaded. %15s"," "))
}
