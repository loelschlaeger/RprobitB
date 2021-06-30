{
  ### load dependencies
  installed_packages = installed.packages()[,"Package"]
  required_packages = c("Rcpp",        ### required to include C++ code
                        "viridis",     ### color palette for visualizations
                        "mvtnorm")     ### required for computation of WAIC
  for(package in required_packages) if(!package %in% installed_packages) suppressMessages(install.packages(package))

  ### load .R files
  r_files = list.files(path="R/",pattern=".R")
  for(file in r_files) source(paste0("R/",file))

  ### load .cpp files
  cpp_files = list.files(path="src/",pattern=".cpp")
  cpp_files = cpp_files[-which(cpp_files %in% c("RcppExports.cpp","RcppExports.o"))]
  for(file in cpp_files) Rcpp::sourceCpp(paste0("src/",file))

  message("RprobitB installed.")
}
