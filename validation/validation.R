### load Bayes code
rm(list = ls()); cat("\f")
try(setwd("RMACML"),silent=TRUE)
source("RprobitB/load_code.R")

### load validation code
source("RprobitB/validation/validation_help.R")

### set validation output
folder = paste0("RprobitB/validation/",Sys.Date())
dir.create(folder,showWarnings=FALSE)
file = paste0(folder,"/validation.txt")
file.create(file,showWarnings=FALSE)

### perform validation runs
run = 1
while(TRUE){
  valid_run = tryCatch({
    valid(id = sprintf("%05d",run), rdir = paste0(folder))
  }, message = function(e) {
    return(e)
  })
  write(paste(sprintf("%05d",run),valid_run$message),file,append=TRUE)
  run = run + 1
}
