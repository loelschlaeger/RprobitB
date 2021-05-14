### load Bayes code
rm(list = ls()); cat("\f")
try(setwd("RMACML"))
source("RprobitB/load_code.R")

set.seed(1)

### model specification
model = list("N" = 100, "T" = sample(10:20,100,replace=TRUE), "J" = 3, "P_f" = 1, "P_r" = 2, "C" = 2)
lcus  = list("do_lcus" = TRUE)
mcmc  = list("R" = 20000)

### start estimation (about 3 minutes computation time)
RprobitB::rpb("model" = model, "lcus" = lcus, "mcmc" = mcmc)