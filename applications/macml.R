### load Bayes code
rm(list = ls()); cat("\f")
try(setwd("RMACML"),silent=TRUE)
source("RprobitB/load_code.R")

### load MACML code
source("Rprobit/load_code.R")

### specify estimation
mod = list("N"    = 300,
           "alt"  = 3,
           "Tp"   = sample(5:10,300,replace=TRUE))
mod_macml = setup_macml(form = choice ~ V1 + V2 | 0 , mod = mod, re = c("V1"), norm_alt = mod$alt)
bayes_specs = list()
bayes_specs[["out"]]  = list("id" = "macml", "rdir" = "RprobitB/applications/results")
mod_macml$control$bayes = TRUE
mod_macml$control$bayes_specs = bayes_specs

### start estimation
mod_macml = fit_macml(mod_macml)
