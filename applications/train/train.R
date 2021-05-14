### load Bayes code
rm(list = ls()); cat("\f")
try(setwd("RMACML"),silent=TRUE)
source("RprobitB/load_code.R")

### load Train data
data("Train", package = "mlogit")

### Fixed number of latent classes, C = 1
rpb("model" = list("P_f" = 2, "P_r" = 2, "C" = 1), 
    "mcmc"  = list("R" = 1e5), 
    "data"  = list("data_raw" = Train, "cov_col" = 4:11, "cov_ord" = c("price","comfort","time","change"), "cov_zst" = TRUE),
    "norm"  = list("parameter" = "a", "index" = 1, "value" = -1), 
    "out"   = list("id" = "train_C1", "rdir" = "RprobitB/applications/results", "pp" = TRUE, "waic" = TRUE))

### Fixed number of latent classes, C = 2 
rpb("model" = list("P_f" = 2, "P_r" = 2, "C" = 2), 
    "mcmc"  = list("R" = 1e5), 
    "data"  = list("data_raw" = Train, "cov_col" = 4:11, "cov_ord" = c("price","comfort","time","change"), "cov_zst" = TRUE), 
    "norm"  = list("parameter" = "a", "index" = 1, "value" = -1), 
    "out"   = list("id" = "train_C2", "rdir" = "RprobitB/applications/results", "pp" = TRUE, "waic" = TRUE))

### Fixed number of latent classes, C = 3
rpb("model" = list("P_f" = 2, "P_r" = 2, "C" = 3), 
    "mcmc"  = list("R" = 1e5), 
    "data"  = list("data_raw" = Train, "cov_col" = 4:11, "cov_ord" = c("price","comfort","time","change"), "cov_zst" = TRUE), 
    "norm"  = list("parameter" = "a", "index" = 1, "value" = -1), 
    "out"   = list("id" = "train_C3", "rdir" = "RprobitB/applications/results", "pp" = TRUE, "waic" = TRUE))

### Updated number of latent classes
rpb("model" = list("P_f" = 2, "P_r" = 2), 
    "mcmc"  = list("R" = 1e5), 
    "data"  = list("data_raw" = Train, "cov_col" = 4:11, "cov_ord" = c("price","comfort","time","change"), "cov_zst" = TRUE), 
    "norm"  = list("parameter" = "a", "index" = 1, "value" = -1), 
    "lcus"  = list("do_lcus" = TRUE), 
    "out"   = list("id" = "train_Cf", "rdir" = "RprobitB/applications/results", "pp" = TRUE, "waic" = TRUE))

### Only time random, fixed number of latent classes, C = 1
rpb("model" = list("P_f" = 3, "P_r" = 1, "C" = 1), 
    "mcmc"  = list("R" = 1e5), 
    "data"  = list("data_raw" = Train, "cov_col" = 4:11, "cov_ord" = c("price","comfort","change","time"), "cov_zst" = TRUE), 
    "norm"  = list("parameter" = "a", "index" = 1, "value" = -1), 
    "out"   = list("id" = "train_random_time_C1", "rdir" = "RprobitB/applications/results", "pp" = TRUE, "waic" = TRUE))

### Only time random, fixed number of latent classes, C = 2
rpb("model" = list("P_f" = 3, "P_r" = 1, "C" = 2), 
    "mcmc"  = list("R" = 1e5), 
    "data"  = list("data_raw" = Train, "cov_col" = 4:11, "cov_ord" = c("price","comfort","change","time"), "cov_zst" = TRUE), 
    "norm"  = list("parameter" = "a", "index" = 1, "value" = -1), 
    "out"   = list("id" = "train_random_time_C2", "rdir" = "RprobitB/applications/results", "pp" = TRUE, "waic" = TRUE))

### Only time random, updated number of latent classes
rpb("model" = list("P_f" = 3, "P_r" = 1), 
    "mcmc"  = list("R" = 1e5), 
    "data"  = list("data_raw" = Train, "cov_col" = 4:11, "cov_ord" = c("price","comfort","change","time"), "cov_zst" = TRUE), 
    "norm"  = list("parameter" = "a", "index" = 1, "value" = -1), 
    "lcus"  = list("do_lcus" = TRUE), 
    "out"   = list("id" = "train_random_time_Cf", "rdir" = "RprobitB/applications/results", "pp" = TRUE, "waic" = TRUE))
