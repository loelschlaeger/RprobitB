
# Child wish --------------------------------------------------------------

contraception_data <- readRDS("applications/contraception_choice/data/data_encoded.rds")
data <- prepare_data(
  form = child_wish ~ 0 | child_cond1 + child_cond2 + child_cond3 +
    child_cond4 + child_cond5 + child_cond6 + child_cond7 + child_cond8,
  choice_data = contraception_data,
  id = "id",
  idc = "wave",
  impute = "complete_cases"
)
# plot(data, by_choice = TRUE)
# saveRDS(data, "applications/contraception_choice/data.rds", compress = "xz")

mod_fix <- mcmc(data)
saveRDS(mod_fix, "applications/contraception_choice/mod_fix.rds", compress = "xz")

mod_re <- nested_model(mod_fix,
                       re = paste0("child_cond", 1:8))
saveRDS(mod_re, "applications/contraception_choice/mod_re.rds", compress = "xz")

# DOES C = 10 work?
mod_cl1 <- nested_model(mod_re,
                        prior = list("delta" = 0.1),
                        latent_classes = list("C = 10", "dp_update" = TRUE))
saveRDS(mod_cl1, "applications/contraception_choice/mod_cl1.rds", compress = "xz")

mod_cl2 <- nested_model(mod_cl1,
                        prior = list("delta" = 0.2))
saveRDS(mod_cl2, "applications/contraception_choice/mod_cl2.rds", compress = "xz")

mod_cl3 <- nested_model(mod_cl1,
                        prior = list("delta" = 0.3))
saveRDS(mod_cl3, "applications/contraception_choice/mod_cl3.rds", compress = "xz")




# Contraception choice ----------------------------------------------------


