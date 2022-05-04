
contraception_data <- readRDS("applications/contraception_choice/data/data_encoded.rds")
contraception_data <- subset(contraception_data,
                             subset = gender==1 & infert==0 & sex_freq>0,
                             select = c("id", "wave", "contr_method", "age", "sex_freq", "rel_stat", "child_wish"))

data_cc <- prepare_data(
  form = contr_method ~ 0 | age + sex_freq + rel_stat + child_wish,
  alternatives = c("0", "1", "2", "4", "1|2"),
  re = c("age", "sex_freq", "rel_stat", "child_wish"),
  standardize = "age",
  choice_data = contraception_data,
  id = "id",
  idc = "wave",
  impute = "complete_cases"
)

plot(data_cc, by_choice = TRUE)
saveRDS(data_cc, "applications/contraception_choice/data_cc.rds", compress = "xz")

model_contr_method <- fit_model(data = data_cc,
                           prior = list("delta" = 0.1),
                           latent_classes = list("C" = 10, "dp_update" = TRUE))
saveRDS(model_contr_method, "applications/contraception_choice/model_contr_method.rds", compress = "xz")
