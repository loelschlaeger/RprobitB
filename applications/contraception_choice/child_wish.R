
contraception_data <- readRDS("applications/contraception_choice/data/data_encoded.rds")

data_cw <- prepare_data(
  form = child_wish ~ 0 | finance + plans + care + empl + leisure + empl_partner + household + ready,
  re = c("finance", "plans", "care", "empl", "leisure", "empl_partner", "household", "ready"),
  choice_data = contraception_data,
  id = "id",
  idc = "wave",
  impute = "complete_cases"
)

plot(data_cw, by_choice = TRUE)
saveRDS(data_cw, "applications/contraception_choice/data_cw.rds", compress = "xz")

model_child_wish <- fit_model(data = data_cw,
                         prior = list("delta" = 0.1),
                         latent_classes = list("C" = 5, "dp_update" = TRUE))
saveRDS(model_child_wish, "applications/contraception_choice/model_child_wish.rds", compress = "xz")
