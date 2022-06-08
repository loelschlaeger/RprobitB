
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

### ordered choices
choice_data <- data_cw$choice_data[c("child_wish","id","wave","age","rel_stat")]
choice_data$child_wish <- as.character(choice_data$child_wish)
for(i in 1:nrow(choice_data)){
  if(choice_data$child_wish[i] == "0"){
    choice_data$child_wish[i] <- sample(c("no","rather no"), 1)
  } else if (choice_data$child_wish[i] == "1") {
    choice_data$child_wish[i] <- sample(c("rather yes","yes"), 1)
  } else { stop() }
}
form <- child_wish ~ age + rel_stat
re <- "rel_stat"
alternatives <- c("no","rather no","rather yes","yes")
ordered <- TRUE
ranked <- FALSE
base <- NULL
id <- "id"
idc <- "wave"
standardize <- "age"
impute <- "complete_cases"
data <- prepare_data(form = form, choice_data = choice_data, re = re,
                     alternatives = alternatives, ordered = ordered,
                     ranked = ranked, base = base, id = id, idc = idc,
                     standardize = standardize, impute = impute)



