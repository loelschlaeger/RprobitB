library("mlogit")
library("RprobitB")

data("Electricity", package = "mlogit")

Electricity <- as_cov_names(
 choice_data = Electricity,
 cov = c("pf", "cl", "loc", "wk", "tod", "seas"),
 alternatives = 1:4
)

### MNP
data_elec <- prepare_data(
  form = choice ~ pf + cl + loc + wk + tod + seas | 0,
  choice_data = Electricity,
  re = c("cl", "loc", "wk", "tod", "seas")
)

### MMNP
model_elec <- fit_model(data_elec, scale = "pf := -1")

### LC MNP


### LC MMNP
model_elec_lc <- fit_model(data_elec, scale = "pf := -1",
                           latent_classes = list("C" = 3))
