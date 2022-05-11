### R code from vignette source 'rprobitb_oelschlaeger_bauer.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
#library(RprobitB) # UNCOMMENT!


###################################################
### code chunk number 2: prepare data call (eval = FALSE)
###################################################
## data <- prepare_data(form = form, choice_data = choice_data)


###################################################
### code chunk number 3: example 1 train overview
###################################################
data("Train", package = "mlogit")
Train$price_A <- Train$price_A / 100 * 2.20371
Train$price_B <- Train$price_B / 100 * 2.20371
Train$time_A <- Train$time_A / 60
Train$time_B <- Train$time_B / 60
str(Train)


###################################################
### code chunk number 4: example 1 train formula
###################################################
form <- choice ~ price + time + comfort + change | 0


###################################################
### code chunk number 5: example 1 train prepare
###################################################
data_train <- prepare_data(
  form = form, choice_data = Train, id = "id", idc = "choiceid"
)


###################################################
### code chunk number 6: example 1 train summary
###################################################
summary(data_train)


###################################################
### code chunk number 7: train-data
###################################################
plot(data_train)


###################################################
### code chunk number 8: simulate choices call (eval = FALSE)
###################################################
## data <- simulate_choices(form = form, N = N, T = T, J = J)


###################################################
### code chunk number 9: example 2 sim meta
###################################################
N <- 100
T <- 30
alternatives <- c("alt1", "alt2")
form <- choice ~ var1 | var2 | var3
re <- c("ASC","var2")


###################################################
### code chunk number 10: example 2 sim overview effects
###################################################
overview_effects(form = form, re = re, alternatives = alternatives)


###################################################
### code chunk number 11: example 2 sim simulation
###################################################
data_sim <- simulate_choices(
  form = form, N = N, T = T, J = 2,
  re = re, alternatives = alternatives, seed = 1,
  true_parameter = list(alpha = c(-1,0,1), C = 2, s = c(0.7,0.3),
                        b = matrix(c(2,-0.5,1,1), ncol = 2), Sigma = 1)
)


###################################################
### code chunk number 12: sim-data
###################################################
plot(data_sim, by_choice = TRUE)


###################################################
### code chunk number 13: fit_model call (eval = FALSE)
###################################################
## fit_model(data = data)


###################################################
### code chunk number 14: example 1 train fit
###################################################
model_train <- fit_model(data = data_train, R = 1000, scale = price ~ -1)


###################################################
### code chunk number 15: coef-model-train
###################################################
plot(coef(model_train), sd = 3)


###################################################
### code chunk number 16: example 1 train summary
###################################################
FUN <- c("mean" = mean, "sd" = stats::sd, "R^" = RprobitB::R_hat)
summary(model_train, FUN = FUN)


###################################################
### code chunk number 17: model-train-trace
###################################################
par(mfrow = c(1,2))
plot(model_train, type = "trace")


###################################################
### code chunk number 18: model-train-acf
###################################################
par(mfrow = c(1,2))
plot(model_train, type = "acf", ignore = c("alpha_1", "alpha_2", "alpha_3"))


###################################################
### code chunk number 19: example 1 train new Q
###################################################
model_train <- transform(model_train, Q = 5)


###################################################
### code chunk number 20: example 3 elec relabel covariates
###################################################
data("Electricity", package = "mlogit")
Electricity <- as_cov_names(
  choice_data = Electricity,
  cov = c("pf","cl","loc","wk","tod","seas"),
  alternatives = 1:4
)


###################################################
### code chunk number 21: example 3 elec estimation
###################################################
data_elec <- prepare_data(
  form = choice ~ pf + cl + loc + wk + tod + seas | 0,
  choice_data = Electricity,
  re = c("cl","loc","wk","tod","seas")
)
model_elec <- fit_model(data_elec, R = 1000, scale = pf ~ -1)


###################################################
### code chunk number 22: example 3 elec coef
###################################################
coef(model_elec)


###################################################
### code chunk number 23: example 3 elec share mixing distribution
###################################################
cl_mu <- coef(model_elec)["cl","mean"]
cl_sd <- sqrt(coef(model_elec)["cl","var"])
pnorm(cl_mu / cl_sd)


###################################################
### code chunk number 24: example 3 elec correlation
###################################################
round(cov_mix(model_elec, cor = TRUE), 2)


###################################################
### code chunk number 25: example 2 sim model fit C fixed
###################################################
model_sim <- fit_model(
  data = data_sim, R = 1000, latent_classes = list("C" = 2), seed = 1
)
summary(model_sim)


###################################################
### code chunk number 26: example 2 sim model fit wb update
###################################################
model_sim <- fit_model(
  data = data_sim, R = 1000, seed = 1,
  latent_classes = list("C" = 10, "weight_update" = TRUE, "buffer" = 5),
)


###################################################
### code chunk number 27: model-sim-class-seq
###################################################
plot(model_sim, type = "class_seq")


###################################################
### code chunk number 28: example 4 berserk fit model (eval = FALSE)
###################################################
## choice_berserk <- create_lagged_cov(
##   choice_data = RprobitB::choice_berserk,
##   column = c("berserk","lost"), k = 1, id = "player_id"
## )
## data <- prepare_data(
##   form = berserk ~ 0 | white + rating + rating_diff + min_rem + streak + berserk.1 + lost.1 + 1,
##   re = c("rating_diff","lost.1"), choice_data = choice_berserk, id = "player_id", idc = "game_id",
##   standardize = c("rating","rating_diff","min_rem"), impute = "zero"
## )
## model_berserk <- fit_model(
##   data, latent_classes = list("dp_update" = TRUE, "C" = 10), R = 5000
## )


###################################################
### code chunk number 29: example 4 berserk access pre-computed model
###################################################
data(model_berserk, package = "RprobitB")
coef(model_berserk)


###################################################
### code chunk number 30: model-berserk-mixture
###################################################
plot(model_berserk, type = "mixture")


###################################################
### code chunk number 31: example 4 berserk classification
###################################################
head(preference_classification(model_berserk), n = 5)


###################################################
### code chunk number 32: example 1 train predict confusion matrix
###################################################
predict(model_train)


###################################################
### code chunk number 33: example 1 train predict individual level
###################################################
pred <- predict(model_train, overview = FALSE)
head(pred, n = 5)


###################################################
### code chunk number 34: roc-example
###################################################
library(plotROC)
ggplot(data = pred, aes(m = A, d = ifelse(true == "A", 1, 0))) +
  geom_roc(n.cuts = 20, labels = FALSE) +
  style_roc(theme = theme_grey)


###################################################
### code chunk number 35: predict-model-train-given-covs-1
###################################################
predict(
  model_train,
  data = data.frame("price_A" = c(100,110),
                    "price_B" = c(100,100)),
  overview = FALSE)


###################################################
### code chunk number 36: predict-model-train-given-covs-2
###################################################
predict(
  model_train,
  data = data.frame("price_A" = c(100,110), "comfort_A" = c(1,0),
                    "price_B" = c(100,100), "comfort_B" = c(1,1)),
  overview = FALSE)


###################################################
### code chunk number 37: example 1 train nested model
###################################################
model_train_sparse <- nested_model(model_train, form = choice ~ price | 0)


###################################################
### code chunk number 38: example 1 train model selection
###################################################
model_train <- compute_p_si(model_train)
model_train_sparse <- compute_p_si(model_train_sparse)
model_selection(
  model_train, model_train_sparse,
  criteria = c("npar", "LL", "AIC", "BIC", "WAIC", "MMLL", "BF", "pred_acc")
)


