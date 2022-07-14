### R code from vignette source 'rprobitb_oelschlaeger_bauer.Rnw'

### Set this flag to 'TRUE' to refit everything, or to 'FALSE' to re-use
### already available results of long computations.
refit <- FALSE


###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library("RprobitB")


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
### code chunk number 9: example 2 sim meta
###################################################
N <- 200
T <- 30
alternatives <- c("alt1","alt2")
form <- choice ~ var1 | var2 | var3
re <- c("var2","ASC")


###################################################
### code chunk number 10: example 2 sim overview effects
###################################################
overview_effects(form = form, re = re, alternatives = alternatives)


###################################################
### code chunk number 11: example 2 sim simulation
###################################################
true_parameter <- list(
  alpha = c(-2,0,1), C = 3, s = c(0.5,0.3,0.2), Sigma = 1,
  b = matrix(c(-2,1,0,2,2,-1), ncol = 3),
  Omega = matrix(c(0.3,0.7,0.7,1.9,1.3,-0.2,-0.2,0.9,0.6,-0.9,-0.9,2.4),
                 ncol = 3)
)
data_sim <- simulate_choices(
  form = form, N = N, T = T, J = 2, re = re, alternatives = alternatives,
  seed = 1, true_parameter = true_parameter
)


###################################################
### code chunk number 12: sim-data
###################################################
plot(data_sim, by_choice = TRUE)


###################################################
### code chunk number 13: example 1 train fit
###################################################
model_train <- fit_model(data = data_train, scale = "price := -1")


###################################################
### code chunk number 14: coef-model-train
###################################################
plot(coef(model_train), sd = 3)


###################################################
### code chunk number 15: example 1 train summary
###################################################
summary(model_train, FUN = c("mean" = mean, "sd" = stats::sd, "R^" = R_hat))


###################################################
### code chunk number 16: model-train-trace
###################################################
par(mfrow = c(1,2))
plot(model_train, type = "trace")


###################################################
### code chunk number 17: model-train-acf
###################################################
par(mfrow = c(1,2))
plot(model_train, type = "acf", ignore = c("alpha_1", "alpha_2", "alpha_3"))


###################################################
### code chunk number 18: example 1 train new Q
###################################################
model_train <- transform(model_train, Q = 5)


###################################################
### code chunk number 19: example 3 elec relabel covariates
###################################################
data("Electricity", package = "mlogit")
Electricity <- as_cov_names(
  choice_data = Electricity, cov = c("pf","cl","loc","wk","tod","seas"),
  alternatives = 1:4
)


###################################################
### code chunk number 20: example 3 elec estimation
###################################################
data_elec <- prepare_data(
  form = choice ~ pf + cl + loc + wk + tod + seas | 0,
  choice_data = Electricity,
  re = c("cl","loc","wk","tod","seas")
)
model_elec <- fit_model(data_elec, scale = "pf := -1")


###################################################
### code chunk number 21: example 3 elec coef
###################################################
coef(model_elec)


###################################################
### code chunk number 22: example 3 elec share mixing distribution
###################################################
cl_mu <- coef(model_elec)["cl","mean"]
cl_sd <- sqrt(coef(model_elec)["cl","var"])
pnorm(cl_mu / cl_sd)


###################################################
### code chunk number 23: example 3 elec correlation
###################################################
round(cov_mix(model_elec, cor = TRUE), 2)


###################################################
### code chunk number 24: example 2 sim model fit C fixed
###################################################
model_sim <- fit_model(
  data = data_sim, R = 1000, latent_classes = list("C" = 3), seed = 1
)
summary(model_sim)


###################################################
### code chunk number 25: example 2 sim model fit wb update
###################################################
model_sim <- fit_model(
  data = data_sim, seed = 1,
  latent_classes = list("C" = 10, "weight_update" = TRUE, "buffer" = 5)
)


###################################################
### code chunk number 26: model-sim-class-seq
###################################################
plot(model_sim, type = "class_seq")


###################################################
### code chunk number 27: example 4 berserk create covariates
###################################################
if(refit) {
  choice_berserk <- create_lagged_cov(
     choice_data = choice_berserk, column = c("berserk","lost"),
     id = "player_id"
  )
}


###################################################
### code chunk number 28: example 4 berserk prepare data
###################################################
if(refit) {
  data <- prepare_data(
    form = berserk ~ 0 | white + rating + rating_diff + min_rem + streak +
      berserk.1 + lost.1 + 1,
    re = c("rating_diff","lost.1"), choice_data = choice_berserk,
    id = "player_id", idc = "game_id",
    standardize = c("rating","rating_diff","min_rem"), impute = "zero"
  )
}


###################################################
### code chunk number 29: example 4 berserk fit model
###################################################
if(refit) {
  model_berserk <- fit_model(
    data, latent_classes = list("dp_update" = TRUE, "C" = 10), R = 5000
  )
}


###################################################
### code chunk number 30: example 4 berserk access pre-computed model
###################################################
if(!refit) {
  data("model_berserk", package = "RprobitB")
  coef(model_berserk)
}


###################################################
### code chunk number 31: example 4 berserk estimated class sizes
###################################################
est <- point_estimates(model_berserk)
round(est[["s"]], 2)


###################################################
### code chunk number 32: model-berserk-mixture
###################################################
plot(model_berserk, type = "mixture")


###################################################
### code chunk number 33: example 4 berserk classification
###################################################
player <- c("zhigalko_sergei", "serg_01")
classification(model_berserk)[player,]


###################################################
### code chunk number 34: example 1 train predict confusion matrix
###################################################
predict(model_train)


###################################################
### code chunk number 35: example 1 train predict individual level
###################################################
pred <- predict(model_train, overview = FALSE)
head(pred, n = 5)


###################################################
### code chunk number 36: roc-example
###################################################
library("mlogit")
Train$choiceid <- 1:nrow(Train)
Tr <- dfidx(
  Train, choice = "choice", varying = 4:11, sep = "_",
  opposite = c("price", "comfort", "time", "change"),
  idx = list(c("choiceid", "id")), idnames = c("chid", "alt")
)
Tr$price <- Tr$price / 100 * 2.20371
Tr$time <- Tr$time / 60
form <- choice ~ price + time + change + comfort | - 1
model_train_mlogit <- mlogit(form, Tr)$probabilities
plot_roc(model_train, model_train_mlogit)


###################################################
### code chunk number 37: predict-model-train-given-covs-1
###################################################
predict(model_train, overview = FALSE,
        data = data.frame("price_A" = c(100,110), "price_B" = c(100,100)))


###################################################
### code chunk number 38: predict-model-train-given-covs-2
###################################################
predict(model_train, overview = FALSE,
        data = data.frame("price_A" = c(100,110), "comfort_A" = c(1,0),
                          "price_B" = c(100,100), "comfort_B" = c(1,1)))


###################################################
### code chunk number 39: example 1 train nested model
###################################################
model_train_sparse <- update(model_train, form = choice ~ price | 0)


###################################################
### code chunk number 40: example 1 train model selection
###################################################
model_train <- compute_p_si(model_train)
model_train_sparse <- compute_p_si(model_train_sparse)
model_selection(
  model_train, model_train_sparse,
  criteria = c("npar", "LL", "AIC", "BIC", "WAIC", "MMLL", "BF", "pred_acc")
)


