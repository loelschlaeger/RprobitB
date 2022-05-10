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
  form = form,
  choice_data = Train,
  id = "id",
  idc = "choiceid"
  )
summary(data_train)


###################################################
### code chunk number 6: train-data
###################################################
plot(data_train)


###################################################
### code chunk number 7: simulate choices call (eval = FALSE)
###################################################
## data <- simulate_choices(form = form, N = N, T = T, J = J)


###################################################
### code chunk number 8: example 2 sim meta
###################################################
N <- 100
T <- 30
alternatives <- c("alt1", "alt2")
base_alternative <- "alt2"
form <- choice ~ var1 | var2 | var3
re <- c("ASC","var2")


###################################################
### code chunk number 9: example 2 sim overview effects
###################################################
overview_effects(
  form = form,
  re = re,
  alternatives = alternatives,
  base_alternative = base_alternative
)


###################################################
### code chunk number 10: example 2 sim simulation
###################################################
data_sim <- simulate_choices(
  form = form,
  N = N,
  T = T,
  J = 2,
  re = re,
  alternatives = alternatives,
  base_alternative = base_alternative,
  seed = 1,
  alpha = c(-1,0,1),
  C = 2,
  s = c(0.7,0.3),
  b = matrix(c(2,-0.5,1,1), ncol = 2),
  Sigma = 1
)


###################################################
### code chunk number 11: sim-data
###################################################
plot(data_sim, by_choice = TRUE)


###################################################
### code chunk number 12: data-split-deciders
###################################################
train_test(data_sim, test_proportion = 0.3, by = "N")


###################################################
### code chunk number 13: data-split-occasions
###################################################
train_test(data_sim, test_number = 2, by = "T", random = TRUE, seed = 1)


###################################################
### code chunk number 14: fit_model call (eval = FALSE)
###################################################
## fit_model(data = data)


###################################################
### code chunk number 15: example 1 train fit (eval = FALSE)
###################################################
## model_train <- fit_model(data = data_train, scale = price ~ -1)


###################################################
### code chunk number 16: example 1 train load model
###################################################
data(model_train, package = "RprobitB")


###################################################
### code chunk number 17: example 1 train coef
###################################################
coef(model_train)


###################################################
### code chunk number 18: coef-model-train
###################################################
plot(coef(model_train), sd = 3)


###################################################
### code chunk number 19: example 1 train gibbs samples
###################################################
str(model_train$gibbs_samples, max.level = 2, give.attr = FALSE)


###################################################
### code chunk number 20: example 1 train summary
###################################################
summary(model_train,
        FUN = c("mean" = mean, "sd" = stats::sd, "R^" = RprobitB::R_hat,
                "custom_stat" = function(x) abs(mean(x) - median(x))))


###################################################
### code chunk number 21: model-train-trace
###################################################
par(mfrow = c(1,2))
plot(model_train, type = "trace")


###################################################
### code chunk number 22: model-train-acf
###################################################
par(mfrow = c(1,2))
plot(model_train, type = "acf", ignore = c("alpha_1", "alpha_2", "alpha_3"))


###################################################
### code chunk number 23: example 3 elec relabel covariates
###################################################
data("Electricity", package = "mlogit")
Electricity <- as_cov_names(
  choice_data = Electricity,
  cov = c("pf","cl","loc","wk","tod","seas"),
  alternatives = 1:4
)


###################################################
### code chunk number 24: example 3 elec estimation (eval = FALSE)
###################################################
## data_elec <- prepare_data(
##   form = choice ~ pf + cl + loc + wk + tod + seas | 0,
##   choice_data = Electricity,
##   re = c("cl","loc","wk","tod","seas")
## )
## model_elec <- fit_model(data_elec, R = 5000, scale = pf ~ -1)


###################################################
### code chunk number 25: example 3 elec load model
###################################################
data(model_elec, package = "RprobitB")


###################################################
### code chunk number 26: example 3 elec coef
###################################################
coef(model_elec)


###################################################
### code chunk number 27: example 3 elec share mixing distribution
###################################################
cl_mu <- coef(model_elec)["cl","mean"]
cl_sd <- sqrt(coef(model_elec)["cl","var"])
pnorm(cl_mu / cl_sd)


###################################################
### code chunk number 28: example 3 elec correlation
###################################################
round(cov_mix(model_elec, cor = TRUE), 2)


###################################################
### code chunk number 29: example 2 sim model fit C fixed
###################################################
model_sim <- fit_model(
  data = data_sim,
  R = 1000,
  latent_classes = list("C" = 2),
  seed = 1
)
summary(model_sim)


###################################################
### code chunk number 30: example 2 sim model fit wb update
###################################################
model_sim <- fit_model(
  data = data_sim,
  R = 1000,
  latent_classes = list(
    "C" = 10,
    "weight_update" = TRUE,
    "buffer" = 5
    ),
  seed = 1
)


###################################################
### code chunk number 31: model-sim-class-seq
###################################################
plot(model_sim, type = "class_seq")


###################################################
### code chunk number 32: simulation dirichlet process
###################################################
set.seed(1)
P_r <- 2
C_true <- 3
N <- c(100,70,30)
(b_true <- matrix(replicate(C_true, rnorm(P_r)), nrow = P_r, ncol = C_true))
(Omega_true <- matrix(replicate(C_true, rwishart(P_r + 1, 0.1*diag(P_r))$W, simplify = TRUE),
                      nrow = P_r*P_r, ncol = C_true))
beta <- c()
for(c in 1:C_true) for(n in 1:N[c])
  beta <- cbind(beta, rmvnorm(mu = b_true[,c,drop=F], Sigma = matrix(Omega_true[,c,drop=F], ncol = P_r)))
z_true <- rep(1:3, times = N)


###################################################
### code chunk number 33: dirichlet process prior parameters
###################################################
delta <- 0.1
xi <- numeric(P_r)
D <- diag(P_r)
nu <- P_r + 2
Theta <- diag(P_r)


###################################################
### code chunk number 34: dirichlet process initial values
###################################################
z <- rep(1, ncol(beta))
C <- 1
b <- matrix(0, nrow = P_r, ncol = C)
Omega <- matrix(rep(diag(P_r), C), nrow = P_r*P_r, ncol = C)


###################################################
### code chunk number 35: dirichlet process application
###################################################
for(r in 1:100){
  dp <- RprobitB:::update_classes_dp(
    Cmax = 10, beta, z, b, Omega, delta, xi, D, nu, Theta, s_desc = TRUE
    )
  z <- dp$z
  b <- dp$b
  Omega <- dp$Omega
}


###################################################
### code chunk number 36: dp-example
###################################################
par(mfrow = c(1,2))
plot(t(beta), xlab = bquote(beta[1]), ylab = bquote(beta[2]), pch = 19)
RprobitB:::plot_class_allocation(beta, z, b, Omega, r = 100, perc = 0.95)


###################################################
### code chunk number 37: example 4 berserk load model
###################################################
data(model_berserk, package = "RprobitB")


###################################################
### code chunk number 38: model-berserk-mixture
###################################################
plot(model_berserk, type = "mixture")


###################################################
### code chunk number 39: example 1 train predict confusion matrix
###################################################
predict(model_train)


###################################################
### code chunk number 40: example 1 train predict individual level
###################################################
pred <- predict(model_train, overview = FALSE)
head(pred, n = 5)


###################################################
### code chunk number 41: roc-example
###################################################
library(plotROC)
ggplot(data = pred, aes(m = A, d = ifelse(true == "A", 1, 0))) +
  geom_roc(n.cuts = 20, labels = FALSE) +
  style_roc(theme = theme_grey)


###################################################
### code chunk number 42: predict-model-train-given-covs-1
###################################################
predict(
  model_train,
  data = data.frame("price_A" = c(100,110),
                    "price_B" = c(100,100)),
  overview = FALSE)


###################################################
### code chunk number 43: predict-model-train-given-covs-2
###################################################
predict(
  model_train,
  data = data.frame("price_A"   = c(100,110),
                    "comfort_A" = c(1,0),
                    "price_B"   = c(100,100),
                    "comfort_B" = c(1,1)),
  overview = FALSE)


###################################################
### code chunk number 44: load-model-train
###################################################
data("model_train", package = "RprobitB")
model_train


###################################################
### code chunk number 45: nested-model-train (eval = FALSE)
###################################################
## model_train_sparse <- nested_model(model_train, form = choice ~ price | 0)


###################################################
### code chunk number 46: load-train-sparse-model
###################################################
data("model_train_sparse", package = "RprobitB")


###################################################
### code chunk number 47: model-selection-example
###################################################
model_selection(model_train, model_train_sparse,
                criteria = c("npar", "LL", "AIC", "BIC", "WAIC", "MMLL", "BF", "pred_acc"))


###################################################
### code chunk number 48: npar-example
###################################################
npar(model_train, model_train_sparse)


###################################################
### code chunk number 49: oglik-example
###################################################
logLik(model_train)
logLik(model_train_sparse)


###################################################
### code chunk number 50: aic-example
###################################################
AIC(model_train, model_train_sparse, k = 2)


###################################################
### code chunk number 51: bic-example
###################################################
BIC(model_train, model_train_sparse)


###################################################
### code chunk number 52: compute-psi-train (eval = FALSE)
###################################################
## model_train <- compute_p_si(model_train)


###################################################
### code chunk number 53: waic-example (eval = FALSE)
###################################################
## WAIC(model_train)
## WAIC(model_train_sparse)


###################################################
### code chunk number 54: waictrace (eval = FALSE)
###################################################
## plot(WAIC(model_train))
## plot(WAIC(model_train_sparse))


###################################################
### code chunk number 55: mml_train
###################################################
model_train <- mml(model_train)
model_train$mml
attr(model_train$mml, "mmll")


###################################################
### code chunk number 56: mmltrace
###################################################
plot(model_train$mml, log = TRUE)


###################################################
### code chunk number 57: arithmetic_mean_estimator (eval = FALSE)
###################################################
## model_train <- mml(model_train, S = 1000, recompute = TRUE)


###################################################
### code chunk number 58: bayes-factor-example (eval = FALSE)
###################################################
## model_selection(model_train, model_train_sparse, criteria = c("BF"))


###################################################
### code chunk number 59: pred-acc-example
###################################################
pred_acc(model_train, model_train_sparse)


