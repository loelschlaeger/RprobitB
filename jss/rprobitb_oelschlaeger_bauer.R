### R code from vignette source 'rprobitb_oelschlaeger_bauer.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library(RprobitB)


###################################################
### code chunk number 2: prepare-data-call (eval = FALSE)
###################################################
## data <- prepare_data(form = form, choice_data = choice_data)


###################################################
### code chunk number 3: overview-Train-data
###################################################
data("Train", package = "mlogit")
str(Train)


###################################################
### code chunk number 4: Train-formula
###################################################
form <- choice ~ price + time + comfort + change | 0


###################################################
### code chunk number 5: Train-re
###################################################
re <- c("price","time")


###################################################
### code chunk number 6: check-form
###################################################
check_form(form = form, re = re)


###################################################
### code chunk number 7: prepare-Train-data
###################################################
data <- prepare_data(form = form, choice_data = Train, re = re, id = "id", idc = "choiceid")


###################################################
### code chunk number 8: summary-plot-Train-data
###################################################
summary(data)
plot(data)


###################################################
### code chunk number 9: simulate-choices-call (eval = FALSE)
###################################################
## data <- simulate_choices(form = form, N = N, T = T, J = J)


###################################################
### code chunk number 10: data-sim-meta
###################################################
N <- 100
T <- 10
alternatives <- c("A", "B")
form <- choice ~ var1 | var2 | var3
re <- c("ASC", "var2")


###################################################
### code chunk number 11: data-sim-overview
###################################################
overview_effects(form = form, re = re, alternatives = alternatives)


###################################################
### code chunk number 12: data-sim
###################################################
data <- simulate_choices(
  form = form,
  N = N,
  T = T,
  J = 2,
  re = re,
  alternatives = alternatives,
  seed = 1,
  alpha = c(-1,0,1),
  b = matrix(c(2,-0.5), ncol = 1)
)
summary(data)


###################################################
### code chunk number 13: data-sim-plot-by-choice
###################################################
plot(data, by_choice = TRUE)


###################################################
### code chunk number 14: data-split-deciders
###################################################
train_test(data, test_proportion = 0.3, by = "N")


###################################################
### code chunk number 15: data-split-occasions
###################################################
train_test(data, test_number = 2, by = "T", random = TRUE, seed = 1)


###################################################
### code chunk number 16: mcmc-call (eval = FALSE)
###################################################
## mcmc(data = data)


###################################################
### code chunk number 17: transform-Train
###################################################
data("Train", package = "mlogit")
Train$price_A <- Train$price_A / 100 * 2.20371
Train$price_B <- Train$price_B / 100 * 2.20371
Train$time_A <- Train$time_A / 60
Train$time_B <- Train$time_B / 60


###################################################
### code chunk number 18: Train-fit (eval = FALSE)
###################################################
## form <- choice ~ price + time + change + comfort | 0
## data <- prepare_data(form = form, choice_data = Train)
## model_train <- mcmc(
##   data = data,
##   scale = list("parameter" = "a", index = 1, value = -1)
## )


###################################################
### code chunk number 19: load-model-Train
###################################################
data(model_train, package = "RprobitB")


###################################################
### code chunk number 20: coef-model-Train
###################################################
coef(model_train)


###################################################
### code chunk number 21: plot-coef-model-train
###################################################
plot(coef(model_train), sd = 3)


###################################################
### code chunk number 22: str-gibbs-samples
###################################################
str(model_train$gibbs_samples, max.level = 2, give.attr = FALSE)


###################################################
### code chunk number 23: summary-model-train
###################################################
summary(model_train,
        FUN = c("mean"        = mean,
                "sd"          = stats::sd,
                "R^"          = R_hat,
                "custom_stat" = function(x) abs(mean(x) - median(x))
                )
       )


###################################################
### code chunk number 24: plot-trace-model-train
###################################################
par(mfrow = c(2,1))
plot(model_train, type = "trace")


###################################################
### code chunk number 25: plot-acf-model-train
###################################################
par(mfrow = c(2,3))
plot(model_train, type = "acf")


###################################################
### code chunk number 26: transform-model-train-B
###################################################
model_train <- transform(model_train, B = 1)


###################################################
### code chunk number 27: transform-model-train-Q
###################################################
model_train <- transform(model_train, Q = 100)


###################################################
### code chunk number 28: transform-model-train-scale
###################################################
model_train <- transform(model_train, scale = list(parameter = "s", index = 1, value = 1))


###################################################
### code chunk number 29: estimate-elec-model (eval = FALSE)
###################################################
## data("Electricity", package = "mlogit")
## Electricity <- as_cov_names(Electricity, c("pf","cl","loc","wk","tod","seas"), 1:4)
## data <- prepare_data(
##   form = choice ~ pf + cl + loc + wk + tod + seas | 0,
##   choice_data = Electricity,
##   re = c("cl","loc","wk","tod","seas")
##   )
## model_elec <- mcmc(data, R = 1000, scale = list(parameter = "a", index = 1, value = -1))


###################################################
### code chunk number 30: load-model-ele
###################################################
data(model_elec, package = "RprobitB")


###################################################
### code chunk number 31: coef-model-elec
###################################################
coef(model_elec)


###################################################
### code chunk number 32: compute-mixdistr-share
###################################################
cl_mu <- coef(model_elec)["cl","mean"]
cl_sd <- sqrt(coef(model_elec)["cl","var"])
pnorm(cl_mu / cl_sd)


###################################################
### code chunk number 33: cov-mixdistr
###################################################
cov_mix(model_elec, cor = TRUE)


###################################################
### code chunk number 34: plot-mixture-model-elec
###################################################
plot(model_elec, type = "mixture")


###################################################
### code chunk number 35: sim-dirichlet
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
### code chunk number 36: dirichlet-prior
###################################################
delta <- 0.1
xi <- numeric(P_r)
D <- diag(P_r)
nu <- P_r + 2
Theta <- diag(P_r)


###################################################
### code chunk number 37: dirichlet-inits
###################################################
z <- rep(1, ncol(beta))
C <- 1
b <- matrix(0, nrow = P_r, ncol = C)
Omega <- matrix(rep(diag(P_r), C), nrow = P_r*P_r, ncol = C)


###################################################
### code chunk number 38: dirichlet-process-app
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
### code chunk number 39: dirichlet-example-plot
###################################################
par(mfrow = c(1,2))
plot(t(beta), xlab = bquote(beta[1]), ylab = bquote(beta[2]), pch = 19)
RprobitB:::plot_class_allocation(beta, z, b, Omega, r = 100, perc = 0.95)


###################################################
### code chunk number 40: load-model-train
###################################################
data("model_train", package = "RprobitB")


###################################################
### code chunk number 41: predict-model-train
###################################################
predict(model_train)


###################################################
### code chunk number 42: predict-model-train-indlevel
###################################################
pred <- predict(model_train, overview = FALSE)
head(pred, n = 10)


###################################################
### code chunk number 43: model-train-covs
###################################################
get_cov(model_train, id = 1, idc = 8)


###################################################
### code chunk number 44: model-train-coeffs
###################################################
coef(model_train)


###################################################
### code chunk number 45: model-train-Sigma
###################################################
point_estimates(model_train)$Sigma


###################################################
### code chunk number 46: roc-example
###################################################
library(plotROC)
ggplot(data = pred, aes(m = A, d = ifelse(true == "A", 1, 0))) +
  geom_roc(n.cuts = 20, labels = FALSE) +
  style_roc(theme = theme_grey)


###################################################
### code chunk number 47: predict-model-train-given-covs-1
###################################################
predict(
  model_train,
  data = data.frame("price_A" = c(100,110),
                    "price_B" = c(100,100)),
  overview = FALSE)


###################################################
### code chunk number 48: predict-model-train-given-covs-2
###################################################
predict(
  model_train,
  data = data.frame("price_A"   = c(100,110),
                    "comfort_A" = c(1,0),
                    "price_B"   = c(100,100),
                    "comfort_B" = c(1,1)),
  overview = FALSE)


###################################################
### code chunk number 49: load-model-train
###################################################
data("model_train", package = "RprobitB")
model_train


###################################################
### code chunk number 50: nested-model-train (eval = FALSE)
###################################################
## model_train_sparse <- nested_model(model_train, form = choice ~ price | 0)


###################################################
### code chunk number 51: load-train-sparse-model
###################################################
data("model_train_sparse", package = "RprobitB")


###################################################
### code chunk number 52: model-selection-example
###################################################
model_selection(model_train, model_train_sparse,
                criteria = c("npar", "LL", "AIC", "BIC", "WAIC", "MMLL", "BF", "pred_acc"))


###################################################
### code chunk number 53: npar-example
###################################################
npar(model_train, model_train_sparse)


###################################################
### code chunk number 54: oglik-example
###################################################
logLik(model_train)
logLik(model_train_sparse)


###################################################
### code chunk number 55: aic-example
###################################################
AIC(model_train, model_train_sparse, k = 2)


###################################################
### code chunk number 56: bic-example
###################################################
BIC(model_train, model_train_sparse)


###################################################
### code chunk number 57: compute-psi-train (eval = FALSE)
###################################################
## model_train <- compute_p_si(model_train)


###################################################
### code chunk number 58: waic-example
###################################################
WAIC(model_train)
WAIC(model_train_sparse)


###################################################
### code chunk number 59: waictrace
###################################################
plot(WAIC(model_train))
plot(WAIC(model_train_sparse))


###################################################
### code chunk number 60: mml_train
###################################################
model_train <- mml(model_train)
model_train$mml
attr(model_train$mml, "mmll")


###################################################
### code chunk number 61: mmltrace
###################################################
plot(model_train$mml, log = TRUE)


###################################################
### code chunk number 62: arithmetic_mean_estimator (eval = FALSE)
###################################################
## model_train <- mml(model_train, S = 1000, recompute = TRUE)


###################################################
### code chunk number 63: bayes-factor-example
###################################################
model_selection(model_train, model_train_sparse, criteria = c("BF"))


###################################################
### code chunk number 64: pred-acc-example
###################################################
pred_acc(model_train, model_train_sparse)


