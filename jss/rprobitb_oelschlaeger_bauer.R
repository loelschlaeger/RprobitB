### R code from vignette source 'rprobitb_oelschlaeger_bauer.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library("RprobitB")
future::plan(future::multisession, workers = 2)
set.seed(1)


###################################################
### code chunk number 2: simulation-dgp
###################################################
common <- c(
  income_car = 0.5, income_train = -0.5, ASC_car = 1, ASC_train = 0.5,
  time_bus = -0.5, time_car = -1, time_train = -1.5
)
dgp_parameters <- list(
  beta = list(
    c(price = -1.75, common), c(price = 0, common),
    c(price = 1.75, common)
  ),
  Omega = list(matrix(0.05), matrix(0.05), matrix(0.05)),
  weights = c(1, 1, 1) / 3, Sigma = diag(c(0.5, 0.5, 1))
)


###################################################
### code chunk number 3: simulation
###################################################
simulation <- fit(
  choice ~ price | income | time, random_effects = "price",
  latent_class_effects = "price", classes = 3,
  alternatives = c("bus", "car", "train"), base = "bus",
  n_deciders = 400, n_occasions = 20, dgp_parameters = dgp_parameters,
  iterations = 40000, warmup = 20000, thin = 20, chains = 2
)


###################################################
### code chunk number 4: simulation-frame
###################################################
head(model.frame(simulation), 3)


###################################################
### code chunk number 5: simulation-summary
###################################################
summary(simulation)


###################################################
### code chunk number 6: train-data
###################################################
data("Train", package = "mlogit")
Train$price_A <- Train$price_A / 100 / 2.20371
Train$price_B <- Train$price_B / 100 / 2.20371
Train$time_A <- Train$time_A / 60
Train$time_B <- Train$time_B / 60


###################################################
### code chunk number 7: train-fit
###################################################
model_train <- fit(
  choice ~ price + time + change + factor(comfort) | 0,
  data = Train, column_decider = "id", column_occasion = "choiceid",
  iterations = 2000, warmup = 1000, chains = 2
)
summary(model_train)


###################################################
### code chunk number 8: train-trace
###################################################
plot(model_train, type = "trace")


###################################################
### code chunk number 9: train-coef
###################################################
coef(model_train)


###################################################
### code chunk number 10: train-confint
###################################################
confint(model_train, level = 0.9)


###################################################
### code chunk number 11: train-compensation
###################################################
train_compensation <- interpret(model_train, reference = "price")
train_compensation


###################################################
### code chunk number 12: train-mea
###################################################
train_mea <- interpret(model_train, type = "mea")
print(as.data.frame(train_mea), digits = 3, row.names = FALSE)


###################################################
### code chunk number 13: fare-curve-figure
###################################################
train_mea_at <- interpret(
  model_train, type = "mea", at = c(price_A = 20, price_B = 10)
)
average_occasion <- with(
  as.data.frame(train_mea),
  setNames(at, paste(covariate, alternative, sep = "_"))
)
fare_difference <- seq(-15, 15, by = 0.5)
fare_grid <- data.frame(
  id = seq_along(fare_difference), choiceid = 1,
  price_A = average_occasion[["price_A"]],
  price_B = average_occasion[["price_A"]] + fare_difference,
  time_A = average_occasion[["time_A"]],
  time_B = average_occasion[["time_B"]],
  change_A = average_occasion[["change_A"]],
  change_B = average_occasion[["change_B"]],
  comfort_A = 1, comfort_B = 1
)
fare_curve <- predict(model_train, newdata = fare_grid)
fare_points <- c(
  average_occasion[["price_B"]] - average_occasion[["price_A"]], -10
)
fare_slopes <- c(
  subset(train_mea, covariate == "price" & alternative == "B")$mean,
  subset(train_mea_at, covariate == "price" & alternative == "B")$mean
)
fare_probability <- approx(
  fare_difference, fare_curve$probability_B, xout = fare_points
)$y
op <- par(mar = c(4, 4, 0.5, 0.5))
plot(
  fare_difference, fare_curve$probability_B, type = "l", lwd = 3,
  col = "gray60", ylim = c(0, 1), yaxt = "n",
  xlab = "price of trip B minus price of trip A in euro",
  ylab = "probability of trip B"
)
axis(2, at = c(0, 0.5, 1))
segments(
  fare_points - 4, fare_probability - 4 * fare_slopes,
  fare_points + 4, fare_probability + 4 * fare_slopes, lty = 2, lwd = 2
)
points(fare_points, fare_probability, pch = 19)
par(op)


###################################################
### code chunk number 14: train-ame
###################################################
train_ame <- interpret(model_train, type = "ame")
print(as.data.frame(train_ame), digits = 3, row.names = FALSE)


###################################################
### code chunk number 15: electricity-data
###################################################
data("Electricity", package = "mlogit")
names(Electricity) <- sub("([1-4])$", "_\\1", names(Electricity))
Electricity$occasion <- with(Electricity, ave(id, id, FUN = seq_along))


###################################################
### code chunk number 16: electricity-fit
###################################################
electricity <- fit(
  choice ~ pf + cl + loc + wk + tod + seas | 0, data = Electricity,
  random_effects = c("cl", "loc"), column_decider = "id",
  column_occasion = "occasion", scale = c(pf = -1),
  iterations = 25000, warmup = 10000, thin = 20, chains = 2,
  save_individual_draws = TRUE
)
summary(electricity)


###################################################
### code chunk number 17: household-coefficients
###################################################
household_coefficients <- coef(electricity, level = "individual")
head(household_coefficients, 5)


###################################################
### code chunk number 18: household-deciles
###################################################
quantile(household_coefficients[, "loc"], probs = c(0.1, 0.9))


###################################################
### code chunk number 19: price-normal
###################################################
price_normal <- update(
  model_train, random_effects = c(price = "n"),
  iterations = 6000, warmup = 3000, thin = 30
)


###################################################
### code chunk number 20: price-lognormal
###################################################
price_lognormal <- update(
  model_train, random_effects = c(price = "ln-"),
  iterations = 6000, warmup = 3000, thin = 30
)


###################################################
### code chunk number 21: price-mixing-figure
###################################################
population_density <- function(x, draws, effect, type = "n") {
  b <- draws[, sprintf("mu[%s]", effect)]
  s <- sqrt(draws[, sprintf("Omega[%s,%s]", effect, effect)])
  density <- switch(type,
    "n" = function(d) dnorm(x, b[d], s[d]),
    "ln-" = function(d) dlnorm(-x, b[d], s[d])
  )
  rowMeans(sapply(seq_along(b), density))
}
price_grid <- seq(-3, 1, length.out = 400)
price_normal_density <- population_density(
  price_grid, posterior::as_draws_matrix(price_normal$draws), "price"
)
price_lognormal_density <- population_density(
  price_grid, posterior::as_draws_matrix(price_lognormal$draws), "price",
  type = "ln-"
)
op <- par(mar = c(4, 4, 0.5, 0.5))
plot(
  price_grid, price_normal_density, type = "l", lwd = 2,
  ylim = c(0, max(price_normal_density, price_lognormal_density)),
  xlab = "price coefficient",
  ylab = "density"
)
lines(price_grid, price_lognormal_density, lwd = 2, lty = 2)
abline(v = 0, lty = 3)
legend(
  "topleft", legend = c("normal", "negative log-normal"),
  lwd = 2, lty = c(1, 2), bty = "n"
)
par(op)


###################################################
### code chunk number 22: mixed-compensation
###################################################
models <- mget(c("model_train", "price_normal", "price_lognormal"))
time_compensation <- sapply(models, function(m) {
  time <- interpret(m, reference = "price", effects = "time")
  unlist(as.data.frame(time)[-1])
})
print(time_compensation, digits = 3)


###################################################
### code chunk number 23: electricity-classes
###################################################
electricity_classes <- update(
  electricity, latent_class_effects = "loc", classes = 2
)
summary(electricity_classes)


###################################################
### code chunk number 24: electricity-classes-figure
###################################################
classes_draws <- posterior::as_draws_matrix(electricity_classes$draws)
class_grid <- seq(-4, 12, length.out = 400)
class_density <- sapply(1:2, function(k) {
  weight <- classes_draws[, sprintf("weight[%d]", k)]
  mean_k <- classes_draws[, sprintf("mu[loc,%d]", k)]
  sd_k <- sqrt(classes_draws[, sprintf("Omega[loc,loc,%d]", k)])
  rowMeans(sapply(seq_along(weight), function(d) {
    weight[d] * dnorm(class_grid, mean_k[d], sd_k[d])
  }))
})
op <- par(mar = c(4, 4, 0.5, 0.5))
hist(
  coef(electricity_classes, level = "individual")[, "loc"], breaks = 30,
  freq = FALSE, col = "gray85", border = "white", main = "",
  xlim = range(class_grid), ylab = "density",
  xlab = "valuation of a local supplier in cents per kWh"
)
lines(class_grid, rowSums(class_density), lwd = 2)
lines(class_grid, class_density[, 1], lty = 2)
lines(class_grid, class_density[, 2], lty = 3)
par(op)


###################################################
### code chunk number 25: dirichlet
###################################################
dynamic <- update(
  electricity_classes, class_update = "dirichlet_process"
)
summary(dynamic, variables = c(
  "weight[1]", "weight[2]", "weight[3]", "mu[loc,1]", "mu[loc,2]",
  "mu[loc,3]", "class_concentration", "n_classes"
))


###################################################
### code chunk number 26: diagnostics
###################################################
dynamic_diagnostics <- latent_class_diagnostics(dynamic)
subset(dynamic_diagnostics$occupancy, probability > 0)
head(dynamic_diagnostics$membership[, 1:3], 3)


###################################################
### code chunk number 27: sparse
###################################################
sparse <- update(
  electricity_classes, class_update = "sparse", classes = 6
)
subset(latent_class_diagnostics(sparse)$occupancy, probability > 0)


###################################################
### code chunk number 28: heuristic
###################################################
heuristic_one <- update(
  simulation, class_update = "weight_based", classes = 1
)
heuristic_ten <- update(
  simulation, class_update = "weight_based", classes = 10
)


###################################################
### code chunk number 29: heuristic-classes
###################################################
heuristic_runs <- list(one = heuristic_one, ten = heuristic_ten)
iterations <- 1:1000
op <- par(mfrow = c(1, 2), mar = c(4, 4, 1.5, 0.5))
for (run in c("one", "ten")) {
  classes <- heuristic_runs[[run]]$model$latent_classes
  history <- classes$history[iterations, ]
  changes <- classes$changes[iterations, ]
  plot(
    iterations, history[, 1], type = "s", ylim = c(1, 10),
    xlab = "iteration", ylab = "number of classes",
    main = paste("start with", run, if (run == "one") "class" else "classes")
  )
  lines(iterations, history[, 2], type = "s", lty = 2)
  for (chain in 1:2) {
    at <- which(changes[, chain] > 0)
    points(
      at, history[at, chain], pch = c(25, 24, 21)[changes[at, chain]],
      bg = "white"
    )
  }
  if (run == "one") {
    legend(
      "topright", legend = c("removal", "split", "merge"),
      pch = c(25, 24, 21), pt.bg = "white", bty = "n"
    )
  } else {
    legend(
      "topright", legend = c("chain 1", "chain 2"), lty = 1:2, bty = "n"
    )
  }
}
par(op)


###################################################
### code chunk number 30: heuristic-summary
###################################################
summary(heuristic_ten, variables = c(
  "weight[1]", "weight[2]", "weight[3]", "mu[price,1]", "mu[price,2]",
  "mu[price,3]", "n_classes"
))


###################################################
### code chunk number 31: ordered-fit
###################################################
data("Health", package = "Rchoice")
Health$age <- Health$age - mean(Health$age)
Health$income <- Health$hhinc / 1000 - mean(Health$hhinc / 1000)
Health$occasion <- with(Health, ave(id, id, FUN = seq_along))
satisfaction <- fit(
  newhsat ~ age + income + handper + married, data = Health,
  alternatives = as.character(0:4), choice_type = "ordered",
  random_effects = "ASC", column_decider = "id",
  column_occasion = "occasion",
  iterations = 20000, warmup = 10000, thin = 10, chains = 2
)
summary(satisfaction)


###################################################
### code chunk number 32: ordered-thresholds
###################################################
satisfaction_coef <- coef(satisfaction)
intercept_variance <- satisfaction_coef[["Omega[ASC,ASC]"]]
utility_young <- satisfaction_coef[["mu[ASC]"]] +
  min(Health$age) * satisfaction_coef[["beta[age]"]] +
  satisfaction_coef[["beta[married]"]]
utility_old <- utility_young + (64 - 25) * satisfaction_coef[["beta[age]"]]
utility_sd <- sqrt(1 + intercept_variance)
thresholds <- c(0, satisfaction_coef[c("gamma[2]", "gamma[3]", "gamma[4]")])
utility_grid <- seq(-2, 8, length.out = 400)
op <- par(mar = c(4, 4, 1.5, 0.5))
plot(
  utility_grid, dnorm(utility_grid, utility_young, utility_sd),
  type = "l", lwd = 2, xlab = "latent utility", ylab = "density"
)
lines(
  utility_grid, dnorm(utility_grid, utility_old, utility_sd),
  lwd = 2, lty = 2
)
abline(v = thresholds, lty = 3)
mtext(
  0:4, side = 3, line = 0.2,
  at = c(-1, (thresholds[-1] + thresholds[-4]) / 2, thresholds[4] + 1)
)
legend(
  "topleft", legend = c("25 years", "64 years"),
  lwd = 2, lty = 1:2, bty = "n"
)
par(op)


###################################################
### code chunk number 33: ranked-fit
###################################################
data("Game", package = "mlogit")
rank_columns <- grep("^ch\\.", names(Game), value = TRUE)
Game$first <- sub("ch.", "", rank_columns, fixed = TRUE)[
  apply(Game[rank_columns], 1, which.min)
]
gaming_rank <- fit(
  ch ~ own | hours + age, data = Game, choice_type = "ranked",
  delimiter = ".", column_decider = NULL,
  iterations = 40000, warmup = 20000, thin = 20, chains = 2
)


###################################################
### code chunk number 34: ranked-interval
###################################################
plot(
  gaming_rank, type = "interval", prob_outer = 0.95,
  variables = grep(
    "own|hours|age", names(coef(gaming_rank)), value = TRUE
  )
)


###################################################
### code chunk number 35: ranked-first
###################################################
gaming_first <- update(
  gaming_rank, first ~ own | hours + age, choice_type = "unordered"
)


###################################################
### code chunk number 36: ranked-first-sd
###################################################
gaming_comparison <- merge(
  summary(gaming_rank)$posterior[, c("variable", "mean", "sd")],
  summary(gaming_first)$posterior[, c("variable", "mean", "sd")],
  by = "variable", suffixes = c("_ranked", "_first")
)
print(
  subset(gaming_comparison, grepl("^beta", variable)),
  digits = 2, row.names = FALSE
)


###################################################
### code chunk number 37: berserk-data
###################################################
data("lichess_berserk_choice", package = "choicedata")
berserk_data <- as.data.frame(lichess_berserk_choice)
berserk_data <- transform(
  berserk_data, rating = c(scale(rating)),
  ratingDifference = c(scale(ratingDifference)),
  minutesRemaining = c(scale(minutesRemaining))
)
berserk_split <- train_test(
  berserk_data, test_number = 1, by = "occasion", random = TRUE,
  column_occasion = "occasionID"
)
sapply(berserk_split, nrow)


###################################################
### code chunk number 38: berserk-fit
###################################################
berserk_fixed <- fit(
  berserk ~ 0 | white + rating + ratingDifference + minutesRemaining +
    streak,
  data = berserk_split$train, column_occasion = "occasionID",
  iterations = 1000, warmup = 500, chains = 2
)
berserk <- update(
  berserk_fixed, random_effects = "ASC", iterations = 10000,
  warmup = 5000, thin = 50, save_individual_draws = TRUE
)
round(cbind(mean = coef(berserk), confint(berserk, level = 0.9)), 3)


###################################################
### code chunk number 39: holdout
###################################################
known <- berserk_split$test$deciderID %in%
  berserk_split$train$deciderID
holdout <- berserk_split$test[known, ]
holdout_fixed <- predict(berserk_fixed, newdata = holdout)
holdout_conditional <- predict(
  berserk, newdata = holdout, type = "conditional"
)
head(holdout_conditional, 3)


###################################################
### code chunk number 40: roc-curve
###################################################
library("ggplot2")
library("plotROC")
roc_data <- rbind(
  data.frame(
    prediction = "conditional", berserk = holdout$berserk,
    probability = holdout_conditional$probability_TRUE
  ),
  data.frame(
    prediction = "without random effect", berserk = holdout$berserk,
    probability = holdout_fixed$probability_TRUE
  )
)
roc_plot <- ggplot(
  roc_data,
  aes(d = as.integer(berserk), m = probability, linetype = prediction)
) +
  geom_roc(n.cuts = 0) +
  style_roc(major.breaks = seq(0, 1, 0.25), minor.breaks = NULL) +
  theme(
    legend.position = "inside", legend.position.inside = c(0.65, 0.15),
    legend.title = element_blank()
  )
print(roc_plot)


###################################################
### code chunk number 41: ordered-grid
###################################################
health_grid <- expand.grid(
  age = c(0, seq(min(Health$age), max(Health$age))), income = 0,
  handper = c(0, 50), married = 1
)
grid_prediction <- predict(
  satisfaction, newdata = health_grid, uncertainty = TRUE
)
grid_prediction <- cbind(health_grid, grid_prediction)
subset(
  grid_prediction, age == 0,
  c(handper, probability_4, lower_4, upper_4)
)


###################################################
### code chunk number 42: ordered-prediction-figure
###################################################
op <- par(mar = c(4, 4, 0.5, 0.5))
plot(
  NA, xlim = c(25, 64), ylim = c(0, 0.25), xlab = "age in years",
  ylab = "probability of the highest level"
)
for (handicap in c(0, 50)) {
  rows <- which(grid_prediction$handper == handicap & grid_prediction$age != 0)
  years <- 25:64
  polygon(
    c(years, rev(years)),
    c(grid_prediction$lower_4[rows], rev(grid_prediction$upper_4[rows])),
    col = grey(0, alpha = 0.15), border = NA
  )
  lines(
    years, grid_prediction$probability_4[rows], lwd = 2,
    lty = if (handicap == 0) 1 else 2
  )
}
legend(
  "topright", legend = c("no handicap", "handicap of 50 percent"),
  lwd = 2, lty = 1:2, bty = "n"
)
par(op)


###################################################
### code chunk number 43: residuals
###################################################
train_residuals <- residuals(model_train)
head(train_residuals, 3)


###################################################
### code chunk number 44: loglik
###################################################
sapply(models, logLik)


###################################################
### code chunk number 45: loo
###################################################
train_loo <- lapply(models, loo)
loo_comparison <- loo::loo_compare(train_loo)
loo_comparison


###################################################
### code chunk number 46: elpd-differences-figure
###################################################
elpd_difference <- train_loo$price_normal$pointwise[, "elpd_loo"] -
  train_loo$model_train$pointwise[, "elpd_loo"]
op <- par(mar = c(4, 4, 0.5, 0.5))
plot(
  sort(elpd_difference), type = "h", col = "gray60",
  xlab = "travelers, ordered by the difference",
  ylab = "difference in elpd per traveler"
)
abline(h = 0)
abline(h = mean(elpd_difference), lty = 2)
par(op)


###################################################
### code chunk number 47: bf
###################################################
log_bf <- bayes_factor(price_normal, model_train, log = TRUE)
log_bf


###################################################
### code chunk number 48: cleanup
###################################################
future::plan(future::sequential)
