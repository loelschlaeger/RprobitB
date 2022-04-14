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


