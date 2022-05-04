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
### code chunk number 3: example 1 train data overview
###################################################
data("Train", package = "mlogit")
str(Train)


###################################################
### code chunk number 4: example 1 train data formula
###################################################
form <- choice ~ price + time + comfort + change | 0
re <- c("price","time")


###################################################
### code chunk number 5: example 1 train data prepare
###################################################
data <- prepare_data(
  form = form,
  choice_data = Train,
  re = re,
  id = "id",
  idc = "choiceid"
  )
summary(data)


###################################################
### code chunk number 6: train-data
###################################################
plot(data)


###################################################
### code chunk number 7: simulate choices call (eval = FALSE)
###################################################
## data <- simulate_choices(form = form, N = N, T = T, J = J)


###################################################
### code chunk number 8: example 2 simulated data meta
###################################################
N <- 100
T <- 10
alternatives <- c("alt1", "alt2")
base_alternative <- "alt2"
form <- choice ~ var1 | var2 | var3
re <- c("ASC","var2")


###################################################
### code chunk number 9: data-sim-overview
###################################################
overview_effects(form = form, re = re, alternatives = alternatives,
                 base_alternative = base_alternative)


###################################################
### code chunk number 10: data-sim
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
  C = 2,
  s = c(0.7,0.3),
  b = matrix(c(2,-0.5,1,1), ncol = 2)
)


###################################################
### code chunk number 11: data-sim-plot-by-choice
###################################################
plot(data, by_choice = TRUE)


###################################################
### code chunk number 12: data-split-deciders
###################################################
train_test(data, test_proportion = 0.3, by = "N")


###################################################
### code chunk number 13: data-split-occasions
###################################################
train_test(data, test_number = 2, by = "T", random = TRUE, seed = 1)


###################################################
### code chunk number 14: fit_model call (eval = FALSE)
###################################################
## fit_model(data = data)


