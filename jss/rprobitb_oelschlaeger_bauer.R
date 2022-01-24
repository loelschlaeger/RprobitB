### R code from vignette source 'rprobitb_oelschlaeger_bauer.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: load RprobitB
###################################################
library(RprobitB)


###################################################
### code chunk number 3: inputs prepare_data
###################################################
args(prepare_data)


###################################################
### code chunk number 4: Train dataset
###################################################
data("Train", package = "mlogit")
Train$price_A = Train$price_A / 100 * 2.2
Train$price_B = Train$price_B / 100 * 2.2


###################################################
### code chunk number 5: Train specification
###################################################
form = choice ~ price | 1 | time + comfort + change
data = prepare_data(form = form, choice_data = Train)


###################################################
### code chunk number 6: Train estimation
###################################################
m1 = RprobitB::mcmc(data)


###################################################
### code chunk number 7: some estimated model
###################################################
x = 1


