### R code from vignette source 'article.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library("MASS")


###################################################
### code chunk number 2: data
###################################################
data("quine", package = "MASS")


###################################################
### code chunk number 3: visualization
###################################################
par(mar = c(4, 4, 1, 1))
plot(table(quine$Days), xlab = "Days", ylab = "Frequency", axes = FALSE)
axis(2)
axis(1, at = 0:16 * 5, labels = FALSE)
axis(1, at = 0:8 * 10)
box()


###################################################
### code chunk number 4: poisson
###################################################
m_pois <- glm(Days ~ (Eth + Sex + Age + Lrn)^2, data = quine,
  family = poisson)


###################################################
### code chunk number 5: negbin
###################################################
library("MASS")
m_nbin <- glm.nb(Days ~ (Eth + Sex + Age + Lrn)^2, data = quine)


###################################################
### code chunk number 6: comparison
###################################################
BIC(m_pois, m_nbin)


###################################################
### code chunk number 7: summary
###################################################
summary(m_nbin)


