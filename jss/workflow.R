try(setwd("jss"),silent=TRUE)
Sweave("rprobitb_oelschlaeger_bauer.Rnw")
tinytex::pdflatex("rprobitb_oelschlaeger_bauer.tex")
Stangle("rprobitb_oelschlaeger_bauer.Rnw")

x <- seq(-2,2,0.01)
f.x <- x/(1-x)
plot(x,f.x,type = "l")


x <- arima.sim(list(order=c(1,0,0), ar = -0.9), n = 100) # AR(1)-Sim.
sum(acf(x, plot = F, lag.max = 10)$acf[,,1]) - 1
(spec.ar(x, plot = F)$spec[1]/var(x) - 1) / 2
