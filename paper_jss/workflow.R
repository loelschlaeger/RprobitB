try(setwd("paper_jss"),silent=TRUE)
Sweave("article.Rnw")
tools::texi2pdf("article.tex")
Stangle("article.Rnw")
