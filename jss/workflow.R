try(setwd("jss"),silent=TRUE)
Sweave("rprobitb_oelschlaeger_bauer.Rnw", encoding = "utf8")
tinytex::pdflatex("rprobitb_oelschlaeger_bauer.tex")
# Stangle("rprobitb_oelschlaeger_bauer.Rnw", encoding = "utf8")
