try(setwd("paper_jss"),silent=TRUE)
Sweave("rprobitb_oelschlaeger_bauer_jss_2021.Rnw")
tools::texi2pdf("rprobitb_oelschlaeger_bauer_jss_2021.tex")
Stangle("rprobitb_oelschlaeger_bauer_jss_2021.Rnw")

