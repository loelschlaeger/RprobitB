callr::r(
  function() {
    utils::Sweave("rprobitb_oelschlaeger_bauer.Rnw", encoding = "utf8")
    tinytex::pdflatex("rprobitb_oelschlaeger_bauer.tex")
    utils::Stangle("rprobitb_oelschlaeger_bauer.Rnw", encoding = "utf8")
  },
  wd = "jss", show = TRUE
)
