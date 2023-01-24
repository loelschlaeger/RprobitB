# RprobitB_formula can be printed

    Code
      RprobitB_formula(formula = choice ~ A | B, re = NULL, ordered = FALSE)
    Output
      Formula: choice ~ A | B

---

    Code
      RprobitB_formula(formula = choice ~ A + B, re = c("A+", "B"), ordered = TRUE)
    Output
      Formula: choice ~ A + B
      Random effects: A+ B

