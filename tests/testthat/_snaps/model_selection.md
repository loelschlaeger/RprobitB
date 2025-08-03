# computing model selection criteria works

    Code
      model_selection(model_train, criteria = criteria)
    Output
               model_train
      npar               4
      LL          -1728.22
      AIC          3464.44
      BIC          3488.37
      WAIC         3467.35
      se(WAIC)        0.45
      pWAIC           5.90
      MMLL        -1731.44
      pred_acc      69.55%

---

    Code
      AIC(model_train)
    Output
      [1] 3464.44

---

    Code
      BIC(model_train)
    Output
      [1] 3488.369

---

    Code
      WAIC(model_train)
    Output
      3467.35 (0.45)

---

    Code
      nobs(model_train)
    Output
      [1] 2929

---

    Code
      logLik(model_train, recompute = TRUE)
    Output
      'log Lik.' -1728.22 (df=4)

---

    Code
      npar(model_train)
    Output
      [1] 4

