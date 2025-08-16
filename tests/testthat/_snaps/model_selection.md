# computing model selection criteria works

    Code
      model_selection(model_train, criteria = criteria)
    Output
               model_train
      npar               4
      LL          -1728.15
      AIC          3464.30
      BIC          3488.23
      WAIC         3463.51
      se(WAIC)        0.25
      pWAIC           3.84
      MMLL        -1730.59
      pred_acc      69.58%

---

    Code
      AIC(model_train)
    Output
      [1] 3464.295

---

    Code
      BIC(model_train)
    Output
      [1] 3488.225

---

    Code
      WAIC(model_train)
    Output
      3463.51 (0.25)

---

    Code
      nobs(model_train)
    Output
      [1] 2929

---

    Code
      logLik(model_train, recompute = TRUE)
    Output
      'log Lik.' -1728.148 (df=4)

---

    Code
      npar(model_train)
    Output
      [1] 4

