# computing model selection criteria works

    Code
      model_selection(model_train, criteria = criteria)
    Output
               model_train
      npar               4
      LL          -1728.59
      AIC          3465.18
      BIC          3489.10
      WAIC         3461.27
      se(WAIC)        0.07
      pWAIC           2.10
      MMLL        -1730.35
      pred_acc      69.10%

---

    Code
      AIC(model_train)
    Output
      [1] 3465.175

---

    Code
      BIC(model_train)
    Output
      [1] 3489.105

---

    Code
      WAIC(model_train)
    Output
      3461.27 (0.07)

---

    Code
      nobs(model_train)
    Output
      [1] 2929

---

    Code
      logLik(model_train, recompute = TRUE)
    Output
      'log Lik.' -1728.588 (df=4)

---

    Code
      npar(model_train)
    Output
      [1] 4

