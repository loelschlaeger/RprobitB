# model_selection works

    Code
      model_selection(model_train, model_train_sparse, criteria = criteria)
    Output
                               model_train model_train_sparse
      npar                               4                  1
      LL                          -1727.70           -1865.86
      AIC                          3463.41            3733.73
      BIC                          3487.34            3739.71
      WAIC                         3463.74            3733.87
      se(WAIC)                        0.19               0.07
      pWAIC                           4.37               1.13
      MMLL                        -1731.13           -1867.26
      BF(*,model_train)                  1             < 0.01
      BF(*,model_train_sparse)       > 100                  1
      pred_acc                      69.61%             63.40%

# AIC computation works

    Code
      AIC(model_train)
    Output
      [1] 3463.408

# BIC computation works

    Code
      BIC(model_train)
    Output
      [1] 3487.338

# WAIC computation works

    Code
      WAIC(model_train)
    Output
      3463.74 (0.19)

# nobs works

    Code
      nobs(model_train)
    Output
      [1] 2929

# logLik computation works

    Code
      logLik(model_train, recompute = TRUE)
    Output
      [1] -1727.704

# npar works

    Code
      npar(model_train)
    Output
      [1] 4

