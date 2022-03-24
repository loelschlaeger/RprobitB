# model_selection works

    Code
      model_selection(model_train, model_train_sparse, criteria = criteria)
    Output
                               model_train model_train_sparse
      npar                               4                  1
      LL                          -1727.74           -1865.86
      AIC                          3463.48            3733.72
      BIC                          3487.41            3739.70
      WAIC                         3463.76            3733.91
      se(WAIC)                        0.18               0.07
      pWAIC                           4.32               1.15
      MMLL                        -1732.14           -1867.48
      BF(*,model_train)                  1             < 0.01
      BF(*,model_train_sparse)       > 100                  1
      pred_acc                      69.55%             63.37%

# AIC computation works

    Code
      AIC(model_train)
    Output
      [1] 3463.485

# BIC computation works

    Code
      BIC(model_train)
    Output
      [1] 3487.414

# WAIC computation works

    Code
      WAIC(model_train)
    Output
      3463.76 (0.18)

# nobs works

    Code
      nobs(model_train)
    Output
      [1] 2929

# logLik computation works

    Code
      logLik(model_train, recompute = TRUE)
    Output
      [1] -1727.742

# npar works

    Code
      npar(model_train)
    Output
      [1] 4

