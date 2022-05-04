# model_selection works

    Code
      model_selection(model_train, model_train_sparse, criteria = criteria)
    Output
                               model_train model_train_sparse
      npar                               4                  1
      LL                          -1727.80           -1865.87
      AIC                          3463.59            3733.73
      BIC                          3487.52            3739.71
      WAIC                         3464.15            3733.73
      se(WAIC)                        0.19               0.07
      pWAIC                           4.48               1.05
      MMLL                        -1733.88           -1866.76
      BF(*,model_train)                  1             < 0.01
      BF(*,model_train_sparse)       > 100                  1
      pred_acc                      69.48%             63.37%

# AIC computation works

    Code
      AIC(model_train)
    Output
      [1] 3463.592

# BIC computation works

    Code
      BIC(model_train)
    Output
      [1] 3487.521

# WAIC computation works

    Code
      WAIC(model_train)
    Output
      3464.15 (0.19)

# nobs works

    Code
      nobs(model_train)
    Output
      [1] 2929

# logLik computation works

    Code
      logLik(model_train, recompute = TRUE)
    Output
      [1] -1727.796

# npar works

    Code
      npar(model_train)
    Output
      [1] 4

