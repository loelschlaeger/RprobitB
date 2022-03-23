# model_selection works

    Code
      model_selection(model_train, model_train_sparse, criteria = criteria)
    Output
                         npar       LL     AIC     BIC    WAIC se(WAIC) pWAIC
      model_train           4 -1727.74 3463.48 3487.41 3463.76     0.18  4.32
      model_train_sparse    1 -1865.86 3733.72 3739.70 3733.91     0.07  1.15
                             MMLL BF:model_train BF:model_train_sparse pred_acc
      model_train        -1732.14              1                 > 100   69.55%
      model_train_sparse -1867.48         < 0.01                     1   63.37%

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

