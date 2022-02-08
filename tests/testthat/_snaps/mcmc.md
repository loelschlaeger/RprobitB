# P

    Code
      print(model)
    Output
      Probit model 'choice ~ cost | income | time'.

---

    Code
      summary(model)
    Output
      Probit model 'choice ~ cost | income | time'.
      
      MCMC settings:
      - R: 1000 
      - B: 500 
      - Q: 1 
      
      Normalization:
      - Level: Utility differences with respect to alternative 2.
      - Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Legend of alternatives:
        name
      1  bus
      2  car
      
      Legend of linear coefficients:
              name    re
      1       cost FALSE
      2 income_bus FALSE
      3    ASC_bus FALSE
      4   time_bus FALSE
      5   time_car FALSE
      
      Parameter statistics:
                true    mean      sd      R^
       alpha
                                            
           1    1.00    0.01    0.25    1.03
           2    2.00    1.79    0.77    1.24
           3    3.00    1.48    0.64    1.24
           4    4.00    2.46    0.90    1.32
           5    5.00    2.81    0.92    1.17
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00

# MNP

    Code
      print(model)
    Output
      Probit model 'choice ~ cost | income | time'.

---

    Code
      summary(model)
    Output
      Probit model 'choice ~ cost | income | time'.
      
      MCMC settings:
      - R: 1000 
      - B: 500 
      - Q: 1 
      
      Normalization:
      - Level: Utility differences with respect to alternative 3.
      - Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Legend of alternatives:
         name
      1   bus
      2   car
      3 train
      
      Legend of linear coefficients:
              name    re
      1       cost FALSE
      2 income_bus FALSE
      3 income_car FALSE
      4    ASC_bus FALSE
      5    ASC_car FALSE
      6   time_bus FALSE
      7   time_car FALSE
      8 time_train FALSE
      
      Parameter statistics:
                true    mean      sd      R^
       alpha
                                            
           1    0.39    0.88    0.39    1.00
           2    0.78    0.91    0.58    1.05
           3    1.17    0.59    0.59    1.07
           4    1.56    2.53    0.99    1.41
           5    1.95    2.60    0.96    1.26
           6    2.35    3.00    0.98    1.02
           7    2.74    2.48    0.87    1.01
           8    3.13    4.43    1.40    1.19
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.36    0.10    0.38    1.04
         2,2    1.26    1.80    1.57    1.00

# MMNP

    Code
      print(model)
    Output
      Probit model 'choice ~ cost | income | time'.

---

    Code
      summary(model)
    Output
      Probit model 'choice ~ cost | income | time'.
      
      MCMC settings:
      - R: 1000 
      - B: 500 
      - Q: 1 
      
      Normalization:
      - Level: Utility differences with respect to alternative 3.
      - Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Legend of alternatives:
         name
      1   bus
      2   car
      3 train
      
      Legend of linear coefficients:
              name    re
      1 income_bus FALSE
      2 income_car FALSE
      3   time_bus FALSE
      4   time_car FALSE
      5 time_train FALSE
      6       cost  TRUE
      7    ASC_bus  TRUE
      8    ASC_car  TRUE
      
      Number of latent classes: 1 
      
      Parameter statistics:
                true    mean      sd      R^
       alpha
                                            
           1    1.00    0.04    0.54    1.07
           2    2.00    1.40    1.02    1.59
           3    3.00    3.03    1.90    1.23
           4    4.00    5.25    2.74    1.28
           5    5.00    2.81    1.63    1.30
      
       s
                                            
           1    1.00    1.00    0.00     NaN
      
       b
                                            
         1.1    1.00    1.03    0.92    1.30
         1.2    2.00    0.37    0.55    1.04
         1.3    3.00    0.61    0.67    1.00
      
       Omega
                                            
       1.1,1    1.00    2.87    3.68    1.00
       1.1,2    0.00   -0.03    1.24    1.07
       1.1,3    0.00   -0.63    1.69    1.02
       1.2,2    1.00    0.83    1.03    1.00
       1.2,3    0.00    0.02    0.61    1.00
       1.3,3    1.00    1.01    1.33    1.00
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.00   -0.15    0.77    2.42
         2,2    1.00    2.19    3.54    1.11

# LCMMNP

    Code
      print(model)
    Output
      Probit model 'choice ~ cost | income | time'.

---

    Code
      summary(model)
    Output
      Probit model 'choice ~ cost | income | time'.
      
      MCMC settings:
      - R: 1000 
      - B: 500 
      - Q: 1 
      
      Normalization:
      - Level: Utility differences with respect to alternative 3.
      - Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Legend of alternatives:
         name
      1   bus
      2   car
      3 train
      
      Legend of linear coefficients:
              name    re
      1 income_bus FALSE
      2 income_car FALSE
      3   time_bus FALSE
      4   time_car FALSE
      5 time_train FALSE
      6       cost  TRUE
      7    ASC_bus  TRUE
      8    ASC_car  TRUE
      
      Number of latent classes: 2 
      
      Parameter statistics:
                true    mean      sd      R^
       alpha
                                            
           1   -0.80   -1.25    0.49    1.00
           2   -0.46   -1.00    0.39    1.02
           3    0.23    0.42    0.23    1.01
           4    1.37    1.92    0.83    1.20
           5   -1.03   -1.37    0.58    1.00
      
       s
                                            
           1    0.68    0.69    0.10    1.00
           2    0.32    0.31    0.10    1.00
      
       b
                                            
         1.1   -1.49   -1.66    0.70    1.03
         1.2   -1.03   -0.99    0.63    1.03
         1.3   -1.09   -1.04    0.66    1.00
         2.1    0.63    0.29    0.63    1.00
         2.2   -0.40   -0.40    0.63    1.09
         2.3    0.92    0.75    0.85    1.00
      
       Omega
                                            
       1.1,1    0.65    1.30    1.32    1.00
       1.1,2   -0.53   -0.24    0.76    1.10
       1.1,3   -0.13   -0.27    0.70    1.00
       1.2,2    0.63    1.63    1.89    1.11
       1.2,3    0.03    0.80    1.18    1.16
       1.3,3    0.10    1.66    2.35    1.09
       2.1,1    0.41    1.47    1.60    1.05
       2.1,2   -0.08    0.12    0.99    1.08
       2.1,3    0.14    0.47    1.49    1.10
       2.2,2    0.34    1.44    1.79    1.01
       2.2,3    0.02    0.75    1.78    1.00
       2.3,3    0.06    2.73    3.13    1.01
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.95    0.16    0.36    1.03
         2,2    1.12    0.98    0.53    1.00

# ULCMMNP

    Code
      print(model)
    Output
      Probit model 'choice ~ cost'.

---

    Code
      summary(model)
    Output
      Probit model 'choice ~ cost'.
      
      MCMC settings:
      - R: 2000 
      - B: 1000 
      - Q: 1 
      
      Normalization:
      - Level: Utility differences with respect to alternative 3.
      - Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Legend of alternatives:
         name
      1   bus
      2   car
      3 train
      
      Legend of linear coefficients:
           name   re
      1    cost TRUE
      2 ASC_bus TRUE
      3 ASC_car TRUE
      
      DP-based update: FALSE 
      Weight-based update: TRUE 
      - Initial classes: 2 
      - Maximum classes: 10 
      - Updating buffer: 100 
      - Minimum class weight: 0.1 
      - Maximum class weight: 0.9 
      - Mimumum class distance: 0.1 
      
      Parameter statistics:
                true    mean      sd      R^
       s
                                            
           1    0.92    0.89    0.04    1.04
           2    0.08    0.11    0.04    1.04
      
       b
                                            
         1.1    1.43    1.84    0.29    1.00
         1.2    1.61    1.81    0.30    1.02
         1.3    0.60    0.71    0.21    1.35
         2.1    0.48    0.58    0.62    1.00
         2.2   -1.55   -1.28    0.66    1.21
         2.3   -1.07   -1.50    0.73    1.09
      
       Omega
                                            
       1.1,1    0.20    0.78    0.32    1.16
       1.1,2   -0.31   -0.47    0.25    1.00
       1.1,3   -0.08    0.14    0.16    1.05
       1.2,2    1.29    1.77    0.67    1.05
       1.2,3   -0.04    0.05    0.28    1.81
       1.3,3    0.11    0.53    0.27    1.60
       2.1,1    0.45    1.80    1.44    1.23
       2.1,2   -0.09   -0.00    0.81    1.00
       2.1,3    0.15    0.66    1.04    1.02
       2.2,2    0.36    2.12    1.97    1.17
       2.2,3    0.02    0.43    1.21    1.00
       2.3,3    0.07    2.31    1.97    1.19
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.14   -0.16    0.23    1.05
         2,2    0.65    1.16    0.38    1.01

