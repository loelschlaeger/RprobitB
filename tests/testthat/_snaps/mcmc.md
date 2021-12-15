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
                                            
           1    0.39    0.68    0.47    1.48
           2    0.78    1.21    0.74    1.49
           3    1.17    1.27    0.68    1.34
           4    1.56    2.20    0.81    1.11
           5    1.95    1.95    0.62    1.03
           6    2.35    2.81    1.01    1.29
           7    2.74    2.58    0.90    1.84
           8    3.13    3.94    1.21    1.26
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.36    0.03    0.33    1.43
         2,2    1.26    0.91    0.72    1.24

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
      
      Latent classes: 1 
      - Update: FALSE 
      
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
      
      Latent classes: 2 
      - Update: FALSE 
      
      Parameter statistics:
                true    mean      sd      R^
       alpha
                                            
           1   -0.80   -0.78    0.19    1.06
           2   -0.46   -0.40    0.14    1.06
           3    0.23    0.26    0.14    1.28
           4    1.37    1.33    0.25    1.04
           5   -1.03   -0.95    0.19    1.07
      
       s
                                            
           1    0.68    0.71    0.09    1.01
           2    0.32    0.29    0.09    1.01
      
       b
                                            
         1.1   -1.49   -1.28    0.29    1.03
         1.2   -1.03   -1.09    0.29    1.02
         1.3   -1.09   -1.14    0.32    1.00
         2.1    0.63    0.39    0.31    1.00
         2.2   -0.40   -0.45    0.35    1.00
         2.3    0.92    0.87    0.40    1.03
      
       Omega
                                            
       1.1,1    0.65    0.54    0.26    1.05
       1.1,2   -0.53   -0.03    0.19    1.47
       1.1,3   -0.13    0.01    0.17    1.46
       1.2,2    0.63    0.50    0.30    1.00
       1.2,3    0.03    0.05    0.16    1.00
       1.3,3    0.10    0.35    0.19    1.02
       2.1,1    0.41    0.43    0.30    1.01
       2.1,2   -0.08    0.02    0.22    1.08
       2.1,3    0.14    0.20    0.33    1.02
       2.2,2    0.34    0.53    0.42    1.01
       2.2,3    0.02    0.10    0.32    1.00
       2.3,3    0.06    0.90    0.74    1.10
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.95    0.11    0.15    1.27
         2,2    1.12    0.30    0.20    1.01

# ULCMMNP

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
              name    re
      1 income_bus FALSE
      2 income_car FALSE
      3   time_bus FALSE
      4   time_car FALSE
      5 time_train FALSE
      6       cost  TRUE
      7    ASC_bus  TRUE
      8    ASC_car  TRUE
      
      Latent classes: 2 
      - Update: TRUE 
      - Maximum number: 10 
      - Buffer: 100 
      - Minimum class weight: 0.1 
      - Maximum class weight: 0.9 
      - Mimumum class distance: 0.1 
      
      Parameter statistics:
                true    mean      sd      R^
       alpha
                                            
           1   -0.83   -1.01    0.18    1.15
           2   -0.48   -0.65    0.11    1.00
           3    0.24    0.12    0.10    1.06
           4    1.43    1.34    0.27    1.52
           5   -1.07   -1.09    0.17    1.15
      
       s
                                            
           1    0.68    0.71    0.10    1.31
           2    0.32    0.29    0.10    1.31
      
       b
                                            
         1.1   -1.55   -1.46    0.39    1.01
         1.2   -1.07   -0.68    0.24    1.32
         1.3   -1.13   -0.95    0.29    1.09
         2.1    0.65    0.25    0.23    1.04
         2.2   -0.42   -0.32    0.42    1.00
         2.3    0.95    0.61    0.43    1.11
      
       Omega
                                            
       1.1,1    0.70    0.96    0.55    1.98
       1.1,2   -0.57   -0.37    0.31    1.25
       1.1,3   -0.14    0.24    0.27    1.66
       1.2,2    0.68    0.70    0.45    1.48
       1.2,3    0.03    0.04    0.19    1.01
       1.3,3    0.11    0.45    0.29    1.85
       2.1,1    0.45    0.40    0.22    1.23
       2.1,2   -0.09    0.07    0.25    1.00
       2.1,3    0.15    0.15    0.31    1.00
       2.2,2    0.36    0.86    0.54    1.00
       2.2,3    0.02    0.37    0.55    1.05
       2.3,3    0.07    0.94    0.92    1.01
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.14    0.49    0.17    1.01
         2,2    0.65    0.64    0.22    1.06

