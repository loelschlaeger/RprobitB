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
                                            
           1   -0.80   -1.18    0.45    1.05
           2   -0.46   -0.51    0.24    1.03
           3    0.23    0.35    0.21    1.23
           4    1.37    2.61    0.94    1.00
           5   -1.03   -1.41    0.47    1.00
      
       s
                                            
           1    0.68    0.75    0.11    1.11
           2    0.32    0.25    0.11    1.11
      
       b
                                            
         1.1   -1.49   -1.76    0.70    1.11
         1.2   -1.03   -1.84    0.68    1.05
         1.3   -1.09   -1.41    0.63    1.04
         2.1    0.63    0.56    0.74    1.11
         2.2   -0.40   -0.85    0.69    1.03
         2.3    0.92    0.73    0.94    1.27
      
       Omega
                                            
       1.1,1    0.65    2.37    2.19    1.21
       1.1,2   -0.53    0.28    0.49    1.02
       1.1,3   -0.13    0.81    1.20    1.44
       1.2,2    0.63    0.60    0.46    1.09
       1.2,3    0.03    0.21    0.42    1.00
       1.3,3    0.10    1.42    1.43    1.15
       2.1,1    0.41    1.62    2.15    1.00
       2.1,2   -0.08   -0.24    1.10    1.07
       2.1,3    0.14    0.64    1.69    1.00
       2.2,2    0.34    1.16    1.29    1.00
       2.2,3    0.02   -0.29    0.97    1.00
       2.3,3    0.06    2.53    3.79    1.02
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.95    0.61    0.24    1.04
         2,2    1.12    1.35    0.68    1.06

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
      
      Latent classes: 3 
      - Update: TRUE 
      - Maximum number: 10 
      - Buffer: 100 
      - Minimum class weight: 0.1 
      - Maximum class weight: 0.9 
      - Mimumum class distance: 0.1 
      
      Parameter statistics:
                true    mean      sd      R^
       alpha
                                            
           1   -0.83   -1.06    0.29    1.06
           2   -0.48   -0.55    0.17    1.03
           3    0.24    0.15    0.15    1.02
           4    1.43    1.56    0.46    1.34
           5   -1.07   -1.33    0.37    1.05
      
       s
                                            
           1    0.68    0.68    0.11    1.00
           2    0.32    0.23    0.08    1.00
           3      NA    0.08    0.06    1.01
      
       b
                                            
         1.1   -1.55   -1.39    0.35    1.00
         1.2   -1.07   -1.09    0.46    1.85
         1.3   -1.13   -1.06    0.36    1.62
         2.1    0.65   -0.01    0.78    1.98
         2.2   -0.42   -0.18    0.80    1.02
         2.3    0.95    0.69    0.77    1.00
         3.1      NA   -0.13    0.90    1.00
         3.2      NA   -0.38    0.96    1.02
         3.3      NA    0.14    0.88    1.01
      
       Omega
                                            
       1.1,1    0.70    1.33    1.07    1.16
       1.1,2   -0.57   -0.36    0.40    1.03
       1.1,3   -0.14    0.68    0.82    1.22
       1.2,2    0.68    0.87    0.60    1.03
       1.2,3    0.03   -0.11    0.32    1.00
       1.3,3    0.11    0.95    1.03    1.57
       2.1,1    0.45    1.36    1.77    1.82
       2.1,2   -0.09   -0.09    1.05    1.00
       2.1,3    0.15    0.43    1.07    1.16
       2.2,2    0.36    1.66    2.25    1.35
       2.2,3    0.02    0.41    1.40    1.10
       2.3,3    0.07    1.49    2.16    1.09
       3.1,1      NA    1.76    2.34    1.38
       3.1,2      NA   -0.03    1.55    1.02
       3.1,3      NA    0.11    1.31    1.01
       3.2,2      NA    2.11    6.09    1.08
       3.2,3      NA   -0.10    3.24    1.00
       3.3,3      NA    1.66    2.76    1.21
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.14    0.05    0.40    2.69
         2,2    0.65    1.12    0.58    1.03

