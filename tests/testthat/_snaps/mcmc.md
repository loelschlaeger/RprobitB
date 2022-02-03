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
      
      Latent classes: 5 
      - Update: TRUE 
      - Maximum number: 10 
      - Buffer: 100 
      - Minimum class weight: 0.1 
      - Maximum class weight: 0.9 
      - Mimumum class distance: 0.1 
      
      Parameter statistics:
                true    mean      sd      R^
       alpha
                                            
           1   -0.83   -0.78    0.17    1.00
           2   -0.48   -0.40    0.14    1.31
           3    0.24    0.10    0.11    1.01
           4    1.43    1.19    0.28    1.02
           5   -1.07   -0.95    0.20    1.00
      
       s
                                            
           1    0.68    0.56    0.09    1.02
           2    0.32    0.21    0.06    1.00
           3      NA    0.12    0.04    1.10
           4      NA    0.07    0.03    1.00
           5      NA    0.04    0.02    1.00
      
       b
                                            
         1.1   -1.55   -1.23    0.32    1.00
         1.2   -1.07   -0.75    0.26    1.69
         1.3   -1.13   -0.80    0.21    1.00
         2.1    0.65    0.32    0.26    1.17
         2.2   -0.42   -0.62    0.40    1.26
         2.3    0.95    0.48    0.36    1.17
         3.1      NA   -0.34    0.71    1.02
         3.2      NA   -0.62    0.64    1.02
         3.3      NA   -0.11    0.60    1.03
         4.1      NA   -0.09    0.51    1.09
         4.2      NA   -0.18    0.49    1.06
         4.3      NA    0.08    0.55    1.13
         5.1      NA   -0.11    0.61    1.01
         5.2      NA   -0.15    0.58    1.00
         5.3      NA    0.03    0.60    1.00
      
       Omega
                                            
       1.1,1    0.70    0.55    0.32    1.02
       1.1,2   -0.57   -0.31    0.25    1.16
       1.1,3   -0.14    0.04    0.15    1.02
       1.2,2    0.68    0.62    0.40    1.02
       1.2,3    0.03    0.04    0.14    1.04
       1.3,3    0.11    0.24    0.16    1.17
       2.1,1    0.45    0.39    0.39    1.39
       2.1,2   -0.09   -0.08    0.38    1.25
       2.1,3    0.15    0.11    0.22    1.00
       2.2,2    0.36    0.71    0.83    1.23
       2.2,3    0.02    0.08    0.27    1.05
       2.3,3    0.07    0.42    0.26    1.01
       3.1,1      NA    0.55    0.53    1.21
       3.1,2      NA   -0.01    0.47    1.01
       3.1,3      NA    0.10    0.31    1.00
       3.2,2      NA    0.72    1.06    1.19
       3.2,3      NA    0.08    0.39    1.05
       3.3,3      NA    0.55    0.48    1.31
       4.1,1      NA    0.57    0.51    1.03
       4.1,2      NA    0.00    0.56    1.02
       4.1,3      NA    0.07    0.48    1.00
       4.2,2      NA    0.71    1.43    1.01
       4.2,3      NA    0.13    1.36    1.00
       4.3,3      NA    0.63    1.79    1.02
       5.1,1      NA    0.70    1.08    1.05
       5.1,2      NA   -0.10    0.60    1.01
       5.1,3      NA    0.05    0.77    1.00
       5.2,2      NA    0.74    1.42    1.07
       5.2,3      NA    0.04    0.56    1.00
       5.3,3      NA    0.67    1.07    1.06
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.14    0.12    0.23    1.08
         2,2    0.65    0.50    0.23    1.01

