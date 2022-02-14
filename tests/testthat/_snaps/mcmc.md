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
      - R: 2000 
      - B: 1000 
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
                                            
           1    1.00   -0.08    0.29    1.00
           2    2.00    1.62    0.68    1.00
           3    3.00    1.43    0.47    1.00
           4    4.00    2.57    0.74    1.00
           5    5.00    2.94    0.85    1.00
      
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
                                            
           1    1.00   -0.13    0.37    1.04
           2    2.00    0.77    0.52    1.75
           3    3.00    2.13    1.26    1.36
           4    4.00    3.96    2.54    1.18
           5    5.00    2.37    1.55    1.66
      
       s
                                            
           1    1.00    1.00    0.00     NaN
      
       b
                                            
         1.1    1.00    0.92    0.80    1.62
         1.2    2.00    0.82    0.91    1.10
         1.3    3.00    1.06    0.80    1.29
      
       Omega
                                            
       1.1,1    1.00    1.79    2.43    1.00
       1.1,2    0.00   -0.49    1.37    1.04
       1.1,3    0.00   -0.03    1.36    1.09
       1.2,2    1.00    1.30    2.06    1.12
       1.2,3    0.00   -0.38    1.65    1.07
       1.3,3    1.00    1.35    2.68    1.13
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.00   -0.01    0.60    1.78
         2,2    1.00    1.17    1.08    1.00

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
      - R: 500 
      - B: 250 
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
                                            
           1   -0.80   -1.40    0.48    2.47
           2   -0.46   -1.11    0.40    1.29
           3    0.23    0.29    0.17    1.55
           4    1.37    1.77    0.50    1.44
           5   -1.03   -1.47    0.38    1.28
      
       s
                                            
           1    0.68    0.66    0.08    0.99
           2    0.32    0.34    0.08    0.99
      
       b
                                            
         1.1   -1.49   -2.11    0.80    1.86
         1.2   -1.03   -1.29    0.56    3.48
         1.3   -1.09   -1.56    0.72    2.29
         2.1    0.63    0.19    0.37    1.03
         2.2   -0.40   -0.11    0.48    1.02
         2.3    0.92    0.84    0.49    1.07
      
       Omega
                                            
       1.1,1    0.65    1.84    1.43    1.37
       1.1,2   -0.53   -0.30    0.52    1.00
       1.1,3   -0.13   -0.70    0.70    1.01
       1.2,2    0.63    0.73    0.70    0.99
       1.2,3    0.03    0.27    0.36    0.99
       1.3,3    0.10    0.88    0.66    1.03
       2.1,1    0.41    0.71    0.64    1.00
       2.1,2   -0.08   -0.02    0.35    1.00
       2.1,3    0.14    0.26    0.74    1.00
       2.2,2    0.34    0.71    0.58    1.35
       2.2,3    0.02    0.36    0.83    1.02
       2.3,3    0.06    1.43    2.24    1.00
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    0.99
         1,2    0.95    0.60    0.20    1.12
         2,2    1.12    0.90    0.46    1.00

# LCMMNP weight update

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
      - R: 500 
      - B: 250 
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
      - Initial classes: 6 
      - Maximum classes: 10 
      - Updating buffer: 100 
      - Minimum class weight: 0.1 
      - Maximum class weight: 0.9 
      - Mimumum class distance: 0.1 
      
      Parameter statistics:
                true    mean      sd      R^
       s
                                            
           1    0.92    0.50    0.09    1.35
           2    0.08    0.33    0.12    4.39
           3      NA    0.09    0.04    2.59
           4      NA    0.05    0.03    2.52
           5      NA    0.02    0.01    3.22
           6      NA    0.01    0.01    2.15
      
       b
                                            
         1.1    1.43    1.64    0.29    1.21
         1.2    1.61    2.71    0.54    1.29
         1.3    0.60    0.78    0.22    1.09
         2.1    0.48    1.89    0.35    0.99
         2.2   -1.55    1.05    0.48    1.06
         2.3   -1.07    0.79    0.28    1.37
         3.1      NA    1.11    1.05    1.68
         3.2      NA   -0.71    1.15    2.59
         3.3      NA   -0.81    1.20    1.45
         4.1      NA    0.48    0.83    1.02
         4.2      NA   -1.34    1.09    1.10
         4.3      NA   -1.69    0.97    1.03
         5.1      NA    0.38    0.93    1.16
         5.2      NA    0.08    0.99    1.03
         5.3      NA   -0.16    1.00    1.01
         6.1      NA    0.13    0.76    1.00
         6.2      NA    0.14    0.89    1.02
         6.3      NA   -0.01    0.80    1.00
      
       Omega
                                            
       1.1,1    0.20    0.39    0.18    1.66
       1.1,2   -0.31   -0.29    0.25    1.02
       1.1,3   -0.08    0.03    0.10    1.14
       1.2,2    1.29    1.85    0.75    0.99
       1.2,3   -0.04   -0.02    0.26    1.07
       1.3,3    0.11    0.37    0.22    2.39
       2.1,1    0.45    0.44    0.25    1.00
       2.1,2   -0.09   -0.06    0.15    1.26
       2.1,3    0.15    0.04    0.12    1.06
       2.2,2    0.36    0.53    0.33    1.62
       2.2,3    0.02    0.06    0.13    1.00
       2.3,3    0.07    0.30    0.16    1.03
       3.1,1      NA    0.97    1.04    1.08
       3.1,2      NA   -0.01    0.75    1.21
       3.1,3      NA    0.38    0.93    1.37
       3.2,2      NA    1.18    0.94    1.01
       3.2,3      NA    0.28    0.73    1.15
       3.3,3      NA    1.31    2.01    1.41
       4.1,1      NA    1.16    0.97    1.07
       4.1,2      NA   -0.15    0.92    1.00
       4.1,3      NA    0.24    0.92    0.99
       4.2,2      NA    1.48    2.03    1.09
       4.2,3      NA    0.17    1.24    1.00
       4.3,3      NA    1.63    1.71    1.22
       5.1,1      NA    1.21    1.38    1.02
       5.1,2      NA   -0.05    1.19    0.99
       5.1,3      NA   -0.10    1.02    0.99
       5.2,2      NA    1.45    2.84    1.00
       5.2,3      NA    0.29    1.22    1.02
       5.3,3      NA    1.36    1.56    1.05
       6.1,1      NA    1.25    2.07    1.03
       6.1,2      NA   -0.02    1.40    1.00
       6.1,3      NA   -0.07    1.49    0.99
       6.2,2      NA    0.99    1.33    1.07
       6.2,3      NA   -0.01    1.11    0.99
       6.3,3      NA    1.11    1.68    1.00
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    0.99
         1,2    0.14   -0.21    0.20    1.82
         2,2    0.65    0.89    0.21    1.30

# LCMMNP DP update

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
      - R: 500 
      - B: 250 
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
      
      DP-based update: TRUE 
      Weight-based update: FALSE 
      - Initial classes: 3 
      - Maximum classes: 10 
      
      Parameter statistics:
                true    mean      sd      R^
       s
                                            
           1    0.92    0.90    0.00     NaN
           2    0.08    0.07    0.00     NaN
           3      NA    0.03    0.00     NaN
      
       b
                                            
         1.1    1.43    1.79    0.22    1.44
         1.2    1.61    1.81    0.23    1.30
         1.3    0.60    0.79    0.17    1.03
         2.1    0.48    0.29    0.39    1.01
         2.2   -1.55   -1.69    0.58    1.04
         2.3   -1.07   -1.90    0.67    1.61
         3.1      NA    0.62    1.11    1.00
         3.2      NA    1.02    1.34    1.12
         3.3      NA    0.21    1.30    1.01
      
       Omega
                                            
       1.1,1    0.20    0.59    0.18    1.64
       1.1,2   -0.31   -0.53    0.21    0.99
       1.1,3   -0.08    0.08    0.10    1.81
       1.2,2    1.29    1.42    0.56    1.09
       1.2,3   -0.04   -0.02    0.16    1.22
       1.3,3    0.11    0.28    0.15    4.55
       2.1,1    0.45    1.01    0.69    1.58
       2.1,2   -0.09    0.11    0.66    1.53
       2.1,3    0.15    0.65    0.76    1.67
       2.2,2    0.36    1.26    1.29    1.19
       2.2,3    0.02    0.26    1.00    1.40
       2.3,3    0.07    1.83    2.08    2.10
       3.1,1      NA    1.78    2.58    0.99
       3.1,2      NA   -0.34    2.04    1.05
       3.1,3      NA    0.00    1.11    0.99
       3.2,2      NA    1.79    3.03    1.02
       3.2,3      NA    0.19    1.14    1.00
       3.3,3      NA    1.44    1.51    1.18
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    0.99
         1,2    0.14    0.03    0.24    0.99
         2,2    0.65    1.21    0.36    1.52

# LCMMNP weight and DP update

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
      - R: 500 
      - B: 250 
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
      
      DP-based update: TRUE 
      Weight-based update: TRUE 
      - Initial classes: 7 
      - Maximum classes: 10 
      - Updating buffer: 100 
      - Minimum class weight: 0.1 
      - Maximum class weight: 0.9 
      - Mimumum class distance: 0.1 
      
      Parameter statistics:
                true    mean      sd      R^
       s
                                            
           1    0.92    0.82    0.05    3.53
           2    0.08    0.08    0.02    1.86
           3      NA    0.04    0.02    2.58
           4      NA    0.03    0.01    1.64
           5      NA    0.02    0.01    4.19
           6      NA    0.01    0.00    3.09
           7      NA    0.01    0.00    3.39
      
       b
                                            
         1.1    1.43    1.90    0.29    1.56
         1.2    1.61    1.88    0.34    2.13
         1.3    0.60    0.81    0.26    2.19
         2.1    0.48    0.81    0.88    1.12
         2.2   -1.55   -0.15    2.05    6.17
         2.3   -1.07   -0.90    1.72    5.82
         3.1      NA    1.05    1.43    1.54
         3.2      NA   -0.75    1.45    1.94
         3.3      NA   -1.12    1.48    2.37
         4.1      NA    0.54    1.27    1.06
         4.2      NA    0.75    2.11    1.36
         4.3      NA   -0.30    1.32    0.99
         5.1      NA    0.33    1.09    1.00
         5.2      NA   -0.54    1.50    1.00
         5.3      NA   -0.90    1.62    1.00
         6.1      NA    0.10    0.94    1.00
         6.2      NA    0.05    1.15    1.02
         6.3      NA   -0.07    1.10    1.00
         7.1      NA    0.17    1.19    0.99
         7.2      NA   -0.18    1.26    1.02
         7.3      NA   -0.18    1.21    1.00
      
       Omega
                                            
       1.1,1    0.20    0.71    0.35    2.35
       1.1,2   -0.31   -0.65    0.27    1.45
       1.1,3   -0.08    0.04    0.11    1.12
       1.2,2    1.29    1.47    0.43    1.33
       1.2,3   -0.04   -0.05    0.18    0.99
       1.3,3    0.11    0.34    0.16    1.51
       2.1,1    0.45    1.66    1.41    1.10
       2.1,2   -0.09   -0.30    0.99    0.99
       2.1,3    0.15    0.44    1.09    1.35
       2.2,2    0.36    2.17    2.90    1.03
       2.2,3    0.02    0.21    1.13    1.04
       2.3,3    0.07    1.81    1.82    1.32
       3.1,1      NA    1.94    3.16    1.10
       3.1,2      NA    0.10    1.51    1.05
       3.1,3      NA    0.32    1.60    1.04
       3.2,2      NA    1.92    2.26    1.23
       3.2,3      NA    0.12    1.80    1.02
       3.3,3      NA    1.97    2.40    1.39
       4.1,1      NA    2.46    2.91    1.14
       4.1,2      NA   -0.18    1.70    1.01
       4.1,3      NA    0.02    1.52    1.02
       4.2,2      NA    2.13    2.65    1.11
       4.2,3      NA   -0.03    1.85    0.99
       4.3,3      NA    2.06    2.29    1.08
       5.1,1      NA    2.25    3.55    1.11
       5.1,2      NA    0.27    2.34    1.00
       5.1,3      NA    0.28    1.88    0.99
       5.2,2      NA    2.08    2.41    1.03
       5.2,3      NA   -0.02    1.50    0.99
       5.3,3      NA    2.31    3.37    1.18
       6.1,1      NA    2.28    3.45    1.07
       6.1,2      NA   -0.17    2.57    0.99
       6.1,3      NA   -0.00    2.95    1.00
       6.2,2      NA    2.24    3.32    1.07
       6.2,3      NA    0.20    2.53    0.99
       6.3,3      NA    2.60    4.68    0.99
       7.1,1      NA    2.25    3.21    1.12
       7.1,2      NA    0.09    1.90    0.99
       7.1,3      NA   -0.20    2.41    0.99
       7.2,2      NA    2.03    4.00    1.01
       7.2,3      NA    0.07    1.58    0.99
       7.3,3      NA    2.13    3.53    1.09
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    0.99
         1,2    0.14    0.18    0.26    2.36
         2,2    0.65    1.65    0.57    2.85

