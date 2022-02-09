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
                                            
           1   -0.80   -1.49    0.58    1.00
           2   -0.46   -1.20    0.48    1.01
           3    0.23    0.39    0.22    1.00
           4    1.37    1.98    0.79    1.13
           5   -1.03   -1.62    0.66    1.01
      
       s
                                            
           1    0.68    0.65    0.09    1.09
           2    0.32    0.35    0.09    1.09
      
       b
                                            
         1.1   -1.49   -2.46    0.94    1.00
         1.2   -1.03   -1.29    0.66    1.11
         1.3   -1.09   -1.29    0.66    1.08
         2.1    0.63    0.56    0.54    1.23
         2.2   -0.40   -0.35    0.48    1.22
         2.3    0.92    1.08    0.68    1.21
      
       Omega
                                            
       1.1,1    0.65    1.53    1.52    1.05
       1.1,2   -0.53   -0.82    1.32    1.01
       1.1,3   -0.13   -0.91    1.33    1.00
       1.2,2    0.63    1.54    1.95    1.00
       1.2,3    0.03    0.86    1.36    1.01
       1.3,3    0.10    1.37    1.54    1.00
       2.1,1    0.41    0.89    0.95    1.01
       2.1,2   -0.08   -0.08    0.48    1.03
       2.1,3    0.14    0.36    0.71    1.00
       2.2,2    0.34    1.00    1.15    1.00
       2.2,3    0.02    0.20    1.06    1.18
       2.3,3    0.06    1.52    1.69    1.00
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.95    0.55    0.30    1.00
         2,2    1.12    1.03    0.74    1.10

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
                                            
           1    0.92    0.62    0.10    1.08
           2    0.08    0.38    0.10    1.08
      
       b
                                            
         1.1    1.43    1.91    0.45    1.14
         1.2    1.61    1.61    0.81    4.26
         1.3    0.60    0.40    0.35    1.77
         2.1    0.48    2.23    0.65    2.18
         2.2   -1.55    1.98    0.97    3.18
         2.3   -1.07    0.70    0.47    1.36
      
       Omega
                                            
       1.1,1    0.20    1.32    0.63    1.11
       1.1,2   -0.31    1.04    0.81    1.14
       1.1,3   -0.08    1.00    0.58    1.12
       1.2,2    1.29    5.00    3.61    2.44
       1.2,3   -0.04    2.12    1.54    1.84
       1.3,3    0.11    1.77    1.00    1.52
       2.1,1    0.45    1.23    0.82    1.02
       2.1,2   -0.09   -0.58    0.56    1.15
       2.1,3    0.15    0.50    0.85    1.59
       2.2,2    0.36    1.47    0.86    1.00
       2.2,3    0.02   -0.35    0.71    1.92
       2.3,3    0.07    1.07    0.96    1.31
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.14   -0.37    0.41    1.31
         2,2    0.65    1.45    0.48    1.08

