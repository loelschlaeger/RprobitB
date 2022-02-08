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
                                            
           1    1.00   -0.55    0.37    1.00
           2    2.00    0.80    0.37    1.50
           3    3.00    2.30    0.83    1.20
           4    4.00    4.33    1.39    1.20
           5    5.00    2.09    0.75    1.24
      
       s
                                            
           1    1.00    1.00    0.00     NaN
      
       b
                                            
         1.1    1.00    0.13    0.03    1.03
         1.2    2.00   -0.34    0.08    1.03
         1.3    3.00    0.80    0.18    1.03
      
       Omega
                                            
       1.1,1    1.00    1.39    0.63    1.02
       1.1,2    0.00    0.97    0.44    1.02
       1.1,3    0.00    0.32    0.15    1.02
       1.2,2    1.00    1.27    0.58    1.02
       1.2,3    0.00    0.26    0.12    1.02
       1.3,3    1.00    0.72    0.33    1.02
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.00   -0.08    0.27    1.17
         2,2    1.00    0.63    0.40    1.08

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
                                            
           1   -0.80   -1.13    0.27    1.14
           2   -0.46   -1.06    0.30    1.04
           3    0.23    0.44    0.23    1.17
           4    1.37    1.83    0.39    1.41
           5   -1.03   -1.27    0.30    2.02
      
       s
                                            
           1    0.68    0.54    0.00     NaN
           2    0.32    0.46    0.00     NaN
      
       b
                                            
         1.1   -1.49   -1.00    0.21    1.45
         1.2   -1.03    0.07    0.01    1.45
         1.3   -1.09   -0.15    0.03    1.45
         2.1    0.63    0.29    0.06    1.45
         2.2   -0.40   -1.27    0.26    1.45
         2.3    0.92   -0.77    0.16    1.45
      
       Omega
                                            
       1.1,1    0.65    0.57    0.23    1.42
       1.1,2   -0.53    0.07    0.03    1.42
       1.1,3   -0.13    0.34    0.14    1.42
       1.2,2    0.63    0.34    0.14    1.42
       1.2,3    0.03   -0.08    0.03    1.42
       1.3,3    0.10    1.46    0.59    1.42
       2.1,1    0.41    4.43    1.79    1.42
       2.1,2   -0.08    2.00    0.81    1.42
       2.1,3    0.14    4.11    1.66    1.42
       2.2,2    0.34    1.86    0.75    1.42
       2.2,3    0.02    2.10    0.85    1.42
       2.3,3    0.06    4.99    2.01    1.42
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.95    0.49    0.29    1.02
         2,2    1.12    1.43    0.67    1.66

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
      
      Number of latent classes: 8 
      
      Parameter statistics:
                true    mean      sd      R^
       alpha
                                            
           1   -0.83   -0.97    0.14    1.00
           2   -0.48   -0.54    0.12    1.02
           3    0.24    0.11    0.10    1.03
           4    1.43    1.53    0.21    1.00
           5   -1.07   -1.25    0.16    1.00
      
       s
                                            
           1    0.68    0.40    0.00     NaN
           2    0.32    0.27    0.00     NaN
           3      NA    0.09    0.00     NaN
           4      NA    0.09    0.00     NaN
           5      NA    0.06    0.00     NaN
           6      NA    0.05    0.00     NaN
           7      NA    0.03    0.00     NaN
           8      NA    0.01    0.00     NaN
      
       b
                                            
         1.1   -1.55   -1.87    0.24    1.00
         1.2   -1.07   -0.81    0.11    1.00
         1.3   -1.13   -1.14    0.15    1.00
         2.1    0.65    0.32    0.04    1.00
         2.2   -0.42   -0.17    0.02    1.00
         2.3    0.95    0.30    0.04    1.00
         3.1      NA   -1.13    0.15    1.00
         3.2      NA   -0.04    0.00    1.00
         3.3      NA    0.04    0.01    1.00
         4.1      NA   -0.54    0.07    1.00
         4.2      NA   -0.68    0.09    1.00
         4.3      NA   -0.47    0.06    1.00
         5.1      NA   -0.04    0.01    1.00
         5.2      NA   -1.01    0.13    1.00
         5.3      NA    0.43    0.06    1.00
         6.1      NA    1.10    0.14    1.00
         6.2      NA   -0.73    0.09    1.00
         6.3      NA    0.64    0.08    1.00
         7.1      NA    0.10    0.01    1.00
         7.2      NA   -0.14    0.02    1.00
         7.3      NA    0.40    0.05    1.00
         8.1      NA    0.10    0.01    1.00
         8.2      NA   -0.32    0.04    1.00
         8.3      NA    0.40    0.05    1.00
      
       Omega
                                            
       1.1,1    0.70    0.34    0.09    1.00
       1.1,2   -0.57   -0.20    0.05    1.00
       1.1,3   -0.14   -0.07    0.02    1.00
       1.2,2    0.68    0.74    0.18    1.00
       1.2,3    0.03   -0.12    0.03    1.00
       1.3,3    0.11    0.65    0.16    1.00
       2.1,1    0.45    0.41    0.10    1.00
       2.1,2   -0.09    0.12    0.03    1.00
       2.1,3    0.15    0.28    0.07    1.00
       2.2,2    0.36    0.55    0.14    1.00
       2.2,3    0.02    0.14    0.04    1.00
       2.3,3    0.07    0.67    0.17    1.00
       3.1,1      NA    2.26    0.56    1.00
       3.1,2      NA   -1.54    0.39    1.00
       3.1,3      NA   -0.02    0.01    1.00
       3.2,2      NA    1.72    0.43    1.00
       3.2,3      NA    0.04    0.01    1.00
       3.3,3      NA    0.34    0.08    1.00
       4.1,1      NA    1.16    0.29    1.00
       4.1,2      NA    0.57    0.14    1.00
       4.1,3      NA    1.31    0.33    1.00
       4.2,2      NA    0.86    0.22    1.00
       4.2,3      NA    0.79    0.20    1.00
       4.3,3      NA    2.14    0.53    1.00
       5.1,1      NA    1.42    0.36    1.00
       5.1,2      NA   -0.09    0.02    1.00
       5.1,3      NA   -0.92    0.23    1.00
       5.2,2      NA    1.51    0.38    1.00
       5.2,3      NA    0.30    0.08    1.00
       5.3,3      NA    1.93    0.48    1.00
       6.1,1      NA    0.30    0.08    1.00
       6.1,2      NA   -0.27    0.07    1.00
       6.1,3      NA    0.12    0.03    1.00
       6.2,2      NA    0.72    0.18    1.00
       6.2,3      NA    0.10    0.03    1.00
       6.3,3      NA    0.63    0.16    1.00
       7.1,1      NA    0.68    0.17    1.00
       7.1,2      NA   -0.00    0.00    1.00
       7.1,3      NA    0.82    0.21    1.00
       7.2,2      NA    4.17    1.04    1.00
       7.2,3      NA    0.36    0.09    1.00
       7.3,3      NA    2.34    0.59    1.00
       8.1,1      NA    0.53    0.13    1.00
       8.1,2      NA    0.16    0.04    1.00
       8.1,3      NA   -0.03    0.01    1.00
       8.2,2      NA    0.58    0.14    1.00
       8.2,3      NA   -0.06    0.01    1.00
       8.3,3      NA    0.44    0.11    1.00
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.14    0.04    0.21    1.06
         2,2    0.65    1.04    0.29    1.05

