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
                                            
           1    1.00    0.92    0.34    1.06
           2    2.00    1.19    0.49    1.06
           3    3.00    1.90    0.70    1.28
           4    4.00    2.60    1.00    1.21
           5    5.00    3.22    1.12    1.29
      
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
                                            
           1    0.39    0.46    0.40    1.07
           2    0.78    1.26    0.59    1.05
           3    1.17    1.14    0.70    1.04
           4    1.56    2.46    1.01    1.04
           5    1.95    2.88    1.14    1.03
           6    2.35    2.95    1.07    1.45
           7    2.74    3.36    1.27    1.70
           8    3.13    4.61    1.51    1.25
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.36    0.10    0.50    1.00
         2,2    1.26    1.95    1.55    1.26

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
      
      Latent classes:
      - Number: 1 
      - Update: FALSE 
      
      Parameter statistics:
                true    mean      sd      R^
       alpha
                                            
           1    1.00    0.40    0.48    1.00
           2    2.00    1.62    0.89    1.06
           3    3.00    3.20    1.96    1.06
           4    4.00    3.46    1.94    1.13
           5    5.00    3.42    1.49    1.11
      
       s
                                            
           1    1.00    1.00    0.00     NaN
      
       b
                                            
         1.1    1.00    0.68    0.54    1.05
         1.2    2.00    0.59    0.63    1.00
         1.3    3.00    0.87    0.71    1.03
      
       Omega
                                            
       1.1,1    1.00    1.41    1.66    1.06
       1.1,2    0.00   -0.03    1.13    1.10
       1.1,3    0.00    0.00    0.88    1.01
       1.2,2    1.00    1.44    2.22    1.28
       1.2,3    0.00   -0.15    0.99    1.03
       1.3,3    1.00    1.17    1.34    1.10
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.00    0.09    0.66    1.63
         2,2    1.00    1.56    1.25    1.02

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
      
      Latent classes:
      - Number: 2 
      - Update: FALSE 
      
      Parameter statistics:
                true    mean      sd      R^
       alpha
                                            
           1   -0.80   -0.95    0.29    1.01
           2   -0.46   -0.52    0.31    1.35
           3    0.23    0.68    0.22    1.00
           4    1.37    1.68    0.56    1.02
           5   -1.03   -1.32    0.35    1.00
      
       s
                                            
           1    0.68    0.68    0.10    1.02
           2    0.32    0.32    0.10    1.02
      
       b
                                            
         1.1   -1.49   -1.34    0.61    1.01
         1.2   -1.03   -1.11    0.46    1.04
         1.3   -1.09   -0.83    0.57    1.00
         2.1    0.63    0.15    0.89    1.02
         2.2   -0.40   -0.77    0.43    1.00
         2.3    0.92    0.04    1.08    1.24
      
       Omega
                                            
       1.1,1    0.65    1.16    1.29    1.05
       1.1,2   -0.53    0.10    0.48    1.15
       1.1,3   -0.13    0.43    0.87    1.07
       1.2,2    0.63    0.91    0.93    1.03
       1.2,3    0.03    0.27    0.72    1.25
       1.3,3    0.10    1.07    1.13    1.07
       2.1,1    0.41    1.31    1.23    1.00
       2.1,2   -0.08   -0.03    0.49    1.00
       2.1,3    0.14    0.66    1.51    1.22
       2.2,2    0.34    0.68    0.60    1.02
       2.2,3    0.02    0.08    0.60    1.08
       2.3,3    0.06    1.91    3.53    1.24
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.95    0.31    0.20    1.00
         2,2    1.12    0.49    0.19    1.01

