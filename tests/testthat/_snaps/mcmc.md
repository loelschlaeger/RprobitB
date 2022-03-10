# P

    Code
      print(model)
    Output
      Probit model 'choice ~ cost | income | time'.

---

    Code
      summary(model)
    Output
      Probit model
      choice ~ cost | income | time 
      R: 2000 
      B: 1000 
      Q: 1 
      
      Normalization
      Level: Utility differences with respect to alternative 2.
      Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Gibbs sample statistics
                true    mean      sd      R^
       alpha
                                            
           1    1.00   -0.08    0.29    1.00
           2    2.00    1.62    0.68    1.00
           3    3.00    1.43    0.47    1.00
           4    4.00    2.57    0.74    1.00
           5    5.00    2.94    0.85    1.00
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00

---

    Code
      print(coef(model))
    Output
                    Estimate   (sd)
      1       cost     -0.08 (0.29)
      2 income_bus      1.62 (0.68)
      3    ASC_bus      1.43 (0.47)
      4   time_bus      2.57 (0.74)
      5   time_car      2.94 (0.85)

# MNP

    Code
      print(model)
    Output
      Probit model 'choice ~ cost | income | time'.

---

    Code
      summary(model)
    Output
      Probit model
      choice ~ cost | income | time 
      R: 1000 
      B: 500 
      Q: 1 
      
      Normalization
      Level: Utility differences with respect to alternative 3.
      Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Gibbs sample statistics
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

---

    Code
      print(coef(model))
    Output
                    Estimate   (sd)
      1       cost      0.88 (0.39)
      2 income_bus      0.91 (0.58)
      3 income_car      0.59 (0.59)
      4    ASC_bus      2.53 (0.99)
      5    ASC_car      2.60 (0.96)
      6   time_bus      3.00 (0.98)
      7   time_car      2.48 (0.87)
      8 time_train      4.43 (1.40)

# MMNP

    Code
      print(model)
    Output
      Probit model 'choice ~ cost | income | time'.

---

    Code
      summary(model)
    Output
      Probit model
      choice ~ cost | income | time 
      R: 1000 
      B: 500 
      Q: 1 
      
      Normalization
      Level: Utility differences with respect to alternative 3.
      Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Latent classes
      C = 1 
      
      Gibbs sample statistics
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

---

    Code
      print(coef(model))
    Output
                    Estimate   (sd) Variance   (sd)
      1 income_bus     -0.13 (0.37)       NA   (NA)
      2 income_car      0.77 (0.52)       NA   (NA)
      3   time_bus      2.13 (1.26)       NA   (NA)
      4   time_car      3.96 (2.54)       NA   (NA)
      5 time_train      2.37 (1.55)       NA   (NA)
      6    cost [1]     0.92 (0.80)     1.79 (2.43)
      7 ASC_bus [1]     0.82 (0.91)     1.30 (2.06)
      8 ASC_car [1]     1.06 (0.80)     1.35 (2.68)

# LCMMNP

    Code
      print(model)
    Output
      Probit model 'choice ~ cost | income | time'.

---

    Code
      summary(model)
    Output
      Probit model
      choice ~ cost | income | time 
      R: 500 
      B: 250 
      Q: 1 
      
      Normalization
      Level: Utility differences with respect to alternative 3.
      Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Latent classes
      C = 2 
      
      Gibbs sample statistics
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

---

    Code
      print(coef(model))
    Output
                     Estimate   (sd) Variance   (sd)
      1  income_bus     -1.40 (0.48)       NA   (NA)
      2  income_car     -1.11 (0.40)       NA   (NA)
      3    time_bus      0.29 (0.17)       NA   (NA)
      4    time_car      1.77 (0.50)       NA   (NA)
      5  time_train     -1.47 (0.38)       NA   (NA)
      6     cost [1]    -2.11 (0.80)     1.84 (1.43)
      7     cost [2]     0.19 (0.37)     0.71 (0.64)
      8  ASC_bus [1]    -1.29 (0.56)     0.73 (0.70)
      9  ASC_bus [2]    -0.11 (0.48)     0.71 (0.58)
      10 ASC_car [1]    -1.56 (0.72)     0.88 (0.66)
      11 ASC_car [2]     0.84 (0.49)     1.43 (2.24)

# LCMMNP weight update

    Code
      print(model)
    Output
      Probit model 'choice ~ cost'.

---

    Code
      summary(model)
    Output
      Probit model
      choice ~ cost 
      R: 500 
      B: 250 
      Q: 1 
      
      Normalization
      Level: Utility differences with respect to alternative 3.
      Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Latent classes
      DP-based update: FALSE 
      Weight-based update: TRUE 
      Initial classes: 6 
      Maximum classes: 10 
      Updating buffer: 100 
      Minimum class weight: 0.1 
      Maximum class weight: 0.9 
      Mimumum class distance: 0.1 
      
      Gibbs sample statistics
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

---

    Code
      print(coef(model))
    Output
                     Estimate   (sd) Variance   (sd)
      1     cost [1]     1.64 (0.29)     0.39 (0.18)
      2     cost [2]     1.89 (0.35)     0.44 (0.25)
      3     cost [3]     1.11 (1.05)     0.97 (1.04)
      4     cost [4]     0.48 (0.83)     1.16 (0.97)
      5     cost [5]     0.38 (0.93)     1.21 (1.38)
      6     cost [6]     0.13 (0.76)     1.25 (2.07)
      7  ASC_bus [1]     2.71 (0.54)     1.85 (0.75)
      8  ASC_bus [2]     1.05 (0.48)     0.53 (0.33)
      9  ASC_bus [3]    -0.71 (1.15)     1.18 (0.94)
      10 ASC_bus [4]    -1.34 (1.09)     1.48 (2.03)
      11 ASC_bus [5]     0.08 (0.99)     1.45 (2.84)
      12 ASC_bus [6]     0.14 (0.89)     0.99 (1.33)
      13 ASC_car [1]     0.78 (0.22)     0.37 (0.22)
      14 ASC_car [2]     0.79 (0.28)     0.30 (0.16)
      15 ASC_car [3]    -0.81 (1.20)     1.31 (2.01)
      16 ASC_car [4]    -1.69 (0.97)     1.63 (1.71)
      17 ASC_car [5]    -0.16 (1.00)     1.36 (1.56)
      18 ASC_car [6]    -0.01 (0.80)     1.11 (1.68)

# LCMMNP DP update

    Code
      print(model)
    Output
      Probit model 'choice ~ cost'.

---

    Code
      summary(model)
    Output
      Probit model
      choice ~ cost 
      R: 500 
      B: 250 
      Q: 1 
      
      Normalization
      Level: Utility differences with respect to alternative 3.
      Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Latent classes
      DP-based update: TRUE 
      Weight-based update: FALSE 
      Initial classes: 4 
      Maximum classes: 10 
      
      Gibbs sample statistics
                true    mean      sd      R^
       s
                                            
           1    0.92    0.74    0.15    5.15
           2    0.08    0.16    0.11    3.99
           3      NA    0.07    0.04    4.15
           4      NA    0.03    0.02    1.11
      
       b
                                            
         1.1    1.43    1.83    0.37    1.88
         1.2    1.61    1.98    0.42    1.31
         1.3    0.60    0.54    0.26    1.44
         2.1    0.48    1.74    1.06    1.27
         2.2   -1.55    0.79    1.51    1.10
         2.3   -1.07   -0.09    1.26    2.24
         3.1      NA    1.13    0.93    1.30
         3.2      NA   -0.92    1.18    0.99
         3.3      NA   -1.20    0.92    1.00
         4.1      NA    0.52    1.06    1.15
         4.2      NA   -0.68    1.28    0.99
         4.3      NA   -0.79    1.57    1.36
      
       Omega
                                            
       1.1,1    0.20    0.72    0.37    2.85
       1.1,2   -0.31   -0.49    0.27    1.66
       1.1,3   -0.08    0.20    0.12    1.05
       1.2,2    1.29    1.08    0.49    1.14
       1.2,3   -0.04   -0.34    0.25    1.03
       1.3,3    0.11    0.44    0.21    1.00
       2.1,1    0.45    1.45    3.42    1.09
       2.1,2   -0.09   -0.09    1.81    1.00
       2.1,3    0.15    0.06    1.78    1.02
       2.2,2    0.36    1.29    2.27    1.19
       2.2,3    0.02    0.14    1.29    1.02
       2.3,3    0.07    1.06    1.47    1.52
       3.1,1      NA    2.04    4.53    1.07
       3.1,2      NA   -0.06    2.34    1.04
       3.1,3      NA    0.11    4.71    1.07
       3.2,2      NA    1.69    4.02    1.11
       3.2,3      NA    0.15    2.74    1.00
       3.3,3      NA    2.32    5.18    1.00
       4.1,1      NA    1.46    1.60    1.01
       4.1,2      NA    0.22    1.26    1.04
       4.1,3      NA    0.13    1.09    1.00
       4.2,2      NA    1.62    1.86    0.99
       4.2,3      NA    0.19    1.39    1.00
       4.3,3      NA    1.46    1.78    1.03
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    0.99
         1,2    0.14   -0.27    0.24    1.16
         2,2    0.65    1.33    0.34    1.06

---

    Code
      print(coef(model))
    Output
                     Estimate   (sd) Variance   (sd)
      1     cost [1]     1.83 (0.37)     0.72 (0.37)
      2     cost [2]     1.74 (1.06)     1.45 (3.42)
      3     cost [3]     1.13 (0.93)     2.04 (4.53)
      4     cost [4]     0.52 (1.06)     1.46 (1.60)
      5  ASC_bus [1]     1.98 (0.42)     1.08 (0.49)
      6  ASC_bus [2]     0.79 (1.51)     1.29 (2.27)
      7  ASC_bus [3]    -0.92 (1.18)     1.69 (4.02)
      8  ASC_bus [4]    -0.68 (1.28)     1.62 (1.86)
      9  ASC_car [1]     0.54 (0.26)     0.44 (0.21)
      10 ASC_car [2]    -0.09 (1.26)     1.06 (1.47)
      11 ASC_car [3]    -1.20 (0.92)     2.32 (5.18)
      12 ASC_car [4]    -0.79 (1.57)     1.46 (1.78)

# LCMMNP weight and DP update

    Code
      print(model)
    Output
      Probit model 'choice ~ cost'.

---

    Code
      summary(model)
    Output
      Probit model
      choice ~ cost 
      R: 500 
      B: 250 
      Q: 1 
      
      Normalization
      Level: Utility differences with respect to alternative 3.
      Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Latent classes
      DP-based update: TRUE 
      Weight-based update: TRUE 
      Initial classes: 4 
      Maximum classes: 10 
      Updating buffer: 100 
      Minimum class weight: 0.1 
      Maximum class weight: 0.9 
      Mimumum class distance: 0.1 
      
      Gibbs sample statistics
                true    mean      sd      R^
       s
                                            
           1    0.92    0.83    0.04    1.00
           2    0.08    0.09    0.03    1.05
           3      NA    0.05    0.02    0.99
           4      NA    0.03    0.02    1.51
      
       b
                                            
         1.1    1.43    1.77    0.33    2.05
         1.2    1.61    1.78    0.32    1.00
         1.3    0.60    0.67    0.28    1.07
         2.1    0.48    0.38    0.48    1.06
         2.2   -1.55   -0.91    1.59    1.48
         2.3   -1.07   -2.02    0.70    1.11
         3.1      NA    1.17    0.98    1.05
         3.2      NA    1.22    3.04    1.02
         3.3      NA   -0.25    1.75    2.37
         4.1      NA    1.01    1.51    1.14
         4.2      NA    0.09    1.50    1.01
         4.3      NA    0.19    1.30    1.07
      
       Omega
                                            
       1.1,1    0.20    0.39    0.28    2.05
       1.1,2   -0.31   -0.24    0.14    1.11
       1.1,3   -0.08    0.04    0.06    1.06
       1.2,2    1.29    1.42    0.51    1.00
       1.2,3   -0.04   -0.04    0.14    1.08
       1.3,3    0.11    0.30    0.13    2.16
       2.1,1    0.45    1.38    1.02    1.28
       2.1,2   -0.09    0.13    1.66    1.10
       2.1,3    0.15    0.27    0.67    1.12
       2.2,2    0.36    4.15    7.40    1.44
       2.2,3    0.02    0.83    2.22    1.44
       2.3,3    0.07    1.46    1.49    1.35
       3.1,1      NA    1.94    2.84    1.04
       3.1,2      NA   -0.19    1.76    1.14
       3.1,3      NA   -0.58    1.58    0.99
       3.2,2      NA    3.14    3.81    1.09
       3.2,3      NA    0.41    2.08    1.18
       3.3,3      NA    2.08    2.03    0.99
       4.1,1      NA    2.21    2.95    1.03
       4.1,2      NA    0.11    1.70    0.99
       4.1,3      NA    0.11    1.38    1.00
       4.2,2      NA    2.37    4.26    1.01
       4.2,3      NA    0.26    2.16    0.99
       4.3,3      NA    1.73    2.23    1.09
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    0.99
         1,2    0.14   -0.15    0.21    1.29
         2,2    0.65    1.08    0.46    1.31

---

    Code
      print(coef(model))
    Output
                     Estimate   (sd) Variance   (sd)
      1     cost [1]     1.77 (0.33)     0.39 (0.28)
      2     cost [2]     0.38 (0.48)     1.38 (1.02)
      3     cost [3]     1.17 (0.98)     1.94 (2.84)
      4     cost [4]     1.01 (1.51)     2.21 (2.95)
      5  ASC_bus [1]     1.78 (0.32)     1.42 (0.51)
      6  ASC_bus [2]    -0.91 (1.59)     4.15 (7.40)
      7  ASC_bus [3]     1.22 (3.04)     3.14 (3.81)
      8  ASC_bus [4]     0.09 (1.50)     2.37 (4.26)
      9  ASC_car [1]     0.67 (0.28)     0.30 (0.13)
      10 ASC_car [2]    -2.02 (0.70)     1.46 (1.49)
      11 ASC_car [3]    -0.25 (1.75)     2.08 (2.03)
      12 ASC_car [4]     0.19 (1.30)     1.73 (2.23)

