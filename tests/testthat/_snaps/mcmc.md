# Gibbs sampling works

    Code
      print(model)
    Output
      Probit model 'choice ~ a | b | c'.

---

    Code
      summary(model)
    Output
      Probit model
      choice ~ a | b | c 
      R: 2000 
      B: 1000 
      Q: 1 
      
      Normalization
      Level: Utility differences with respect to alternative 2.
      Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Gibbs sample statistics
                true    mean      sd      R^
       alpha
                                            
           1   -0.49   -1.10    0.39    1.00
           2   -0.28   -0.09    0.35    1.00
           3    0.14    0.45    0.36    1.00
           4    0.85    1.85    0.73    1.00
           5   -0.64   -2.33    0.75    1.02
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00

---

    Code
      print(coef(model))
    Output
               Estimate   (sd)
      1     a     -1.10 (0.39)
      2   b_A     -0.09 (0.35)
      3 ASC_A      0.45 (0.36)
      4   c_A      1.85 (0.73)
      5   c_B     -2.33 (0.75)

# consistency of MNP

    Code
      summary(model)
    Output
      Probit model
      choice ~ a | b | c 
      R: 10000 
      B: 5000 
      Q: 1 
      
      Normalization
      Level: Utility differences with respect to alternative 3.
      Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Gibbs sample statistics
                true    mean      sd      R^
       alpha
                                            
           1   -1.12   -1.03    0.04    1.00
           2   -0.64   -0.63    0.04    1.00
           3    0.32    0.28    0.06    1.00
           4    1.92    1.78    0.06    1.00
           5   -1.44   -1.32    0.13    1.00
           6    1.92    1.78    0.07    1.00
           7    2.15    1.96    0.11    1.01
           8    0.80    0.77    0.04    1.00
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    1.59    1.50    0.10    1.00
         2,2    4.04    3.56    0.32    1.00

# consistency of MMNP

    Code
      summary(model)
    Output
      Probit model
      choice ~ a | b | c 
      R: 10000 
      B: 5000 
      Q: 1 
      
      Normalization
      Level: Utility differences with respect to alternative 3.
      Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.
      
      Latent classes
      C = 1 
      
      Gibbs sample statistics
                true    mean      sd      R^
       s
                                            
           1    1.00    1.00    0.00     NaN
      
       b
                                            
         1.1    1.10    1.32    0.18    1.00
         1.2   -0.82   -0.96    0.21    1.00
         1.3    1.10    1.25    0.24    1.00
         1.4    1.23    1.29    0.17    1.00
         1.5    0.46    0.52    0.13    1.00
         1.6    0.37    0.34    0.13    1.00
         1.7   -1.19   -1.31    0.16    1.00
         1.8   -0.82   -1.05    0.20    1.00
      
       Omega
                                            
       1.1,1    0.82    1.01    0.29    1.01
       1.1,2    0.39    0.57    0.23    1.00
       1.1,3    0.34    0.37    0.22    1.01
       1.1,4    0.25    0.12    0.16    1.00
       1.1,5    0.38    0.38    0.17    1.00
       1.1,6    0.32    0.25    0.15    1.00
       1.1,7    0.03   -0.02    0.09    1.00
       1.1,8   -0.82   -0.91    0.29    1.00
       1.2,2    2.03    2.61    0.72    1.00
       1.2,3    0.55    0.26    0.30    1.00
       1.2,4    0.08    0.06    0.22    1.00
       1.2,5    0.08    0.29    0.18    1.00
       1.2,6   -0.76   -1.02    0.34    1.00
       1.2,7   -0.28   -0.44    0.17    1.01
       1.2,8   -0.13    0.08    0.30    1.04
       1.3,3    1.68    2.25    0.69    1.00
       1.3,4    0.84    1.10    0.35    1.00
       1.3,5    0.08    0.28    0.18    1.01
       1.3,6    0.16    0.32    0.23    1.00
       1.3,7   -0.08   -0.14    0.15    1.00
       1.3,8   -1.05   -1.32    0.45    1.00
       1.4,4    1.12    1.26    0.37    1.03
       1.4,5   -0.07   -0.10    0.14    1.02
       1.4,6    0.08   -0.05    0.15    1.00
       1.4,7   -0.02   -0.08    0.11    1.00
       1.4,8   -0.23   -0.29    0.25    1.00
       1.5,5    0.50    0.63    0.21    1.01
       1.5,6    0.41    0.35    0.16    1.01
       1.5,7   -0.00   -0.06    0.08    1.00
       1.5,8   -0.52   -0.62    0.23    1.00
       1.6,6    0.96    1.22    0.37    1.00
       1.6,7    0.27    0.24    0.12    1.02
       1.6,8   -0.58   -0.77    0.28    1.01
       1.7,7    0.29    0.39    0.14    1.03
       1.7,8   -0.09   -0.15    0.14    1.01
       1.8,8    1.83    2.28    0.60    1.00
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.13    0.22    0.23    1.00
         2,2    0.67    1.08    0.21    1.02

