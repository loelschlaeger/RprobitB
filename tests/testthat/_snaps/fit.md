# model fitting works

    Code
      summary(m1)
    Output
      Summary of fitted probit model 'choice ~ var | 0' via Bayesian estimation:
      
      MCMC settings:
      - R: 10000 
      - B: 5000 
      - Q: 10 
      
      Normalization:
      - Differenced with respect to alternative 2 (B) 
      - Coefficient of the 1. error term variance in Sigma fixed to 1 
      
      Legend of alternatives (Sigma):
      - 1: A 
      - 2: B 
      
      Covariates with fixed coefficients (alpha):
      - 1: 
      
      Estimates:
               true   mean     sd    min   q.25 median   q.75    max     R^
       alpha
                                                                           
           1  -0.93  -0.87   0.05  -1.03   -0.9  -0.87  -0.83   -0.7      1
      
       Sigma
                                                                           
         1,1      1      1      0      1      1      1      1      1      1

---

    Code
      summary(m2)
    Output
      Summary of fitted probit model 'choice ~ var | 0' via Bayesian estimation:
      
      MCMC settings:
      - R: 10000 
      - B: 5000 
      - Q: 10 
      
      Normalization:
      - Differenced with respect to alternative 3 (C) 
      - Coefficient of the 1. error term variance in Sigma fixed to 1 
      
      Legend of alternatives (Sigma):
      - 1: A 
      - 2: B 
      - 3: C 
      
      Covariates with fixed coefficients (alpha):
      - 1: 
      
      Estimates:
               true   mean     sd    min   q.25 median   q.75    max     R^
       alpha
                                                                           
           1   0.38   0.36   0.03   0.28   0.34   0.35   0.37   0.43      1
      
       Sigma
                                                                           
         1,1   1.00   1.00   0.00   1.00   1.00   1.00   1.00   1.00   1.00
         1,2   0.59   0.60   0.06   0.44   0.56   0.60   0.64   0.77   1.07
         2,2   0.66   0.72   0.10   0.47   0.65   0.71   0.77   1.12   1.02

---

    Code
      summary(m3)
    Output
      Summary of fitted probit model 'choice ~ 0 | var' via Bayesian estimation:
      
      MCMC settings:
      - R: 10000 
      - B: 5000 
      - Q: 10 
      
      Normalization:
      - Differenced with respect to alternative 3 (C) 
      - Coefficient of the 1. error term variance in Sigma fixed to 1 
      
      Legend of alternatives (Sigma):
      - 1: A 
      - 2: B 
      - 3: C 
      
      Covariates with fixed coefficients (alpha):
      - 1: 
      - 2: 
      
      Random effects (b, Omega):
      - 1: 
      - 2: 
      
      Latent classes (s):
      
      Estimates:
               true   mean     sd    min   q.25 median   q.75    max     R^
       alpha
                                                                           
           1   0.69  -0.02   0.31  -0.48  -0.29  -0.07   0.20   0.92   1.19
           2   0.88   0.39   0.22   0.06   0.20   0.35   0.55   1.08   1.19
      
       s
                                                                           
           1      1      1      0      1      1      1      1      1    NaN
      
       b
                                                                           
         1.1  -0.57  -0.24   0.18  -0.83  -0.36  -0.21  -0.11   0.10   1.11
         1.2  -0.79  -0.65   0.19  -1.31  -0.77  -0.63  -0.51  -0.25   1.26
      
       Omega
                                                                           
       1.1,1   0.13   0.15   0.07   0.02   0.10   0.14   0.19   0.46   1.04
       1.1,2   0.09  -0.02   0.07  -0.38  -0.06  -0.03   0.01   0.25   1.01
       1.2,2   0.14   0.22   0.16   0.02   0.11   0.18   0.28   1.38   1.15
      
       Sigma
                                                                           
         1,1   1.00   1.00   0.00   1.00   1.00   1.00   1.00   1.00   1.00
         1,2   0.69  -0.10   0.42  -1.21  -0.40  -0.10   0.23   0.79   1.03
         2,2   0.78   0.51   0.32   0.08   0.29   0.44   0.62   2.04   1.33

---

    Code
      summary(m4)
    Output
      Summary of fitted probit model 'choice ~ 0 | var' via Bayesian estimation:
      
      MCMC settings:
      - R: 10000 
      - B: 5000 
      - Q: 10 
      
      Normalization:
      - Differenced with respect to alternative 3 (C) 
      - Coefficient of the 1. error term variance in Sigma fixed to 1 
      
      Legend of alternatives (Sigma):
      - 1: A 
      - 2: B 
      - 3: C 
      
      Covariates with fixed coefficients (alpha):
      - 1: 
      - 2: 
      
      Random effects (b, Omega):
      - 1: 
      - 2: 
      
      Latent classes (s):
      
      Estimates:
               true   mean     sd    min   q.25 median   q.75    max     R^
       alpha
                                                                           
           1   0.86   0.67   0.28   0.08   0.44   0.66   0.87   1.47   1.01
           2   1.09   0.89   0.25   0.36   0.68   0.88   1.07   1.71   1.02
      
       s
                                                                           
           1   0.04   0.24   0.13    0.0   0.14   0.24   0.34    0.5      1
           2   0.96   0.76   0.13    0.5   0.66   0.76   0.86    1.0      1
      
       b
                                                                           
         1.1  -1.01  -0.15   0.33  -1.14  -0.38  -0.20   0.04   1.22   1.13
         1.2   0.08  -0.34   0.38  -1.85  -0.54  -0.29  -0.08   0.73   1.04
         2.1  -0.08   0.09   0.25  -0.59  -0.10   0.10   0.27   0.70   1.01
         2.2  -1.05  -0.80   0.26  -1.78  -0.96  -0.79  -0.60  -0.27   1.04
      
       Omega
                                                                           
       1.1,1   0.21   0.22   0.25   0.00   0.08   0.16   0.26   2.70   1.00
       1.1,2  -0.35  -0.06   0.15  -1.10  -0.10  -0.04   0.00   0.98   1.00
       1.2,2   0.75   0.25   0.30   0.01   0.09   0.18   0.30   3.63   1.01
       2.1,1   0.07   0.19   0.11   0.02   0.11   0.17   0.25   0.77   1.04
       2.1,2  -0.17  -0.09   0.08  -0.65  -0.13  -0.08  -0.03   0.09   1.05
       2.2,2   0.47   0.23   0.21   0.02   0.09   0.16   0.29   1.60   1.00
      
       Sigma
                                                                           
         1,1   1.00   1.00   0.00   1.00    1.0   1.00   1.00   1.00   1.00
         1,2   0.35   0.29   0.12  -0.05    0.2   0.27   0.38   0.62   1.00
         2,2   0.26   0.19   0.11   0.03    0.1   0.17   0.25   0.67   1.01

