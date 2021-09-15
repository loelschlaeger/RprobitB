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
                                                                           
           1  -2.52  -2.43   0.17  -3.03  -2.53  -2.43  -2.32   -1.9      1
      
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
                                                                           
           1  -0.94  -0.85   0.06  -1.01  -0.88  -0.85  -0.81  -0.66      1
      
       Sigma
                                                                           
         1,1   1.00   1.00   0.00   1.00   1.00   1.00   1.00   1.00   1.00
         1,2  -0.42  -0.30   0.05  -0.45  -0.34  -0.30  -0.26  -0.13   1.02
         2,2   0.27   0.17   0.04   0.08   0.14   0.16   0.19   0.29   1.00

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
                                                                           
           1  -0.91  -1.51   0.24  -2.17  -1.66  -1.50  -1.34  -0.95   1.08
           2  -0.52  -0.63   0.22  -1.48  -0.79  -0.63  -0.45  -0.20   1.00
      
       s
                                                                           
           1      1      1      0      1      1      1      1      1    NaN
      
       b
                                                                           
         1.1  -1.17   -1.8   0.32  -2.81  -2.02  -1.77  -1.55  -1.04   1.08
         1.2   1.56    1.4   0.42   0.56   1.06   1.37   1.71   2.81   1.00
      
       Omega
                                                                           
       1.1,1   1.92   3.27   1.13   1.12   2.44   3.10   3.88   8.04   1.04
       1.1,2  -0.27  -0.68   0.40  -2.45  -0.90  -0.61  -0.39   0.21   1.01
       1.2,2   0.51   0.79   0.42   0.16   0.49   0.70   0.99   3.21   1.01
      
       Sigma
                                                                           
         1,1   1.00   1.00   0.00   1.00   1.00   1.00   1.00   1.00   1.00
         1,2   1.34   0.36   0.63  -1.24   0.00   0.35   0.75   1.90   1.07
         2,2   3.46   2.61   1.48   0.48   1.35   2.32   3.52   8.61   1.00

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
                                                                           
           1  -0.44  -0.39   0.11  -0.60  -0.48  -0.41  -0.33  -0.01   1.16
           2  -0.25  -0.33   0.21  -1.07  -0.47  -0.33  -0.19   0.14   1.74
      
       s
                                                                           
           1   0.12   0.24   0.13   0.01   0.13   0.23   0.34   0.50      1
           2   0.88   0.76   0.13   0.50   0.66   0.77   0.87   0.99      1
      
       b
                                                                           
         1.1   0.32  -0.70   0.58  -3.78  -1.01  -0.70  -0.38   2.10   1.03
         1.2   0.25  -0.05   0.52  -1.70  -0.37  -0.04   0.27   1.84   1.05
         2.1  -0.82  -0.60   0.16  -1.18  -0.71  -0.60  -0.48  -0.12   1.00
         2.2  -0.57  -0.53   0.23  -1.54  -0.66  -0.52  -0.38   0.29   1.18
      
       Omega
                                                                           
       1.1,1   0.01   0.60   0.58   0.04   0.25   0.44   0.72   4.24   1.00
       1.1,2   0.05  -0.11   0.41  -2.30  -0.24  -0.06   0.04   3.13   1.03
       1.2,2   0.40   0.78   1.06   0.04   0.25   0.52   0.98  18.21   1.04
       2.1,1   0.16   0.28   0.14   0.04   0.18   0.25   0.34   0.88   1.07
       2.1,2  -0.28  -0.07   0.14  -0.75  -0.14  -0.05   0.01   0.42   1.04
       2.2,2   0.51   0.51   0.39   0.03   0.26   0.42   0.65   4.34   1.16
      
       Sigma
                                                                           
         1,1   1.00   1.00   0.00   1.00   1.00   1.00   1.00   1.00   1.00
         1,2  -0.02  -0.28   0.36  -1.19  -0.54  -0.32   0.00   0.55   1.64
         2,2   0.56   0.80   0.56   0.06   0.39   0.67   1.03   3.45   1.33

