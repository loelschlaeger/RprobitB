# setting prior parameter works

    Code
      prior
    Output
      $eta
      [1] 0
      
      $Psi
           [,1]
      [1,]    1
      
      $delta
      [1] 1
      
      $xi
      [1] 0 0
      
      $D
           [,1] [,2]
      [1,]    1    0
      [2,]    0    1
      
      $nu
      [1] 4
      
      $Theta
           [,1] [,2]
      [1,]    1    0
      [2,]    0    1
      
      $kappa
      [1] 4
      
      $E
           [,1] [,2]
      [1,]    1    0
      [2,]    0    1
      
      $zeta
      [1] NA
      
      $Z
      [1] NA
      
      attr(,"class")
      [1] "RprobitB_prior" "list"          

# setting of initial Gibbs values works

    Code
      init
    Output
      $alpha0
      [1] 0
      
      $z0
      [1] 1 1
      
      $m0
      [1] 1 1
      
      $b0
           [,1] [,2]
      [1,]    0    0
      [2,]    0    0
      
      $Omega0
           [,1] [,2]
      [1,]    1    1
      [2,]    0    0
      [3,]    0    0
      [4,]    1    1
      
      $beta0
           [,1] [,2]
      [1,]    0    0
      [2,]    0    0
      
      $U0
           [,1] [,2] [,3] [,4] [,5] [,6]
      [1,]    0    0    0    0    0    0
      [2,]    0    0    0    0    0    0
      
      $Sigma0
           [,1] [,2]
      [1,]    1    0
      [2,]    0    1
      
      $d0
      [1] NA
      

# RprobitB_latent_class setting works

    Code
      RprobitB_latent_classes(list(C = 2))
    Output
      Latent classes
      C = 2 

---

    Code
      (out <- RprobitB_latent_classes(list(wb_update = TRUE, dp_update = TRUE)))
    Output
      Latent classes
      Dirichlet process update: TRUE 
      Weight-based update: TRUE 
      Initial classes: 1 
      Maximum classes: 10 
      Updating buffer: 50 
      Minimum class weight: 0.01 
      Maximum class weight: 0.7 
      Mimumum class distance: 0.1 

---

    Code
      str(out)
    Output
      List of 10
       $ wb_update   : logi TRUE
       $ dp_update   : logi TRUE
       $ C           : num 1
       $ Cmax        : num 10
       $ buffer      : num 50
       $ epsmin      : num 0.01
       $ epsmax      : num 0.7
       $ deltamin    : num 0.1
       $ deltashift  : num 0.5
       $ class_update: logi TRUE
       - attr(*, "class")= chr "RprobitB_latent_classes"

# building of RprobitB_normalization works

    Code
      RprobitB_normalization(level = "B", scale = "price := -1", form = form, re = re,
        alternatives = alternatives, base = "B")
    Output
      Level: Utility differences with respect to alternative 'B'.
      Scale: Coefficient of effect 'price' (alpha_1) fixed to -1.

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
      Formula: choice ~ a | b | c 
      R: 2000, B: 1000, Q: 1
      Level: Utility differences with respect to alternative 'B'.
      Scale: Coefficient of the 1. error term variance fixed to 1.
      
      Gibbs sample statistics
                true    mean      sd      R^
       alpha
                                            
           1   -0.94   -0.94    0.06    1.06
           2   -0.54   -0.55    0.06    1.01
           3    0.27    0.33    0.06    1.00
           4    1.62    1.64    0.10    1.09
           5   -1.21   -1.27    0.08    1.15
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00

---

    Code
      print(coef(model))
    Output
               Estimate   (sd)
      1     a     -0.94 (0.06)
      2   b_A     -0.55 (0.06)
      3 ASC_A      0.33 (0.06)
      4   c_A      1.64 (0.10)
      5   c_B     -1.27 (0.08)

# Ordered probit model estimation works

    Code
      print(model)
    Output
      Probit model 'opinion_on_sth ~ age + gender'.

---

    Code
      summary(model)
    Output
      Probit model
      Formula: opinion_on_sth ~ age + gender 
      R: 1000, B: 500, Q: 1
      Level: Fixed first utility threshold to 0.
      Scale: Error term variance fixed to 1.
      
      Gibbs sample statistics
                true    mean      sd      R^
       alpha
                                            
           1   -0.95   -0.76    0.04    1.01
           2   -0.55   -0.61    0.06    1.01
      
       Sigma
                                            
         1,1    1.00    1.00    0.00     NaN
      
       d
                                            
           1    0.91    0.44    0.05    1.15
           2    0.20   -0.46    0.07    1.17
           3    0.90    0.79    0.30    5.12

---

    Code
      print(coef(model))
    Output
                Estimate   (sd)
      1    age     -0.76 (0.04)
      2 gender     -0.61 (0.06)

# Ranked probit model estimation works

    Code
      print(model)
    Output
      Probit model 'product ~ price'.

---

    Code
      summary(model)
    Output
      Probit model
      Formula: product ~ price 
      R: 1000, B: 500, Q: 1
      Level: Utility differences with respect to alternative 'C'.
      Scale: Coefficient of the 1. error term variance fixed to 1.
      
      Gibbs sample statistics
                true    mean      sd      R^
       alpha
                                            
           1   -0.93   -0.91    0.04    1.03
           2   -0.53   -0.43    0.05    1.00
           3    0.26    0.32    0.04    1.03
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.51    0.45    0.05    1.00
         2,2    1.01    0.93    0.10    1.02

---

    Code
      print(coef(model))
    Output
               Estimate   (sd)
      1 price     -0.91 (0.04)
      2 ASC_A     -0.43 (0.05)
      3 ASC_B      0.32 (0.04)

# computation of sufficient statistics works

    Code
      ss
    Output
      $N
      [1] 2
      
      $T
      [1] 1 2
      
      $J
      [1] 3
      
      $P_f
      [1] 3
      
      $P_r
      [1] 2
      
      $Tvec
      [1] 1 2
      
      $csTvec
      [1] 0 1
      
      $W
      $W[[1]]
                  v1 ASC_A ASC_B
      [1,] -1.113883     1     0
      [2,]  1.107852     0     1
      
      $W[[2]]
                   v1 ASC_A ASC_B
      [1,] -0.5546814     1     0
      [2,] -0.4088169     0     1
      
      $W[[3]]
                 v1 ASC_A ASC_B
      [1,] -1.41141     1     0
      [2,] -1.39625     0     1
      
      
      $X
      $X[[1]]
                 v2_A       v2_B
      [1,] -0.3053884  0.0000000
      [2,]  0.0000000 -0.3053884
      
      $X[[2]]
               v2_A     v2_B
      [1,] 1.511781 0.000000
      [2,] 0.000000 1.511781
      
      $X[[3]]
                v2_A      v2_B
      [1,] 0.3898432 0.0000000
      [2,] 0.0000000 0.3898432
      
      
      $y
           [,1] [,2]
      [1,]    1   NA
      [2,]    1    1
      
      $WkW
                 [,1]       [,2]       [,3]       [,4]
       [1,]  3.540485  0.9634269  0.9634269  3.3439801
       [2,] -3.079974  0.0000000 -0.6972149  0.0000000
       [3,]  0.000000 -3.0799742  0.0000000 -0.6972149
       [4,] -3.079974 -0.6972149  0.0000000  0.0000000
       [5,]  3.000000  0.0000000  0.0000000  0.0000000
       [6,]  0.000000  3.0000000  0.0000000  0.0000000
       [7,]  0.000000  0.0000000 -3.0799742 -0.6972149
       [8,]  0.000000  0.0000000  3.0000000  0.0000000
       [9,]  0.000000  0.0000000  0.0000000  3.0000000
      
      $XkX
      $XkX[[1]]
                 [,1]       [,2]       [,3]       [,4]
      [1,] 0.09326207 0.00000000 0.00000000 0.00000000
      [2,] 0.00000000 0.09326207 0.00000000 0.00000000
      [3,] 0.00000000 0.00000000 0.09326207 0.00000000
      [4,] 0.00000000 0.00000000 0.00000000 0.09326207
      
      $XkX[[2]]
              [,1]    [,2]    [,3]    [,4]
      [1,] 2.43746 0.00000 0.00000 0.00000
      [2,] 0.00000 2.43746 0.00000 0.00000
      [3,] 0.00000 0.00000 2.43746 0.00000
      [4,] 0.00000 0.00000 0.00000 2.43746
      
      
      $rdiff
      [1] NA
      

