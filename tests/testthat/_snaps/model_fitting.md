# setting prior parameter works

    Code
      prior
    Output
      $mu_alpha_0
      [1] 0
      
      $Sigma_alpha_0
           [,1]
      [1,]   10
      
      $delta
      [1] 1
      
      $mu_b_0
      [1] 0 0
      
      $Sigma_b_0
           [,1] [,2]
      [1,]   10    0
      [2,]    0   10
      
      $n_Omega_0
      [1] 4
      
      $V_Omega_0
           [,1] [,2]
      [1,]    1    0
      [2,]    0    1
      
      $n_Sigma_0
      [1] 4
      
      $V_Sigma_0
           [,1] [,2]
      [1,]    1    0
      [2,]    0    1
      
      $mu_d_0
      [1] NA
      
      $Sigma_d_0
      [1] NA
      
      attr(,"class")
      [1] "RprobitB_prior" "list"          

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
                                            
           1   -0.14   -0.11    0.03    1.00
           2    0.20    0.21    0.04    1.00
           3    0.67    0.70    0.04    1.00
           4   -0.20   -0.19    0.04    1.00
           5   -0.51   -0.51    0.04    1.04
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00

---

    Code
      print(coef(model))
    Output
               Estimate   (sd)
      1     a     -0.11 (0.03)
      2   b_A      0.21 (0.04)
      3 ASC_A      0.70 (0.04)
      4   c_A     -0.19 (0.04)
      5   c_B     -0.51 (0.04)

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
                                            
           1    0.60    0.56    0.04    1.00
           2    1.21    1.09    0.06    1.00
      
       Sigma
                                            
         1,1    1.00    1.00    0.00     NaN
      
       d
                                            
           1    0.00   -0.55    0.06    1.04
           2    1.00    0.41    0.04    1.06
           3    2.00    1.37    0.31    3.29

---

    Code
      print(coef(model))
    Output
                Estimate   (sd)
      1    age      0.56 (0.04)
      2 gender      1.09 (0.06)

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
                                            
           1   -0.68   -0.67    0.03    1.00
           2    0.18    0.19    0.04    1.01
           3    1.05    1.00    0.05    1.00
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.59    0.61    0.04    1.00
         2,2    0.74    0.71    0.07    1.01

---

    Code
      print(coef(model))
    Output
               Estimate   (sd)
      1 price     -0.67 (0.03)
      2 ASC_A      0.19 (0.04)
      3 ASC_B      1.00 (0.05)

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
      [2,]    2    2
      
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
      

