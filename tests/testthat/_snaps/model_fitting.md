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
      
      attr(,"class")
      [1] "RprobitB_prior"

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
      

# RprobitB_latent_class setting works

    Code
      RprobitB_latent_classes(list(C = 2))
    Output
      Latent classes
      C = 2 

---

    Code
      (out <- RprobitB_latent_classes(list(weight_update = TRUE, dp_update = TRUE)))
    Output
      Latent classes
      DP-based update: TRUE 
      Weight-based update: TRUE 
      Initial classes: 1 
      Maximum classes: 10 
      Updating buffer: 100 
      Minimum class weight: 0.01 
      Maximum class weight: 0.99 
      Mimumum class distance: 0.1 

---

    Code
      str(out)
    Output
      List of 8
       $ weight_update: logi TRUE
       $ dp_update    : logi TRUE
       $ C            : num 1
       $ Cmax         : num 10
       $ buffer       : num 100
       $ epsmin       : num 0.01
       $ epsmax       : num 0.99
       $ distmin      : num 0.1
       - attr(*, "class")= chr "RprobitB_latent_classes"

# building of RprobitB_normalization works

    Code
      RprobitB_normalization(J = J, P_f = P_f)
    Output
      Normalization
      Level: Utility differences with respect to alternative 5.
      Scale: Coefficient of the 1. error term variance in Sigma fixed to 1.

---

    Code
      RprobitB_normalization(J = J, P_f = P_f, scale = scale)
    Output
      Normalization
      Level: Utility differences with respect to alternative 5.
      Scale: Coefficient of alpha_1 fixed to 1.

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
      
      

