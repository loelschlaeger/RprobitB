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
                                            
           1   -1.09   -1.11    0.06    1.00
           2    1.52    1.51    0.09    1.01
           3   -0.61   -0.62    0.06    1.01
           4   -0.12   -0.08    0.05    1.03
           5    0.49    0.37    0.05    1.00
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00

---

    Code
      print(coef(model))
    Output
               Estimate   (sd)
      1     a     -1.11 (0.06)
      2   b_A      1.51 (0.09)
      3 ASC_A     -0.62 (0.06)
      4   c_A     -0.08 (0.05)
      5   c_B      0.37 (0.05)

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
                                            
           1    0.59    0.64    0.04    1.01
           2    1.18    1.17    0.07    1.00
      
       Sigma
                                            
         1,1    1.00    1.00    0.00      NA
      
       d
                                            
           1    0.00   -0.55    0.07    1.00
           2    1.00    0.54    0.04    1.00
           3    2.00    2.78    0.22    1.05

---

    Code
      print(coef(model))
    Output
                Estimate   (sd)
      1    age      0.64 (0.04)
      2 gender      1.17 (0.07)

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
                                            
           1    1.00    1.04    0.05    1.00
           2   -0.56   -0.57    0.05    1.00
           3   -1.63   -1.65    0.09    1.01
      
       Sigma
                                            
         1,1    1.00    1.00    0.00    1.00
         1,2    0.39    0.39    0.08    1.03
         2,2    0.84    0.95    0.13    1.00

---

    Code
      print(coef(model))
    Output
               Estimate   (sd)
      1 price      1.04 (0.05)
      2 ASC_A     -0.57 (0.05)
      3 ASC_B     -1.65 (0.09)

