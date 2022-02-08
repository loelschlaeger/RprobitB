# latent_class setting works

    Code
      RprobitB_latent_classes(list(C = 2))
    Output
      Number of latent classes: 2 

---

    Code
      (out <- RprobitB_latent_classes(list(weight_update = TRUE, dp_update = TRUE)))
    Output
      DP-based update: TRUE 
      Weight-based update: TRUE 
      - Initial classes: 1 
      - Maximum classes: 10 
      - Updating buffer: 100 
      - Minimum class weight: 0.01 
      - Maximum class weight: 0.99 
      - Mimumum class distance: 0.1 

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

