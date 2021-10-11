# checks for latent classes work

    Code
      check_latent_classes(latent_classes = NULL)
    Output
      Latent classes:
      - Number: 1 
      - Update: FALSE 

---

    Code
      check_latent_classes(latent_classes = list(C = 2, update = TRUE, Cinit = 5,
        Cmax = 10, buffer = 100, epsmin = 0.01, epsmax = 0.99, distmin = 0.1))
    Output
      Latent classes:
      - Update: TRUE 
      - Initial number: 5 
      - Maximum number: 10 
      - Buffer: 100 
      - Minimum class weight: 0.01 
      - Maximum class weight: 0.99 
      - Mimumum class distance: 0.1 

