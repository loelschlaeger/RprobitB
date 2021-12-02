# P

    Code
      print(data)
    Output
      Empirical data of 2929 choices.

---

    Code
      summary(data)
    Output
      Summary of empirical choice data
      
      235 decision makers 
      5 to 19 choice occasions each 
      2929 choices in total
      
      Alternatives
        frequency
      A      1474
      B      1455
      
      Linear coefficients
             name    re
      1     price FALSE
      2    time_A FALSE
      3    time_B FALSE
      4 comfort_A FALSE
      5 comfort_B FALSE
      6  change_A FALSE
      7  change_B FALSE

# MMNP

    Code
      print(data)
    Output
      Empirical data of 2929 choices.

---

    Code
      summary(data)
    Output
      Summary of empirical choice data
      
      235 decision makers 
      5 to 19 choice occasions each 
      2929 choices in total
      
      Alternatives
        frequency
      A      1474
      B      1455
      
      Linear coefficients
             name    re
      1 comfort_A FALSE
      2 comfort_B FALSE
      3  change_A FALSE
      4  change_B FALSE
      5     price  TRUE
      6    time_A  TRUE
      7    time_B  TRUE

# without choice variable

    Code
      print(data)
    Output
      Empirical data of 2929 choices.

---

    Code
      summary(data)
    Output
      Summary of empirical choice data
      
      235 decision makers 
      5 to 19 choice occasions each 
      2929 choices in total
      
      Alternatives
        frequency
      A        NA
      B        NA
      
      Linear coefficients
             name    re
      1 comfort_A FALSE
      2 comfort_B FALSE
      3  change_A FALSE
      4  change_B FALSE
      5     price  TRUE
      6    time_A  TRUE
      7    time_B  TRUE

# train and test data

    Code
      print(data)
    Output
      $train
      Empirical data of 2343 choices.
      
      $test
      Empirical data of 586 choices.
      

---

    Code
      print(data[[i]])
    Output
      Empirical data of 2343 choices.

---

    Code
      summary(data[[i]])
    Output
      Summary of empirical choice data
      
      188 decision makers 
      5 to 19 choice occasions each 
      2343 choices in total
      
      Alternatives
        frequency
      A      1179
      B      1164
      
      Linear coefficients
             name    re
      1 comfort_A FALSE
      2 comfort_B FALSE
      3  change_A FALSE
      4  change_B FALSE
      5     price  TRUE
      6    time_A  TRUE
      7    time_B  TRUE

---

    Code
      print(data[[i]])
    Output
      Empirical data of 586 choices.

---

    Code
      summary(data[[i]])
    Output
      Summary of empirical choice data
      
      47 decision makers 
      7 to 18 choice occasions each 
      586 choices in total
      
      Alternatives
        frequency
      A       295
      B       291
      
      Linear coefficients
             name    re
      1 comfort_A FALSE
      2 comfort_B FALSE
      3  change_A FALSE
      4  change_B FALSE
      5     price  TRUE
      6    time_A  TRUE
      7    time_B  TRUE

