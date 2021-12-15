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
      Empirical data of 2055 choices.
      
      $test
      Empirical data of 874 choices.
      

---

    Code
      print(data[[i]])
    Output
      Empirical data of 2055 choices.

---

    Code
      summary(data[[i]])
    Output
      Summary of empirical choice data
      
      165 decision makers 
      5 to 19 choice occasions each 
      2055 choices in total
      
      Alternatives
        frequency
      A      1048
      B      1007
      
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
      Empirical data of 874 choices.

---

    Code
      summary(data[[i]])
    Output
      Summary of empirical choice data
      
      70 decision makers 
      5 to 18 choice occasions each 
      874 choices in total
      
      Alternatives
        frequency
      A       426
      B       448
      
      Linear coefficients
             name    re
      1 comfort_A FALSE
      2 comfort_B FALSE
      3  change_A FALSE
      4  change_B FALSE
      5     price  TRUE
      6    time_A  TRUE
      7    time_B  TRUE

