# overview effects can be generated

    Code
      out
    Output
              name    re
      1 income_bus FALSE
      2 income_car FALSE
      3   time_bus FALSE
      4   time_car FALSE
      5 time_train FALSE
      6       cost  TRUE
      7    ASC_bus  TRUE
      8    ASC_car  TRUE

# data preparation works

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
      1     ASC_A FALSE
      2 comfort_A FALSE
      3 comfort_B FALSE
      4  change_A FALSE
      5  change_B FALSE
      6     price  TRUE
      7    time_A  TRUE
      8    time_B  TRUE

# simulating choice data works

    Code
      print(data)
    Output
      Simulated data of 55 choices.

---

    Code
      summary(data)
    Output
      Summary of simulated choice data
      10 decision makers 
      1 to 10 choice occasions each 
      55 choices in total
      
      Alternatives
            frequency
      bus           7
      car          22
      train        26
      
      Linear coefficients
              name    re
      1 income_bus FALSE
      2 income_car FALSE
      3   time_bus FALSE
      4   time_car FALSE
      5 time_train FALSE
      6       cost  TRUE
      7    ASC_bus  TRUE
      8    ASC_car  TRUE

# splitting data set by N works

    Code
      train_test(x, test_proportion = 0.3, by = "N")
    Output
      $train
      Simulated data of 28 choices.
      
      $test
      Simulated data of 27 choices.
      

---

    Code
      train_test(x, test_proportion = 0, by = "N")
    Output
      $train
      Simulated data of 55 choices.
      
      $test
      Simulated data of 0 choices.
      

---

    Code
      train_test(x, test_proportion = 1, by = "N")
    Output
      $train
      Simulated data of 0 choices.
      
      $test
      Simulated data of 55 choices.
      

---

    Code
      train_test(x, test_proportion = 0.5, by = "N", random = TRUE, seed = 1)
    Output
      $train
      Simulated data of 32 choices.
      
      $test
      Simulated data of 23 choices.
      

---

    Code
      train_test(x, test_number = 1, by = "N")
    Output
      $train
      Simulated data of 45 choices.
      
      $test
      Simulated data of 10 choices.
      

---

    Code
      train_test(x, test_number = 2, by = "N")
    Output
      $train
      Simulated data of 36 choices.
      
      $test
      Simulated data of 19 choices.
      

---

    Code
      train_test(x, test_number = 1, by = "N", random = TRUE, seed = 1)
    Output
      $train
      Simulated data of 46 choices.
      
      $test
      Simulated data of 9 choices.
      

# splitting data set by T works

    Code
      train_test(x, test_proportion = 0.3, by = "T")
    Output
      $train
      Simulated data of 70 choices.
      
      $test
      Simulated data of 30 choices.
      

---

    Code
      train_test(x, test_proportion = 0.5, by = "T", random = TRUE, seed = 1)
    Output
      $train
      Simulated data of 50 choices.
      
      $test
      Simulated data of 50 choices.
      

---

    Code
      train_test(x, test_number = 1, by = "T")
    Output
      $train
      Simulated data of 90 choices.
      
      $test
      Simulated data of 10 choices.
      

---

    Code
      train_test(x, test_number = 2, by = "T")
    Output
      $train
      Simulated data of 80 choices.
      
      $test
      Simulated data of 20 choices.
      

---

    Code
      train_test(x, test_number = 1, by = "T", random = TRUE, seed = 1)
    Output
      $train
      Simulated data of 90 choices.
      
      $test
      Simulated data of 10 choices.
      

