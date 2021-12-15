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
      

