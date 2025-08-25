# overview effects can be generated

    Code
      out
    Output
            effect as_value as_coef random
      1 income_bus    FALSE    TRUE  FALSE
      2 income_car    FALSE    TRUE  FALSE
      3   time_bus     TRUE    TRUE  FALSE
      4   time_car     TRUE    TRUE  FALSE
      5 time_train     TRUE    TRUE  FALSE
      6       cost     TRUE   FALSE   TRUE
      7    ASC_bus    FALSE    TRUE   TRUE
      8    ASC_car    FALSE    TRUE   TRUE

# data preparation works

    Code
      print(data)
    Output
      Empirical data of 2929 choices.

---

    Code
      summary(data)
    Output
                       count
      deciders           235
      choice occasions  5-19
      total choices     2929
      alternatives         2
      - 'A'             1474
      - 'B'             1455

# data preparation with ordered choices works

    Code
      print(data)
    Output
      Empirical data of 55 (ordered) choices.

---

    Code
      summary(data)
    Output
                       count
      deciders            10
      choice occasions  1-10
      total choices       55
      alternatives         5
      - 'very bad'        10
      - 'bad'              2
      - 'indifferent'     40
      - 'good'             1
      - 'very good'        2

# data preparation with ranked choices works

    Code
      print(data)
    Output
      Empirical data of 55 (ranked) choices.

---

    Code
      summary(data)
    Output
                       count
      deciders            10
      choice occasions  1-10
      total choices       55
      alternatives         6
      - 'A,B,C'            2
      - 'A,C,B'            1
      - 'B,A,C'           14
      - 'B,C,A'           24
      - 'C,A,B'            4
      - 'C,B,A'           10

# data preparation with non-standard base alternative works

    Code
      print(data)
    Output
      Empirical data of 2929 choices.

---

    Code
      summary(data)
    Output
                       count
      deciders           235
      choice occasions  5-19
      total choices     2929
      alternatives         2
      - 'A'             1474
      - 'B'             1455

# simulating choice data works

    Code
      print(data)
    Output
      Simulated data of 55 choices.

---

    Code
      summary(data)
    Output
                       count
      deciders            10
      choice occasions  1-10
      total choices       55
      alternatives         3
      - 'bus'             20
      - 'car'             15
      - 'train'           20

# simulating ordered choices works

    Code
      print(data)
    Output
      Simulated data of 55 (ordered) choices.

---

    Code
      summary(data)
    Output
                       count
      deciders            10
      choice occasions  1-10
      total choices       55
      alternatives         5
      - 'very bad'        10
      - 'bad'              2
      - 'indifferent'     40
      - 'good'             1
      - 'very good'        2

# simulating ranked choices works

    Code
      print(data)
    Output
      Simulated data of 55 (ranked) choices.

---

    Code
      summary(data)
    Output
                       count
      deciders            10
      choice occasions  1-10
      total choices       55
      alternatives         6
      - 'A,B,C'            2
      - 'A,C,B'            1
      - 'B,A,C'           14
      - 'B,C,A'           24
      - 'C,A,B'            4
      - 'C,B,A'           10

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
      train_test(x, test_proportion = 0.5, by = "N", random = TRUE)
    Output
      $train
      Simulated data of 23 choices.
      
      $test
      Simulated data of 32 choices.
      

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
      train_test(x, test_number = 1, by = "N", random = TRUE)
    Output
      $train
      Simulated data of 48 choices.
      
      $test
      Simulated data of 7 choices.
      

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
      train_test(x, test_proportion = 0.5, by = "T", random = TRUE)
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
      train_test(x, test_number = 1, by = "T", random = TRUE)
    Output
      $train
      Simulated data of 90 choices.
      
      $test
      Simulated data of 10 choices.
      

