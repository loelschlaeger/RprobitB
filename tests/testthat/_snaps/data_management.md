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
        number deciders choice occasions choices total
      1             235     5 to 19 each          2929
      
        alternative frequency
      1           A      1474
      2           B      1455
      
           effect as_value as_coef random
      1     ASC_A    FALSE    TRUE  FALSE
      2 comfort_A     TRUE    TRUE  FALSE
      3 comfort_B     TRUE    TRUE  FALSE
      4  change_A     TRUE    TRUE  FALSE
      5  change_B     TRUE    TRUE  FALSE
      6     price     TRUE   FALSE   TRUE
      7    time_A     TRUE    TRUE   TRUE
      8    time_B     TRUE    TRUE   TRUE

# data preparation with non-standard base alternative works

    Code
      print(data)
    Output
      Empirical data of 2929 choices.

---

    Code
      summary(data)
    Output
        number deciders choice occasions choices total
      1             235     5 to 19 each          2929
      
        alternative frequency
      1           A      1474
      2           B      1455
      
           effect as_value as_coef random
      1     ASC_A    FALSE    TRUE  FALSE
      2 comfort_A     TRUE    TRUE  FALSE
      3 comfort_B     TRUE    TRUE  FALSE
      4  change_A     TRUE    TRUE  FALSE
      5  change_B     TRUE    TRUE  FALSE
      6     price     TRUE   FALSE   TRUE
      7    time_A     TRUE    TRUE   TRUE
      8    time_B     TRUE    TRUE   TRUE

# simulating choice data works

    Code
      print(data)
    Output
      Simulated data of 55 choices.

---

    Code
      summary(data)
    Output
        number deciders choice occasions choices total
      1              10     1 to 10 each            55
      
        alternative frequency
      1         bus         7
      2         car        22
      3       train        26
      
            effect as_value as_coef random
      1 income_bus    FALSE    TRUE  FALSE
      2 income_car    FALSE    TRUE  FALSE
      3   time_bus     TRUE    TRUE  FALSE
      4   time_car     TRUE    TRUE  FALSE
      5 time_train     TRUE    TRUE  FALSE
      6       cost     TRUE   FALSE   TRUE
      7    ASC_bus    FALSE    TRUE   TRUE
      8    ASC_car    FALSE    TRUE   TRUE

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
      

# parameter reproducibility works

    Code
      unclass(x)
    Output
      $alpha
         1    2 
      -1.4 -0.8 
      
      $C
      [1] 1
      
      $s
      1 
      1 
      
      $b
           [,1]
      [1,] -1.8
      [2,]  2.4
      attr(,"names")
      [1] "1.1" "1.2"
      
      $Omega
                 [,1]
      [1,]  4.5285282
      [2,] -0.6271751
      [3,] -0.6271751
      [4,]  1.1964129
      attr(,"names")
      [1] "1.1,1" "1.1,2" "1.2,1" "1.2,2"
      
      $Sigma
                [,1]
      [1,] 0.6498953
      attr(,"names")
      [1] "1,1"
      
      $Sigma_full
                [,1]      [,2]
      [1,] 0.1387557 0.3542515
      [2,] 0.3542515 1.2196427
      attr(,"names")
      [1] "1,1" "1,2" "2,1" "2,2"
      
      $beta
                [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
      [1,] -6.532535 -1.036613 -3.801733 -3.534281 -4.832658 -1.271369 -1.759235
      [2,]  1.724397  2.282640  2.555224  2.895377  3.205471  2.395559  2.665422
                [,8]       [,9]     [,10]     [,11]     [,12]      [,13]     [,14]
      [1,] -3.181115 -0.3866967 -1.494049 -3.740912 -3.496234 -0.1568553 -2.704011
      [2,]  2.465750  3.3639748  2.233591  1.154519  3.955911  1.9412064  2.083866
               [,15]     [,16]     [,17]     [,18]      [,19]     [,20]     [,21]
      [1,] 0.3216218 0.8728506 0.9649806 -1.782186 -0.5311406 -2.400476 -1.312639
      [2,] 1.8156760 2.7110020 1.0972128  1.469664  2.3503752  4.016832  3.382216
                [,22]     [,23]     [,24]     [,25]     [,26]     [,27]     [,28]
      [1,] -0.1361772 -3.110847 -4.205499 -4.525478 -2.865499 -2.677856 -1.745984
      [2,]  1.3513501  2.630610  3.340636  4.489633  4.315405  1.497417  2.421460
               [,29]     [,30]      [,31]     [,32]     [,33]     [,34]    [,35]
      [1,] -5.375486 -4.182545 -0.7470578 -2.052793 -4.074214 -4.170130 1.387409
      [2,]  4.005156  3.083492  2.3995918  2.643242  1.868898  4.392644 2.235221
               [,36]    [,37]      [,38]      [,39]     [,40]     [,41]      [,42]
      [1,] -4.423656 1.416890 -0.1020068 -0.3329915 -4.421114 -3.650927 -0.2225389
      [2,]  2.759439 1.453401  1.1388691  1.1899917  1.755064  1.697074  2.2536976
               [,43]     [,44]     [,45]     [,46]     [,47]     [,48]      [,49]
      [1,] -2.488953 -3.961931 -4.182802 -0.796129 1.3287567 -2.005408 0.09528331
      [2,]  1.350945  1.890661  2.257919  1.017496 0.5852963  4.924600 1.87187626
               [,50]     [,51]     [,52]    [,53]      [,54]     [,55]     [,56]
      [1,] -3.642375 -1.826663 -1.123528 3.857714 -0.1410188 -2.955271 -2.541821
      [2,]  3.268827  2.008838  1.791428 3.386365  2.9215348  3.493036  1.440901
               [,57]     [,58]     [,59]     [,60]     [,61]     [,62]     [,63]
      [1,] 2.2074762 -2.426060 -3.815409 -5.042304 -3.022120 -1.949851 -3.060276
      [2,] 0.8664531  1.838946  3.310055  2.631851  1.104921  1.966885  3.608003
                [,64]     [,65]     [,66]     [,67]    [,68]     [,69]     [,70]
      [1,] -0.6670149 -1.466983 -2.228461 -1.835641 2.508761 0.2445987 -4.064580
      [2,]  2.1478058  1.577230  3.620320  2.575357 1.062023 4.0028478  2.732209
               [,71]     [,72]     [,73]      [,74]     [,75]     [,76]     [,77]
      [1,] -2.629739 -4.025323 0.9012989 -0.1494239 -2.577587 -1.929033 0.1706928
      [2,]  1.997894  1.764167 2.6514098  3.8118656  3.367814  1.889742 2.1659788
               [,78]    [,79]     [,80]       [,81]    [,82]     [,83]     [,84]
      [1,] -4.068910 1.381886 -4.902050 0.003859756 1.197471 -1.206992 1.5541180
      [2,]  2.463052 3.194024  2.929744 0.439146424 1.414203  2.113550 0.3812013
               [,85]      [,86]     [,87]     [,88]     [,89]    [,90]     [,91]
      [1,] -2.107731 -0.9348633 -5.022901 -2.113397 -3.889422 1.811163 -3.302240
      [2,]  1.438560  4.6283837  2.781354  4.067246  3.212445 1.625227  2.438274
                [,92]    [,93]     [,94]     [,95]     [,96]     [,97]      [,98]
      [1,] -0.7331702 1.636247 -7.981724 1.4932749 -2.016003 -5.197870 -0.9028138
      [2,]  1.1846351 1.930043  2.089898 0.9149531  2.474841  3.387747  4.2496273
               [,99]    [,100]
      [1,] 0.4014815 -1.975613
      [2,] 2.1812826  3.062731
      
      $z
        [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       [75] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      

# parameter printing works

    Code
      x
    Output
      RprobitB model parameter
      
      alpha : -1.4
      
      C : 1
      
      s : 1
      
      b : -1.8
      
      Omega : 2.849231
      
      Sigma : 0.4539793
      
      Sigma_full : 2 x 2 matrix of doubles 
      
                [,1]      [,2]
      [1,] 0.6751573 0.2612002
      [2,] 0.2612002 0.3012224
      
      
      beta : 1 x 100 matrix of doubles 
      
              [,1]    [,2]    [,3] ...  [,100]
      [1,] -1.3778 -0.7564 -2.0914 ... -0.2967
      
      
      z : integer vector of length 100 
      
      1 1 1 ... 1
      

