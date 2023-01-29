# RprobitB_covariates can be simulated and validated

    Code
      x
    Output
      [[1]]
      [[1]][[1]]
              cost
      apple     -6
      windows    2
      
      
      [[2]]
      [[2]][[1]]
              cost
      apple     -8
      windows   14
      
      [[2]][[2]]
              cost
      apple      3
      windows   -7
      
      
      [[3]]
      [[3]][[1]]
              cost
      apple      4
      windows    7
      
      [[3]][[2]]
              cost
      apple      5
      windows   -3
      
      [[3]][[3]]
              cost
      apple     14
      windows    4
      
      
      attr(,"class")
      [1] "RprobitB_covariates" "list"               

# custom covariates can be specified for the simulation

    Code
      x
    Output
      [[1]]
      [[1]][[1]]
        cost ASC_B ASC_C    time_A  time_B    time_C  price_B price_C
      A    1     0     0 -5.638084 0.00000  0.000000  0.00000       0
      B    2     1     0  0.000000 1.65279  0.000000 14.35753       0
      C    3     0     1  0.000000 0.00000 -7.520658  0.00000       1
      
      
      [[2]]
      [[2]][[1]]
        cost ASC_B ASC_C  time_A    time_B   time_C  price_B price_C
      A    1     0     0 2.96557  0.000000 0.000000 0.000000       0
      B    2     1     0 0.00000 -7.384215 0.000000 6.644922       0
      C    3     0     1 0.00000  0.000000 4.386861 0.000000       1
      
      [[2]][[2]]
        cost ASC_B ASC_C   time_A    time_B   time_C  price_B price_C
      A    1     0     0 5.182032  0.000000  0.00000 0.000000       0
      B    2     1     0 0.000000 -2.748495  0.00000 3.508589       0
      C    3     0     1 0.000000  0.000000 13.60603 0.000000       1
      
      
      [[3]]
      [[3]][[1]]
        cost ASC_B ASC_C    time_A   time_B   time_C    price_B price_C
      A    1     0     0 -5.591165   0.0000  0.00000  0.0000000       0
      B    2     1     0  0.000000 -19.9323  0.00000 -0.4044025       0
      C    3     0     1  0.000000   0.0000 10.12438  0.0000000       1
      
      [[3]][[2]]
        cost ASC_B ASC_C     time_A   time_B   time_C  price_B price_C
      A    1     0     0 -0.1457124 0.000000 0.000000 0.000000       0
      B    2     1     0  0.0000000 8.494526 0.000000 5.345112       0
      C    3     0     1  0.0000000 0.000000 7.390991 0.000000       1
      
      [[3]][[3]]
        cost ASC_B ASC_C   time_A   time_B    time_C   price_B price_C
      A    1     0     0 8.270796 0.000000 0.0000000   0.00000       0
      B    2     1     0 0.000000 7.039227 0.0000000 -17.90417       0
      C    3     0     1 0.000000 0.000000 0.6710849   0.00000       1
      
      
      attr(,"class")
      [1] "RprobitB_covariates" "list"               

